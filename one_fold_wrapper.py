# wrapper for train_on_fold.py to do k-fold cross validation training and validation
import argparse
import os
import subprocess
import numpy as np
import pandas as pd
import pickle
import tensorflow as tf
import random

random.seed(161)
np.random.seed(161)
# GPU memory allocation handeling
# Dynamic memory growth - starts small and grows as needed BUT never frees space
gpus = tf.config.list_physical_devices('GPU')
for gpu in gpus:
    tf.config.experimental.set_memory_growth(gpu, True)
##
tf.keras.utils.set_random_seed(161)

from keras.callbacks import EarlyStopping, ReduceLROnPlateau
from tqdm import tqdm
from datetime import datetime
from functools import partial
from pathlib import Path
from sklearn.model_selection import KFold
from preprocess import augment_keras, calc_bias, create_dataset, get_dataset_partitions, resize_and_rescale
#from train_val_kfold import create_model, load_model_base
from model_zoo import model_dict
from train_one_fold import create_model, load_model_base

parser = argparse.ArgumentParser()

parser.add_argument('-m', '--model', type=str, required=True, help='Model to choose from model zoo')
parser.add_argument('-c', '--csv', type=str, required=True, help='CSV file for dataset creation')
parser.add_argument('-o', '--output', type=str, required=True, help='Output directory for results')
parser.add_argument('-g', '--ground', type=bool, default=False, help='True if groundcover classification')

args = parser.parse_args()

csv_file = args.csv
model_selector = args.model.upper()
o_gc = args.ground
output_dir = args.output

print("gc: ", o_gc)

df = pd.read_csv(csv_file)
start = 0 if o_gc else 1
end = 10 if o_gc else None
print(f"Using columns {start} to {end} for classification.")

# Define triplets for out of sample testing
oos_triplets = ["03_spruce", "26_beech", "47_oak", "64_pine"]

# ---------------------------------------------------------------
# Data preparation for k-fold
# ---------------------------------------------------------------
with tqdm(total=7, leave=True, position=0) as p_bar:
    # 1 - Weights
    p_bar.set_description('Calculating weights'.ljust(35, '.'))
    weights = {n: w for n, w in enumerate(calc_bias(df, start=start, end=end))}
    num_classes = len(weights)

    # 2 - Global variables    
    p_bar.update(1)
    p_bar.set_description('Setting globals'.ljust(35, '.'))


    BATCH_SIZE = 8 #16
    AUTOTUNE = tf.data.AUTOTUNE

    # set depending on model base
    RESCALE_VAL = model_dict[model_selector][1]
    image_shape = (RESCALE_VAL, RESCALE_VAL, 3)

    # 3 - random number
    p_bar.update(1)
    p_bar.set_description('Generating random numbers'.ljust(35, '.'))
    rng = tf.random.Generator.from_seed(42, alg='philox')

    # 4 - Create datasets
    p_bar.update(1)
    p_bar.set_description('Create datasets'.ljust(35, '.'))
    target_str = "gc" if o_gc else "grade"
    mask = df['image_path'].str.contains('|'.join(oos_triplets))

    df_test = df[mask]
    df_rest = df[~mask]

    complete_set = create_dataset(df_rest, target_str, num_calls=AUTOTUNE)

    # 5 - Split datasets
    p_bar.update(1)
    p_bar.set_description('Split datasets'.ljust(35, '.'))

    train = tf.data.Dataset
    test = tf.data.Dataset
    val = tf.data.Dataset
    train, val, test = get_dataset_partitions(
        ds=complete_set,
        ds_size=len(complete_set),
        train_split=1.0,
        val_split=0.0,
        test_split=0.0,
        shuffle_size=300,
        seed=4
    )
    test = create_dataset(df_test, target_str, num_calls=AUTOTUNE)

    p_bar.update(1)

    # Steps per epoch
    TST_STEPS_PER_EPOCH = len(train) // BATCH_SIZE
    VAL_STEPS_PER_EPOCH = len(val) // BATCH_SIZE

    seed = rng.make_seeds(2)[0]

    train = (
        train.map(
            partial(augment_keras, seed=seed, size=RESCALE_VAL),
            num_parallel_calls=AUTOTUNE
        )
        .batch(BATCH_SIZE)
        .prefetch(AUTOTUNE)
    )
    p_bar.update(1)

    test = (
        test.map(partial(resize_and_rescale, size=RESCALE_VAL), num_parallel_calls=AUTOTUNE)
        .batch(BATCH_SIZE)
        .prefetch(AUTOTUNE)
    )
    p_bar.update(1)

# Create callbacks
active = "softmax" if o_gc else None
early_stop = EarlyStopping(
    monitor="val_mean_squared_error",
    min_delta=2e-5,
    patience=10,
    restore_best_weights=True
)
lr_scheduler = ReduceLROnPlateau(
    monitor="val_loss",
    factor=0.1,
    patience=4,
    verbose=1,
    mode="min",
    min_lr=1e-10
)

# ----------------------------------------
# k-fold cross validation
# ----------------------------------------
kf = KFold(n_splits=10, shuffle=True, random_state=42)

# Convert dataset into NumPy list, is loaded into CPU RAM
indis = np.arange(len(train))
train_list = list(train.as_numpy_iterator())
validation_scores = []

# Train and validate on each fold
for fold, (train_idx, val_idx) in enumerate(kf.split(indis)):
    print(f"\n===============================")
    print(f"Starting Fold {fold}...")
    print("===============================")

    # Create train and validation subset
    train_sub = [train_list[i] for i in train_idx]
    val_sub = [train_list[i] for i in val_idx]

    if isinstance(train_sub[0], tuple):
        train_x, train_y = zip(*train_sub)
        val_x, val_y = zip(*val_sub)

        train_x = np.concatenate(train_x, axis=0)
        train_y = np.concatenate(train_y, axis=0)
        val_x = np.concatenate(val_x, axis=0)
        val_y = np.concatenate(val_y, axis=0)

    # Save temporary files
    if not os.path.exists("tmp"):
        os.mkdir("tmp")
    np.save(f"tmp/train_x_{fold}.npy", train_x)
    np.save(f"tmp/train_y_{fold}.npy", train_y)
    np.save(f"tmp/val_x_{fold}.npy", val_x)
    np.save(f"tmp/val_y_{fold}.npy", val_y)

    try:
        if o_gc == True:
            # If groundcover
            # Run subprocess
            subprocess.run(
                [
                    "python",
                    "train_one_fold.py",
                    "--fold", str(fold),
                    "--img_shape", str(RESCALE_VAL),
                    "--n_classes", str(num_classes),
                    "--active", str(o_gc),
                    "--tmp_dir", "tmp",
                    "--batch_size", str(BATCH_SIZE),
                    "--csv", str(csv_file),
                    "--model", str(model_selector)
                ],
                check=True
            )
        else:
            # If context image
            # Run subprocess
            subprocess.run(
                [
                    "python",
                    "train_one_fold.py",
                    "--fold", str(fold),
                    "--img_shape", str(RESCALE_VAL),
                    "--n_classes", str(num_classes),
                    "--tmp_dir", "tmp",
                    "--batch_size", str(BATCH_SIZE),
                    "--csv", str(csv_file),
                    "--model", str(model_selector)
                ],
                check=True
            )
        print("Fold subprocess finished successful")
    except subprocess.CalledProcessError as e:
        print("Command failed!\n")
        print("Return code:", e.returncode)

    # Load fold results
    # val_score = pd.read_csv("tmp/val_score_fold_{fold}.csv")
    with open(f"tmp/val_score_fold_{fold}.npy", "rb") as f:
        #val_score = pd.read_csv(f)
        val_score = np.load(f, allow_pickle=True)
        validation_scores.append(val_score)

    # Delete tmp data
    os.remove(f"tmp/train_x_{fold}.npy")
    os.remove(f"tmp/train_y_{fold}.npy")
    os.remove(f"tmp/val_x_{fold}.npy")
    os.remove(f"tmp/val_y_{fold}.npy")

# # Delete tmp directory and files
# os.shutil.rmtree("tmp")

print("\n Cross validation done!")

# Validation score: average of the scores of k-fold
val_score = np.average(validation_scores)

# ---------------------------------------------------
# Create final model
# ---------------------------------------------------
# Recreate full training dataset
train_list = list(train.as_numpy_iterator())
if isinstance(train_list[0], tuple):
    train_x, train_y = zip(*train_list)
    train_x = np.concatenate(train_x, axis=0)
    train_y = np.concatenate(train_y, axis=0)
    full_dataset = tf.data.Dataset.from_tensor_slices((train_x, train_y))
else:
    full_dataset = tf.data.Dataset.from_tensor_slices(train_list)

# Shuffle and split into training and validation sets (90/10 split)
total_size = len(full_dataset)
val_size = int(0.1 * total_size)

full_dataset = full_dataset.shuffle(total_size, seed=42)
val_data = full_dataset.take(val_size).batch(BATCH_SIZE).prefetch(AUTOTUNE)
training = full_dataset.skip(val_size).batch(BATCH_SIZE).prefetch(AUTOTUNE)

# Load model base
model_base = load_model_base(image_shape, model_selector)

# Train final model on non-test data
model = create_model(model_base, num_classes, active)
hist = model.fit(
    training,
    epochs=180,
    validation_data=val_data,
    callbacks=[early_stop, lr_scheduler],
    class_weight=weights
).history
test_score = model.evaluate(test)
print("Model trained.")

# Save model outputs to directory
outpath = Path(output_dir).joinpath(
    model_base.name, datetime.now().strftime("%Y%m%d-%H%M%S")
)
outpath.mkdir(parents=True)

with open(outpath.joinpath('history'), "wb") as f:
    pickle.dump(hist, f)

model.save(outpath.joinpath("model"), True)
print("Model saved.")

# Gives predictions per class (note the transponse -> for class aggregation)
val_predictions = model.predict(val_data).T
val_ground_truth = np.array([y for x, y in val_data.unbatch().as_numpy_iterator()]).T
print("model validation done")

# Test files
test_predictions = model.predict(test).T
test_ground_truth = np.array([y for x, y in val_data.unbatch().as_numpy_iterator()]).T
#

# Write test resutls into a file
test_loss, *catch_rest = model.evaluate(test)
test_res = f"{model.metrics_names} \n{model.evaluate(test)} \nValidation_score {val_score}"
print(f"Test loss: {test_loss:.5f}")
testfile = outpath.joinpath("test_res.txt")
with open(testfile, "w") as f:
    f.write(test_res)

savefile = outpath.joinpath(f"val_{model_base.name}.npy")
savefile_tst = outpath.joinpath(f"tst_{model_base.name}.npy")
savefile_tst2 = outpath.joinpath(f"tst_{model_base.name}_gt.npy")

with open(savefile, "wb") as f:
    np.save(f, val_ground_truth)
    np.save(f, val_predictions)

with open(savefile_tst, "wb") as f:
    np.save(f, test_predictions)

with open(savefile_tst2, "wb") as f:
    np.save(f, test_ground_truth)

### Save scores from runs
validation_scores = np.array(validation_scores)
scores_dict = {
    "Fold": list(range(1, len(validation_scores) + 1)),
    f"{model.metrics_names[0]}": validation_scores[:, 0],
    f"{model.metrics_names[1]}": validation_scores[:, 1],
    f"{model.metrics_names[2]}": validation_scores[:, 2],
    f"{model.metrics_names[3]}": validation_scores[:, 3],
    f"{model.metrics_names[4]}": validation_scores[:, 4]
}

# Convert to dataframe
df_scores = pd.DataFrame(scores_dict)
df_scores.to_csv(outpath.joinpath("cross_val_res.csv"), index=False)

print("I'm OK, validation saved -->> DONE")