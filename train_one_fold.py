# Train one fold of k-fold cross-validation
import argparse
import gc
import numpy as np
import tensorflow as tf
import pandas as pd
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

from preprocess import calc_bias
from model_zoo import model_dict, metrics
from keras.layers import (
    Dropout,
    Dense,
    GlobalAveragePooling2D,
    BatchNormalization,
)
from keras.optimizers import Adam
from keras.models import Sequential
from keras.losses import MeanSquaredError

def create_model(base_model, num_classes, activ):
    model = Sequential(
        [
            base_model,
            Dropout(0.8),
            GlobalAveragePooling2D(),
            Dropout(0.6),
            BatchNormalization(),
            Dense(num_classes,
                  dtype="float32",
                  activation=activ,
                  ),
        ]
    )

    for layer in model.layers:
        layer.trainable = not isinstance(layer, BatchNormalization)

    model.summary()

    opt = Adam(learning_rate=1e-5)
    model.compile(optimizer=opt, loss=MeanSquaredError(), metrics=[metrics(num_classes), 'accuracy'])

    print('Compiled model')
    return model


def load_model_base(img_shape, model_selector):
    model_base = model_dict[model_selector][0](
        weights="imagenet", include_top=False, input_shape=img_shape
    )
    model_base.trainable = False

    return model_base

def train_fold(
    fold,
    img_shape,
    n_classes,
    active,
    train,
    val,
    callbacks,
    weights,
    model_selector
):
    tf.keras.backend.clear_session()

    # Load model base and create model
    model_base = load_model_base(img_shape, model_selector)
    model = create_model(model_base, n_classes, active)
    print(f"Model for fold {fold} created.")

    # Train model
    model.fit(
        train,
        epochs=180,
        validation_data=val,
        callbacks=callbacks,
        class_weight=weights,
    )

    # Evaluate model
    val_score = model.evaluate(val)
    print(f"Fold {fold} validation score: {val_score}")

    # Cleanup
    del model, model_base
    del train, val

    gc.collect()

    # Reset TF's memory tracker
    for i, gpu in enumerate(gpus):
        try:
            tf.config.experimental.reset_memory_stats(f'GPU:{i}')
            print(f"Reset memory stats for GPU:{i}")
        except Exception as e:
            print(f"Could not reset memory stats for GPU:{i}: {e}")

    return val_score


if __name__ == "__main__":
    parser = argparse.ArgumentParser()

    parser.add_argument('--fold', type=int, required=True, help='Fold number to train')
    parser.add_argument('--img_shape', type=int, required=True, help='Image shape (height, width, channels)')
    parser.add_argument('--n_classes', type=int, required=True, help='Number of classes')
    parser.add_argument('--active', type=bool, default=False, help='Ground cover - softmax activation')
    parser.add_argument('--tmp_dir', type=str, required=True, help='Path to training and validation data')
    parser.add_argument('--batch_size', type=int, default=16, help='Batch size for training')
    parser.add_argument('--csv', type=str, required=True, help='CSV file for dataset creation')
    parser.add_argument('--model', type=str, required=True, help='Model found in model zoo')

    args = parser.parse_args()

    img_shape = (args.img_shape, args.img_shape, 3)
    model_selector = args.model.upper()
    BATCH_SIZE = args.batch_size
    AUTOTUNE = tf.data.AUTOTUNE
    active = "softmax" if args.active else None
    start = 0 if args.active else 1
    end = 10 if args.active else None
    df = pd.read_csv(args.csv)
    weights = {n: w for n, w in enumerate(calc_bias(df, start=start, end=end))}

    # Load training and validation data for the specified fold
    train_x = np.load(f"{args.tmp_dir}/train_x_{args.fold}.npy", allow_pickle=True)
    train_y = np.load(f"{args.tmp_dir}/train_y_{args.fold}.npy", allow_pickle=True)
    val_x = np.load(f"{args.tmp_dir}/val_x_{args.fold}.npy", allow_pickle=True)
    val_y = np.load(f"{args.tmp_dir}/val_y_{args.fold}.npy", allow_pickle=True)

    # Build datasets
    train_ds = (
        tf.data.Dataset.from_tensor_slices((train_x, train_y))
        .batch(BATCH_SIZE)
        .prefetch(AUTOTUNE)        
    )
    val_ds = (
        tf.data.Dataset.from_tensor_slices((val_x, val_y))
        .batch(BATCH_SIZE)
        .prefetch(AUTOTUNE)        
    )

    # Define callbacks
    early_stop = tf.keras.callbacks.EarlyStopping(
        monitor='val_mean_squared_error',
        min_delta=2e-5,
        patience=10,
        restore_best_weights=True
    )
    lr_scheduler = tf.keras.callbacks.ReduceLROnPlateau(
        monitor='val_loss',
        factor=0.1,
        patience=4,
        verbose=1,
        mode='min',
        min_lr=1e-10
    )

    # run training for the fold
    val_score = train_fold(
        fold=args.fold,
        img_shape=img_shape,
        n_classes=args.n_classes,
        active=active,
        train=train_ds,
        val=val_ds,
        callbacks=[early_stop, lr_scheduler],
        weights=weights,
        model_selector=model_selector
    )

    with open(f"{args.tmp_dir}/val_score_fold_{args.fold}.npy", "wb") as f:
        np.save(f, val_score)
    # val_score.to_csv(f"{args.tmp_dir}/val_score_fold_{args.fold}.csv", index=False)
    print(f"Validation score for fold {args.fold} saved.")