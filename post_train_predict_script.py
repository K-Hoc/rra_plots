import argparse
import pickle
import matplotlib.pyplot as plt
from pathlib import Path
import numpy as np
import pandas as pd
import csv
from model_zoo import model_dict

parser = argparse.ArgumentParser()
parser.add_argument(
    "-m", "--model",
    help="Name of the model that was used.",
    required=True
)
parser.add_argument(
    "-o", "--output",
    help="Path to output directory of the model e.g. ~/xception/0108-112304/",
    required=True
)
#parser.add_argument(
#    "-c", "--csv",
#    help="original CSV file, as comparison for predicted values.",
#    required=True
#)
#parser.add_argument(
#    "-p", "--pred",
#    help="CSV file containing predicted values.",
#    required=True
#)
parser.add_argument(
    "-gc", "--groundcover",
    help="Using groundcover -> True, Default is False.",
    type=bool,
    default=False
)
params = parser.parse_args()

used_model = params.model.upper()
output_dir = params.output
#OG_csv = params.csv
#pred_csv = params.pred
gc = params.groundcover

# Function for plotting training history
def plot_train(history_path):
    histfile = Path(history_path)

    with open(histfile, 'rb') as src:
        history = pickle.load(src)

    plt.plot(history['mean_squared_error'])
    plt.plot(history['val_mean_squared_error'])

    plt.title('Mean squared error during training')
    plt.ylabel('MSE')
    plt.xlabel('epoch')
    plt.savefig(histfile.parent.joinpath('training_metrics.png'))

# Accecibility of history file outside Python
def access_hist(output_path):
    path = Path(output_path)
    hist_file = Path(f'{path}/history')

    with open(hist_file, 'rb') as src:
        history = pickle.load(src)
    print(history)

    # Flatten the dictionary
    flat_history = {}
    for key, value in history.items():
        flat_history[key] = value

    # Determine the maximum length of lists to set the number of columns in CSV
    max_length = max(len(lst) for lst in flat_history.values())

    # Fill shorter lists with None values to make them all the same length
    for key, value in flat_history.items():
        flat_history[key] = value + [None] * (max_length - len(value))

    # Write to CSV
    with open(Path(f'{path}/history.csv'), 'w', newline='') as f: # Enter path here as well
        w = csv.DictWriter(f, fieldnames=flat_history.keys())
        w.writeheader()
        for i in range(max_length):
            row = {key: flat_history[key][i] for key in flat_history}
            w.writerow(row)

# Accecibilty of val_file
def accec_val_file(output_path, val_file_path, gc=False):
    path = Path(output_path)

    with open(val_file_path, 'rb') as src:
        npy = np.lib.format.read_array(src)

    df = pd.DataFrame(data=npy[0:, 0:])
    df = df.T

    if (gc == True):
        # use this list of columns for groundcover
        # gc_Mature_Trees,gc_rejuvenation,gc_shrub_layer,gc_mosses,gc_ferns,gc_herb_layer,
        # gc_grasses,gc_soil/foliage,gc_rock,gc_deadwood/stumps
        strCols = "gc_Mature_Trees","gc_rejuvenation","gc_shrub_layer","gc_mosses","gc_ferns","gc_herb_layer","gc_grasses","gc_soil/foliage","gc_rock","gc_deadwood/stumps"
        strGTCols = "gc_Mature_Trees_gt","gc_rejuvenation_gt","gc_shrub_layer_gt","gc_mosses_gt","gc_ferns_gt","gc_herb_layer_gt","gc_grasses_gt","gc_soil/foliage_gt","gc_rock_gt","gc_deadwood/stumps_gt"
    else:
        # use this list of columns for complexity of surroundings
        # grade_treespecies,grade_shrubs,grade_herbs,grade_grass,grade_moss,grade_deadwood,grade_layers,grade_mixing
        strCols = "grade_stand_density","grade_treespecies","grade_shrubs","grade_herbs","grade_grass","grade_moss","grade_deadwood","grade_layers","grade_mixing"
        strGTCols = "grade_stand_density_gt","grade_treespecies_gt","grade_shrubs_gt","grade_herbs_gt","grade_grass_gt","grade_moss_gt","grade_deadwood_gt","grade_layers_gt","grade_mixing_gt"

    df.columns = strCols

    npy_file = Path(f'{val_file_path}')
    with open(npy_file, 'rb') as src:
        npy_gt = np.lib.format.read_array(src)

    dfGT = pd.DataFrame(data=npy_gt[0:, 0:])
    dfGT = dfGT.T
    dfGT.columns = strGTCols

    df = pd.concat([df, dfGT], axis=1)
    df.to_csv(path.joinpath('test_results.csv'), index=False)


### Main part of the script
history_path = f'{output_dir}/history'
parts = output_dir.split('/')
model_name = parts[2]
print(model_name)

# visualize training
plot_train(history_path)

# accecibility of history file
access_hist(output_dir)

# accecibility of val_file
accec_val_file(
    output_path=output_dir,
    # val_file_path=f'{output_dir}val_{used_model.lower()}.npy',
    val_file_path=f'{output_dir}val_{model_name}.npy',
    gc=gc
)