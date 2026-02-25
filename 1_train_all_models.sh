#!/bin/bash

conda activate rra_plot_3.11
#cd ~/NAS/Paper_1/NeuralNetworks/rra_plots_KH
cd ~/NAS/Paper_1/2_work

# Set GPUs to use
export CUDA_VISIBLE_DEVICES=3

# Function to get the latest sub-directory by modification date
get_latest_subdir() {
    parent_dir="$1"
    latest=$(ls -td "$parent_dir"/*/ 2>/dev/null | head -n 1)
    echo "$latest"
}
# Save latest model directories to variables for later use
# Groundcover
GC_DIR="output/groundcover"

# Complexity
CO_DIR="output/complexity"

echo "Running GROUNDCOVER training..."

# Define scripts with custom arguments
# Groundcover
python one_fold_wrapper.py -m B0 -c ../1_dataRaw/paths_and_labels.csv -o "$GC_DIR" -g True
python one_fold_wrapper.py -m B2 -c ../1_dataRaw/paths_and_labels.csv -o "$GC_DIR" -g True
python one_fold_wrapper.py -m B4 -c ../1_dataRaw/paths_and_labels.csv -o "$GC_DIR" -g True
python one_fold_wrapper.py -m B5 -c ../1_dataRaw/paths_and_labels.csv -o "$GC_DIR" -g True

python one_fold_wrapper.py -m XCEPTION -c ../1_dataRaw/paths_and_labels.csv -o "$GC_DIR" -g True

python one_fold_wrapper.py -m CNT -c ../1_dataRaw/paths_and_labels.csv -o "$GC_DIR" -g True
python one_fold_wrapper.py -m CNS -c ../1_dataRaw/paths_and_labels.csv -o "$GC_DIR" -g True
python one_fold_wrapper.py -m CNB -c ../1_dataRaw/paths_and_labels.csv -o "$GC_DIR" -g True
python one_fold_wrapper.py -m CNL -c ../1_dataRaw/paths_and_labels.csv -o "$GC_DIR" -g True
python one_fold_wrapper.py -m CNXL -c ../1_dataRaw/paths_and_labels.csv -o "$GC_DIR" -g True

# Complexity
python one_fold_wrapper.py -m B0 -c ../1_dataRaw/compl_cleaned.csv -o "$CO_DIR"
python one_fold_wrapper.py -m B2 -c ../1_dataRaw/compl_cleaned.csv -o "$CO_DIR"
python one_fold_wrapper.py -m B4 -c ../1_dataRaw/compl_cleaned.csv -o "$CO_DIR"
python one_fold_wrapper.py -m B5 -c ../1_dataRaw/compl_cleaned.csv -o "$CO_DIR"

python one_fold_wrapper.py -m XCEPTION -c ../1_dataRaw/compl_cleaned.csv -o "$CO_DIR"

python one_fold_wrapper.py -m CNT -c ../1_dataRaw/compl_cleaned.csv -o "$CO_DIR"
python one_fold_wrapper.py -m CNS -c ../1_dataRaw/compl_cleaned.csv -o "$CO_DIR"
python one_fold_wrapper.py -m CNB -c ../1_dataRaw/compl_cleaned.csv -o "$CO_DIR"
python one_fold_wrapper.py -m CNL -c ../1_dataRaw/compl_cleaned.csv -o "$CO_DIR"
python one_fold_wrapper.py -m CNXL -c ../1_dataRaw/compl_cleaned.csv -o "$CO_DIR"


echo "FINISHED complexity training"
echo "ALL DONE"