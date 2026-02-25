#!/bin/bash
# Function to get the latest sub-directory by modification date
get_latest_subdir() {
    parent_dir="$1"
    latest=$(ls -td "$parent_dir"/*/ 2>/dev/null | head -n 1)
    echo "$latest"
}
# Set up environment and navigate to project directory
conda activate rra_plots_3.11
cd ~/NAS/Paper_1/2_work

# Set variables for the latest model directories
GC_B0_DIR=$(get_latest_subdir "output/groundcover/efficientnetb0")
GC_B2_DIR=$(get_latest_subdir "output/groundcover/efficientnetb2")
GC_B4_DIR=$(get_latest_subdir "output/groundcover/efficientnetb4")
GC_B5_DIR=$(get_latest_subdir "output/groundcover/efficientnetb5")
GC_CNT_DIR=$(get_latest_subdir "output/groundcover/convnext_tiny")
GC_CNS_DIR=$(get_latest_subdir "output/groundcover/convnext_small")
GC_CNB_DIR=$(get_latest_subdir "output/groundcover/convnext_base")
GC_CNL_DIR=$(get_latest_subdir "output/groundcover/convnext_large")
GC_CNXL_DIR=$(get_latest_subdir "output/groundcover/convnext_xlarge")
GC_XCEPTION_DIR=$(get_latest_subdir "output/groundcover/xception")

CO_B0_DIR=$(get_latest_subdir "output/complexity/efficientnetb0")
CO_B2_DIR=$(get_latest_subdir "output/complexity/efficientnetb2")
CO_B4_DIR=$(get_latest_subdir "output/complexity/efficientnetb4")
CO_B5_DIR=$(get_latest_subdir "output/complexity/efficientnetb5")
CO_CNT_DIR=$(get_latest_subdir "output/complexity/convnext_tiny")
CO_CNS_DIR=$(get_latest_subdir "output/complexity/convnext_small")
CO_CNB_DIR=$(get_latest_subdir "output/complexity/convnext_base")
CO_CNL_DIR=$(get_latest_subdir "output/complexity/convnext_large")
CO_CNXL_DIR=$(get_latest_subdir "output/complexity/convnext_xlarge")
CO_XCEPTION_DIR=$(get_latest_subdir "output/complexity/xception")

echo "Running for GROUNDCOVER..."
python post_train_predict_script.py -m B0 -o "$GC_B0_DIR" -gc TRUE
python post_train_predict_script.py -m B2 -o "$GC_B2_DIR" -gc TRUE
python post_train_predict_script.py -m B4 -o "$GC_B4_DIR" -gc TRUE
python post_train_predict_script.py -m B5 -o "$GC_B5_DIR" -gc TRUE

python post_train_predict_script.py -m CNT -o "$GC_CNT_DIR" -gc TRUE
python post_train_predict_script.py -m CNS -o "$GC_CNS_DIR" -gc TRUE
python post_train_predict_script.py -m CNB -o "$GC_CNB_DIR" -gc TRUE
python post_train_predict_script.py -m CNL -o "$GC_CNL_DIR" -gc TRUE
python post_train_predict_script.py -m CNXL -o "$GC_CNXL_DIR" -gc TRUE

python post_train_predict_script.py -m XCEPTION -o "$GC_XCEPTION_DIR" -gc TRUE
echo "Finished GROUNDCOVER post-processing."

echo "Running for COMPLEXITY..."
python post_train_predict_script.py -m B0 -o "$CO_B0_DIR"
python post_train_predict_script.py -m B2 -o "$CO_B2_DIR"
python post_train_predict_script.py -m B4 -o "$CO_B4_DIR"
python post_train_predict_script.py -m B5 -o "$CO_B5_DIR"

python post_train_predict_script.py -m CNT -o "$CO_CNT_DIR"
python post_train_predict_script.py -m CNS -o "$CO_CNS_DIR"
python post_train_predict_script.py -m CNB -o "$CO_CNB_DIR"
python post_train_predict_script.py -m CNL -o "$CO_CNL_DIR"
python post_train_predict_script.py -m CNXL -o "$CO_CNXL_DIR"

python post_train_predict_script.py -m XCEPTION -o "$CO_XCEPTION_DIR"
echo "Finished COMPLEXITY post-processing."
echo "ALL DONE"