# A computer vision-based assessment of post-disturbance forest resilience

## Overview

**Project:** Rapid assessment of post-disturbance plot images using pretrained deep neural networks (DNNs)

This repository contains code, notebooks and analysis used to train and evaluate pretrained convolutional neural networks for multi-label classification of GoPro images taken at recently disturbed forest plots. The trained model outputs are further used to model and predict post-disturbance vegetation trajectories.

## Abstract (short)

We trained and compared multiple pretrained DNN architectures to (1) classify plot-level imagery (groundcover and surrounding-plot complexity) and (2) generate per-image and per-plot prediction arrays used in downstream trajectory analysis (disturbance detection, severity, development pathway). This repository reproduces the code and analysis used in the accompanying manuscript; data access and usage instructions are provided below.

## Citation

Please cite the paper associated with this repository when using the code or results.

## Key contents

- Script for image path gathering: `gather_images.py`
- Training and orchestration scripts: `1_train_all_models.sh`, `train_one_fold.py`, `one_fold_wrapper.py`
- Prediction and post-processing: `2_run_predictions.sh`, `predict.py`, `post_train_predict_script.py`, `npy_to_csv.py`
- Preprocessing and model definitions: `preprocess.py`, `model_zoo.py`, `GradCAM.py`
- Notebooks: `notebooks/AI_Workflow.ipynb`, `notebooks/Visualizing_activations.ipynb`, and others for exploration and plotting
- R analysis: all downstream statistical analysis and plotting in the `R/` folder
- Output: trained checkpoints, metrics, and exported prediction arrays in `output/` (per-model subfolders)

## Quick start (recommended)

1) Create the conda environment (Anaconda / Miniconda; tested with Python 3.11). If `rra_plot.yml` exists in this repository, use it; otherwise adapt your environment to match dependency versions noted in notebooks.

```bash
conda env create -f rra_plot.yml
conda activate rra_plot
```

2) User gather images to save paths to the image data (change paths in the script)
```bash
python gather_images.py
```

3) Train all models (Git Bash or WSL recommended on Windows):

```bash
./1_train_all_models.sh
```

4) Run predictions for trained models:

```bash
./2_run_predictions.sh
```

5) Run post-processing to create summary metrics and CSV exports:

```bash
./3_post_train.sh
```

Refer to each script's docstring or `--help` for full CLI arguments. 
After predictions are available, the analysis found in the R directory can be used.

## Directory layout

- `1_dataRaw/` — labels and metadata (not included in this repository; see Data Availability)
- `output/` — trained models, checkpoints, per-model metrics, and exported predictions (not included in this repository, will be created during model training and predictions)
- `notebooks/` — interactive notebooks used during development and analysis (optional, if wanting to understand certain aspects in more detail)
- `R/` — R scripts and artifacts for downstream analysis and figure generation (usefull after training and prediction runs)
- `*.py` — training, prediction, preprocessing, and helper scripts
- `*.sh` — orchestration scripts for batch runs

## Data formats and required inputs

- Raw images: jpg/JPG in `1_dataRaw/` (organization: per-patch folders referenced in CSVs)
- Labels: CSV files that map image paths to labels
- Model outputs: NumPy `.npy` files with logits/probabilities
- Checkpoints: saved in `output/<model>/<timestamp>/` (framework-specific checkpoint files)

## Reproducibility

- Recreate the environment with `rra_plot.yml` and install any missing dependencies.
- Ensure `1_dataRaw/` is placed at repository root and matches the structure expected by `preprocess.py`.
- Run a single-fold training to verify everything works before executing the full `1_train_all_models.sh` flow.

Example quick smoke test to run after environment setup (user must provide at least one image and minimal CSV):

```bash
python predict.py -m model_abreviation -o path/to/sample_checkpoint.ckpt
python post_train_predict_script.py -m model_abreviation -o path/to/model/location
```

## Outputs and interpretation

Model outputs and analysis artifacts are organized under `output/` by model name and timestamp. Typical contents of a model run folder:

- `checkpoints/` — saved model weights
- `predictions.csv` — per-image logits/probabilities
- `figures/` — plots used in analyses and the paper

## Notebooks and analysis

- `notebooks/AI_Workflow.ipynb` — Guides through the training, prediction and post-prediction process.
- `notebooks/Visualizing_activations.ipynb` — visualization of model activations and Grad-CAM outputs
- R scripts in `R/` perform statistical analyses and generate figures for the manuscript.

## Data Availability

The raw `1_dataRaw/` used in the analyses is not included in this repository. Access to the data is restricted and available on request. Contact the corresponding author listed in the published paper.

## License

This repository is released under the Creative Commons Attribution 4.0 International (CC BY 4.0) license. See the `LICENSE` file for details.

## Contributing

- Please open issues for reproducibility problems or bugs.
- Use pull requests for code changes and include minimal tests where applicable.

## Acknowledgements

The study this repository belongs to the [FutureForest.ai](https://future-forest.eu/) project, funded by the German Federal Ministry for the Environment, Climate Action, Nature Conservation and Nuclear Safety (67KI21002A). Thanks to field teams for image collection and to collaborators who provided feedback during development.

## Contact

For questions about reproducing analyses, requesting data, or other inquiries, open an issue or contact the authors listed in the paper.
