# A computer vision-based assessment of post-disturbance forest resilience

## Overview

**Authors:** Kilian Hochholzer, Maria Potterf, Michael Reuss, Rupert Seidl, Werner Rammer

**Project:** Rapid assessment of post-disturbance plot images using pretrained deep neural networks (DNNs)

This repository contains code, notebooks and analysis used to train and evaluate pretrained convolutional neural networks for multi-label classification of GoPro images taken at recently disturbed forest plots. The trained model outputs are further used to model and predict post-disturbance vegetation trajectories.

## Abstract (short)

We trained and compared multiple pretrained DNN architectures to (1) classify plot-level imagery (groundcover and surrounding-plot complexity) and (2) generate per-image and per-plot prediction arrays used in downstream trajectory analysis. This repository reproduces the code and analysis used in the accompanying manuscript; data access and usage instructions are provided below.

## Citation

Please cite the paper associated with this repository when using the code or results. A placeholder citation is provided in `CITATION.bib`; replace the DOI/identifier once available.

## Key contents

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

2) Train all models (Git Bash or WSL recommended on Windows):

```bash
./1_train_all_models.sh
```

3) Run predictions for trained models:

```bash
./2_run_predictions.sh
```

4) Run post-processing to create summary metrics and CSV exports:

```bash
./3_post_train.sh
```

You can also run the main Python entrypoints directly for more control:

```bash
python train_one_fold.py --config configs/<model>.yaml
python predict.py --model output/<model>/<timestamp>/best.ckpt --data OG_data/ --out output/<model>/<timestamp>/predictions.npy
```

Refer to each script's docstring or `--help` for full CLI arguments.

## Directory layout

- `1_dataRaw/` — labels and metadata (not included in this repository; see Data Availability)
- `output/` — trained models, checkpoints, per-model metrics, and exported predictions
- `notebooks/` — interactive notebooks used during development and analysis
- `R/` — R scripts and artifacts for downstream analysis and figure generation
- `*.py` — training, prediction, preprocessing, and helper scripts
- `*.sh` — orchestration scripts for batch runs

## Data formats and required inputs

- Raw images: JPEG/PNG in `1_dataRaw/` (organization: per-patch folders referenced in CSVs)
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

The raw `1_dataRaw/` used in the analyses is not included in this repository. Access to the data is restricted and available on request. Contact the corresponding author listed in the published paper. When possible, we will provide a curated subset for reproducibility testing.

## License

This repository is released under the Creative Commons Attribution 4.0 International (CC BY 4.0) license. See the `LICENSE` file for details.

## Citation and DOI

Fill in the DOI / arXiv ID in `CITATION.bib` after publication. See `CITATION.bib` for the placeholder entry.

## Contributing

- Please open issues for reproducibility problems or bugs.
- Use pull requests for code changes and include minimal tests where applicable.

## Acknowledgements

Funded by the FutureForest project. Thanks to field teams for image collection and to collaborators who provided feedback during development.

## Contact

For questions about reproducing analyses, requesting data, or other inquiries, open an issue or contact the authors listed in the paper.

## TODO (post-publication)

- Replace citation placeholders with final DOI/identifier
- Map each figure in the paper to the exact `output/<model>/<timestamp>/` folder used to generate it
- Optionally publish a small curated sample dataset for smoke-testing the reproducibility workflow

## Reproducibility checklist

Follow these steps to verify the repository environment and reproduce core outputs used in the paper. These are minimal smoke-tests; full reproduction requires the original `OG_data/` and configuration files.

- **1 — Create environment**: create and activate the conda environment (use `rra_plot.yml` if present).

```bash
conda env create -f rra_plot.yml
conda activate rra_plot
```

- **2 — Verify script help / CLI**: confirm main entrypoints print help text.

```bash
python train_one_fold.py --help
python predict.py --help
```

- **3 — Smoke-test prediction (single image)**: run `predict.py` on a single example image (user must provide sample image and minimal CSV mapping if required by the script).

```bash
python predict.py --model path/to/sample_checkpoint.ckpt --data OG_data/sample.jpg --out output/sample_predictions.npy
python npy_to_csv.py output/sample_predictions.npy output/sample_predictions.csv
```

- **4 — Run a single training fold (dry-run / quick)**: run `train_one_fold.py` with a small config or reduced dataset to confirm training loop works. Example (adjust `--config` to a minimal config):

```bash
python train_one_fold.py --config configs/minimal.yaml
```

- **5 — Run full pipeline scripts** (on a machine with required resources):

```bash
./1_train_all_models.sh
./2_run_predictions.sh
./3_post_train.sh
```

- **6 — Check outputs**: confirm model folders under `output/<model>/<timestamp>/` contain `checkpoints/`, `metrics.csv` (or `metrics.json`), `predictions.npy`, and any `figures/` used in the manuscript.

- **7 — Record environment and seed**: when reproducing results for publication, include the conda environment YAML, Git commit hash, and random seeds used. Optionally attach these as a release asset or in `reproducibility.md`.

If you want, I can add a small `scripts/sample_smoke_test.sh` that runs steps 2–4 on a bundled tiny sample dataset (you would need to provide or allow creation of a tiny example image and metadata). 