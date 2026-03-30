# Notes

## About

Data generated from pictures using DNNs are here analysed. Meaning within a couple of steps a likely development of this disturbed site will be predicted. For this different machine learning models are created and the resulting models used to make a prediction about the future.

## Workflow

A mixture of .R - scripts and .Rmd - files are used.
- 00_label_plausibility.R - Script used for checking label plausibility (Figure S6).
- 01_Visualize_train_val.Rmd - Loads the history.csv files from the model creation and creates some plots with it. Used to get an impression which model had a good training and validation run as well. These results, together with the test results in the model directories help with the model selection. Also Supplement Tables S2 and S3 are created here.
- 02_Analysis.Rmd - Main analysis notebook, that calculates all metrics and creates or delivers the data for most figures (S4, S5) and tables (2, S4) in the publication.
- 03_table_figure_creation.R - Script that creates Table 2, Figure 3, Figure 4 for the publication.

So the actual workflow is 00_label_plausibility.R, 01_Visualize_train_val.Rmd, 02_Analysis.Rmd, 03_table_figure_creation.R

- support_functions.R - offer some functionality that is used throughout, like getting latest directory, metric calculations like MAE, RMSE
