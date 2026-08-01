# Notes

## About

Data generated from pictures using DNNs are here analysed. Meaning within a couple of steps a likely development of this disturbed site will be predicted. For this different machine learning models are created and the resulting models used to make a prediction about the future.

## Workflow

A mixture of .R - scripts and .Rmd - files are used.
- 00_label_plausibility.R - Legacy script used for checking label plausibility (Figure S6). Logic is now integrated in generate_supplement_figures.R.
- 01_Visualize_train_val.Rmd - Loads the history.csv files from the model creation and creates some plots with it. Used to get an impression which model had a good training and validation run as well. These results, together with the test results in the model directories help with the model selection. Also Supplement Tables S2 and S3 are created here.
- 02_Analysis.Rmd - Main analysis notebook, historically used to create or deliver data for several supplement outputs. Relevant logic has been migrated into generate_supplement_figures.R for automated supplement builds.
- 03_table_figure_creation.R - Script that creates Table 2, Figure 3, Figure 4 for the publication.
- analysis.R - Runs the standardized analysis pipeline and writes output/analyses/*.rds objects.
- generate_supplement_figures.R - Unified supplement builder (S1-S6) from one selected analysis object in output/analyses/.

Current recommended workflow:
1. Run analysis.R.
2. Set analysis_name in generate_supplement_figures.R.
3. Run generate_supplement_figures.R to write S1-S6 into output/supplement/{analysis_name}.

Legacy workflow was 00_label_plausibility.R, 01_Visualize_train_val.Rmd, 02_Analysis.Rmd, 03_table_figure_creation.R.

- support_functions.R - offer some functionality that is used throughout, like getting latest directory, metric calculations like MAE, RMSE
