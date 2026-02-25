# Notes

## About

Data generated from pictures using DNNs are here analysed. Meaning within a couple of steps a likely development of this disturbed site will be predicted. For this different machine learning models are created and the resulting models used to make a prediction about the future.

## Workflow

A mixture of .R - scripts and .Rmd - files are used.
- 01_Visualize_train_val.Rmd - Loads the history.csv files from the model creation and creates some plots with it. Used to get an impression which model had a good training and validation run as well. These results, together with the test results in the model directories help with the model selection.
- 02_Initial analysis.Rmd - Takes a closer look at the models that showed good results during the 01_Visualize_train_val.Rmd run.
- 03_Calc_DevIndicators.Rmd - The first approach to create indicators from picture data and work towards the development direction. This file is deprecated, it is replaced by 04_Results_Paper_1.Rmd
- 04_Results_Paper_1.Rmd - Here all the calculations and transformations for the final paper are done.

So the actual workflow is 01_Visualize_train_val.Rmd, 02_Initial analysis, 04_Results_Paper_1.Rmd

- Complexity_Analysis.R - Compares the complexity of different pictures created from different DNNs (initial performance analysis of DNNs for Complexity grade)
- Figure_creation.R - Used as a side script to safe and play around with figure creation.
- GetUnlabeledPictures.R - Was used to see which pictures out of all pictures in the rapid assessment dataset were not labeled.

### Data required

TODO ...