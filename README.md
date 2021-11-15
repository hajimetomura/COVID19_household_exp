# COVID19_household_exp (Code branch)

In the code branch:

  "COVID_estimation.r": Estimate the regression model in a stan file, and save the mcmc samples in a COVID_regress_model_mcmc.data.  

  "COVID_data_upload.r": Load data from csv files in the data folder (a source file).

  "COVID_figures.r": Plot figures, and save the dependent variable, the explanatory variables, and the Delta-variant and vaccinations data in csv formats without Japanese characters that appear in the original data sources. This code also generates R2_pred_Covid_regress_model_upto_June2021.txt and COVID_regress_ccf_DEP_nom_hes.txt, which record R^2 of the out-of-sample forecasts and the cross correlation between the dependent variable and lagged values of large categories of nominal household expenditures. COVID_regress_DEP_nom_hes.pdf is produced as a byproduct. This pdf file can be ignored.

  "COVID_def_func.r": Load user-defined functions (a source file).

  "data.zip": the folder for data files with Japanese characters in csv formats.
