# COVID19_household_exp (Code branch)

In the code branch:

  "COVID_estimation.r": Estimate the regression model in a stan file, and save the mcmc samples in COVID_weathertype_i_exconlinecloth_j_mcmc_empJPN_real_hes_popu_2021-1.data, where i = 0,1,2,3,4 and j = 0,1. The weather variable in the explanatory variables is the nationwide dummy for absolute humidity if i=0; the nationwide dummy for outside temperature if i=1; the nationwide average of absolute humidity if i=2; and the nationwide average of outside temperature if i=3. No weather variable in the explanatory variables if i=4. Household expenditures for clothing and footwear in the explanatory variables include both online and offline purchases if j=0; and only offline purchases if j=1. 

  "COVID_data_upload.r": Load data from csv files in the data folder (a source file).

  "COVID_figures.r": Plot figures in COVID_figures_weathertype_i_exconlinecloth_j_dummyfor2ndSE_k_ENG_TRUE.pdf, where i and j are the same indices as defined above, and k=0 if the time dummy for the second state of emergency is set to zero for out-of-sample forecasts and k=1 if it is not. This code also save the dependent variable, the explanatory variables, and the Delta-variant and vaccinations data in csv formats without Japanese characters that appear in the original data sources. This code generates R2_upto_June2021_weathertype_i_exconlinecloth_j_dummyfor2ndSE_k.txt and COVID_ccf_DEP_nom_hes_ENG_TRUE.txt, which record R^2 of the out-of-sample forecasts and the cross correlation between the dependent variable and lagged values of large categories of nominal household expenditures, respectively. i, j, and k are the same indices as defined above. 

  "COVID_def_func.r": Load user-defined functions (a source file).

  "data.zip": the folder for data files with Japanese characters in csv formats.
