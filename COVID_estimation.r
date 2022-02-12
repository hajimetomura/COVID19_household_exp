# Estimate a linear regression for the reproduction number with mobility report data and household expenditure data.
# Frequency: daily.

# Model 7: Baseline regression model.
# Model 8: Without weather variable in the explanatory variables.


########## Clear global workspace and plots. ################ 

if( dev.cur() > 1 ) dev.off() # Clear plots.
rm( list = ls( envir = globalenv() ), envir = globalenv() ) # Clear global workspace.


########## Set parameters ###################################

flag_pref_wgt <- 1 # Default value: 1. If = 1, use population share among prefectures to compute average absolute humidity across prefectures. If = 0, use new cases in the past 7 days.

log_abs_hum <- 0 # Default value: 0. If = 1, use a dummy that temperature exceeds 13 Celsius degree. If = 2, use the level of absolute humidity. If = 3, use the level of temperature. If = 4, use no weather date, Otherwise, use a dummy that absolute humidity exceeds 7. Unit: g/m^3.

log_dist_incub <- 0 #  Default value: 0. If = 1, use a log normal distribution based on Chinese data reported by Stephen A et al. (2020). =0, use the empirical distribution of incubation periods based on Sugishita (2020).

nominal_hes <- 0 # Default value: 0. If = 1, use nominal household expenditure data. If = 0, use real household expenditure data denominated by 2020 average CPI for each item.

exc_online_cloth <- 0 # Default value: 0. If = 1, the online expenditure on clothing and footwear is excluded from household expenditure on clothing and footwear in the regressor.

# Set the numbering of the stan model to be estimated.
if (log_abs_hum == 4){
  # No weather data among the regressors.
  mdl_number <- 8
}else{
  # Some weather data among the regressors. The type of weather date is set by the value of log_abs_hum.
  mdl_number <- 7
}

########## Call libraries and functions, and load data. ########################

library(cmdstanr) # Load cmdstanr for estimation.
library(seasonal)
library(rstan)
library(xtable)
library(tidyverse)
library(viridisLite)


# Call hand-made functions. 
source("./COVID_def_func.R",encoding="utf-8")

# Load and reform data from the source files. 
source("./COVID_data_upload.R",encoding="utf-8") # Load data from csv files.


########## Run a regression model ###################################

# Ensure the sample period stops at 2021 Jan, due to a concern on mutations.
if(hes_end[1] !=2021  || hes_end[2] !=1){
  stop("Set the end of the sample period for estimation to 2021 Jan.")
}

# Define the input into the model.

if (mdl_number == 7){
  # Some weather data among the regressors.
  # The stan file uses only the values of explanatory variables from the second element (which is for 2020 February 16.)
  dat <- list(R_TT = length(RoC_newcases_data), # From 2020 March 1.
              R = RoC_newcases_data, # The rate of change in the number of new cases over a week.
              H_TT = ncol(H_expvals), # From 2020 Jan. 1.
              H_expvals = H_expvals,
              M_TT = mob_ndays, # From 2020 Feb. 15.
              M_trans = mob_var4[,1], # Unit: Percentage points.
              W_TT = length(temper_var_ave), # From 2020 Jan. 1.
              W_abs_hum = W_abs_hum, # Unit: g/m^3 or log of g/m^3 or dummy.
              TT_diff_M_H = 31+14, # Number of days between the first sample dates of M_series and H_series. 
              TT_diff_M_W = 31+14, # Number of days between the first sample dates of M_series and W_series. 
              dist_incub =dist_incub,
              D_NY = D_NY, # From 2020 Feb. 15, the same as M_series. 
              D_SE1 = D_SE1, # Dummy variable for the declaration of the first state of emergency.
              D_SE2 = D_SE2, # Dummy variable for the declaration of the second state of emergency.
              D_pre_SE1 = D_pre_SE1 # Dummy variable for the period before the first state of emergency.
  )
}else{
  # No weather data among the regressors.
  # The stan file uses only the values of explanatory variables from the second element (which is for 2020 February 16.)
  dat <- list(R_TT = length(RoC_newcases_data), # From 2020 March 1.
              R = RoC_newcases_data, # The rate of change in the number of new cases over a week.
              H_TT = ncol(H_expvals), # From 2020 Jan. 1.
              H_expvals = H_expvals,
              M_TT = mob_ndays, # From 2020 Feb. 15.
              M_trans = mob_var4[,1], # Unit: Percentage points.
              # W_TT = length(temper_var_ave), # From 2020 Jan. 1.
              # W_abs_hum = W_abs_hum, # Unit: g/m^3 or log of g/m^3 or dummy.
              TT_diff_M_H = 31+14, # Number of days between the first sample dates of M_series and H_series. 
              # TT_diff_M_W = 31+14, # Number of days between the first sample dates of M_series and W_series. 
              dist_incub =dist_incub,
              D_NY = D_NY, # From 2020 Feb. 15, the same as M_series. 
              D_SE1 = D_SE1, # Dummy variable for the declaration of the first state of emergency.
              D_SE2 = D_SE2, # Dummy variable for the declaration of the second state of emergency.
              D_pre_SE1 = D_pre_SE1 # Dummy variable for the period before the first state of emergency.
  )
}

# Activate parallel computing. See Matuura (2016), page 46.
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores()) 



if (log_dist_incub == 1){
  incub_sfx <- "estCHN" # Based on Stephen A et al. (2020).
}else{
  incub_sfx <- "empJPN" # Based on Sugishita (2020).
}

if (nominal_hes == 1){
  nominal_sfx <- "nominal_hes" # Use nominal household expenditure.
}else{
  nominal_sfx <- "real_hes" # Use real household expenditure denominated by 2020 CPI average for each item.
}
  
if (flag_pref_wgt == 1){
  weath_sfx <- "popu"
  
}else{
  weath_sfx <- "nwcs"
}

# Define file names.
thm <- "COVID_regress_model"
mdl_nm <- eval(parse(text=paste0("\"",thm,mdl_number,".stan\""))) # Name of the model to estimate.
file_summary_nm <- eval(parse(text=paste0("\"",thm,mdl_number,"_weathertype",log_abs_hum,"_exconlinecloth",exc_online_cloth,"_fit-summary_",incub_sfx,"_",nominal_sfx,"_",weath_sfx,"_",hes_end[1],"-",hes_end[2],".csv\""))) # Name of the file for the summary of estimates.
file_mcmc_nm <- eval(parse(text=paste0("\"",thm,mdl_number,"_weathertype",log_abs_hum,"_exconlinecloth",exc_online_cloth,"_mcmc_",incub_sfx,"_",nominal_sfx,"_",weath_sfx,"_",hes_end[1],"-",hes_end[2],".data\""))) # Name of the file for the mcmc samples.

# Run a stan model.
# Use cmdstan for estimation.

# Define a stan model for cmdstan.
stnfl_pth <-  file.path("./",mdl_nm) # Path of the stan file.
cmdstn_mod <- cmdstan_model(stnfl_pth) # Define a model for cmdstanr. 

# Draw mcmc samples.
cmdstn_fit <- cmdstn_mod$sample(
  data=dat,
  seed=18,
  adapt_delta=0.99, 
  max_treedepth = 20,
  show_messages = FALSE
)

# Convert the mcmc samples into a rstan object.
fit <- rstan::read_stan_csv(cmdstn_fit$output_files())

# Save the mcmc sample.
save(fit, file=file_mcmc_nm)

# Record the parameter estimates
write.csv(data.frame(summary(fit)$summary),file=file_summary_nm)



########## Write a table of the parameter estimates in a tex file. #####################

sink(paste0("para_estim_model",mdl_number,"_weathertype",log_abs_hum,"_exconlinecloth",exc_online_cloth,"_",incub_sfx,"_","_",hes_end[1],"-",hes_end[2],".txt"))
# Define the variable names in the model, which are the same order as in the stan file except six R_err_0 before parameters contained in conv_para_vn2.
if (mdl_number==7){
  # Some weather data among the regressors.
  conv_para_vn1 <- c("$\\sigma_{\\eta}$","$\\alpha_0$","$\\alpha_1$","$\\alpha_2$","$\\beta_1$","$\\beta_2$","$\\beta_0$","$\\gamma_1$","$\\gamma_2$","$\\gamma_3$","$\\gamma_4$","$\\gamma_5$","$\\gamma_6$","$\\gamma_7$","$\\gamma_8$","$\\gamma_9$","$\\theta_1$","$\\theta_2$","$\\theta_3$","$\\theta_4$","$\\theta_5$","$\\theta_6$","$\\theta_7$","$\\theta_8$","$\\theta_9$","$\\psi_{11}$","$\\psi_{12}$","$\\psi_{13}$","$\\psi_{14}$","$\\psi_{15}$","$\\psi_{16}$","$\\psi_{17}$","$\\psi_{18}$","$\\psi_{19}$","$\\psi_{21}$","$\\psi_{22}$","$\\psi_{23}$","$\\psi_{24}$","$\\psi_{25}$","$\\psi_{26}$","$\\psi_{27}$","$\\psi_{28}$","$\\psi_{29}$","$\\psi_{01}$","$\\psi_{02}$","$\\psi_{03}$","$\\psi_{04}$","$\\psi_{05}$","$\\psi_{06}$","$\\psi_{07}$","$\\psi_{08}$","$\\psi_{09}$")
  conv_para_vn2 <- c("$\\rho$")
}else{
  # No weather data among the regressors.
  conv_para_vn1 <- c("$\\sigma_{\\eta}$","$\\alpha_0$","$\\alpha_1$","$\\beta_1$","$\\beta_2$","$\\beta_0$","$\\gamma_1$","$\\gamma_2$","$\\gamma_3$","$\\gamma_4$","$\\gamma_5$","$\\gamma_6$","$\\gamma_7$","$\\gamma_8$","$\\gamma_9$","$\\psi_{11}$","$\\psi_{12}$","$\\psi_{13}$","$\\psi_{14}$","$\\psi_{15}$","$\\psi_{16}$","$\\psi_{17}$","$\\psi_{18}$","$\\psi_{19}$","$\\psi_{21}$","$\\psi_{22}$","$\\psi_{23}$","$\\psi_{24}$","$\\psi_{25}$","$\\psi_{26}$","$\\psi_{27}$","$\\psi_{28}$","$\\psi_{29}$","$\\psi_{01}$","$\\psi_{02}$","$\\psi_{03}$","$\\psi_{04}$","$\\psi_{05}$","$\\psi_{06}$","$\\psi_{07}$","$\\psi_{08}$","$\\psi_{09}$")
  conv_para_vn2 <- c("$\\rho$")
}

# Extract the posterior mean, 2.5% point, and 97.5% point of time-invariant parameters.
# The length(conv_para_vn1)+1-th row is skipped not to print the initial value of error in the observation equation.
temp <- data.frame(summary(fit)$summary)[c(1:length(conv_para_vn1),length(conv_para_vn1)+1+1:length(conv_para_vn2)),c(1,4,8)]
# Among the household expenditures included in the explanatory variables, clothing and footwear is placed before admissions, viewing game fees in the matrix for explanatory variables used for estimation, H_exp_all. In the table, place the coefficient of clothing and footwear after that of admissions, viewing, and game fees. 
temp_swap <- temp # Define the vehicle matrix to swap rows.
if (mdl_number==7){
  temp_swap[1+2*3+seq(7,45,by=9),] <- temp[1+2*3+seq(6,45,by=9),] # Moving the estimates of coefficients associated with clothing and footwear to 7th rows in each group of coefficients of household expenditures.
  temp_swap[1+2*3+seq(6,45,by=9),] <- temp[1+2*3+seq(7,45,by=9),] # Moving the estimates of coefficients associated with admissions, viewing, and game fees to 6th rows in each group of coefficients of household expenditures.
}else{
  temp_swap[1+2+3+seq(7,45,by=9),] <- temp[1+2+3+seq(6,45,by=9),] # Moving the estimates of coefficients associated with clothing and footwear to 7th rows in each group of coefficients of household expenditures.
  temp_swap[1+2+3+seq(6,45,by=9),] <- temp[1+2+3+seq(7,45,by=9),] # Moving the estimates of coefficients associated with admissions, viewing, and game fees to 6th rows in each group of coefficients of household expenditures.
}
temp <- temp_swap # Redefine the matrix for the coefficient table with swapped rows.
# Replace the data labels in the stan file into those in the paper.
rownames(temp) <- c(conv_para_vn1,conv_para_vn2) 
# Print the table in the latex form.
print(xtable(temp,digits=c(0,rep(3,3))), sanitize.rownames.function = identity)
sink()




