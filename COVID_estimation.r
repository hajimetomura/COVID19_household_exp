# Estimate a linear regression for the reproduction number with mobility report data and household expenditure data.
# Frequency: daily.


########## Clear global workspace and plots. ################ 

if( dev.cur() > 1 ) dev.off() # Clear plots.
rm( list = ls( envir = globalenv() ), envir = globalenv() ) # Clear global workspace.

########## Call libraries and functions, and load data. ########################

library(cmdstanr) # Load cmdstanr for estimation.
library(seasonal)
library(rstan)
library(xtable)

# Call hand-made functions. 
source("./COVID_def_func.R",encoding="utf-8")

# Load and reform data from the source files. 
source("./COVID_data_upload.R",encoding="utf-8") # Load data from csv files.



########## Run a regression model ###################################

# Ensure the sample period stops at 2021 Jan, due to a concern on mutations.
# mdl_Jan2021 is set by COVID_nationwide_data.R
if(hes_end[1] !=2021  || hes_end[2] !=1){
  eval(parse(text=paste0("stop(\"Set the end of the sample period for estimation to 2021 Jan.\")")))
}

# Define the input into the model.

# The stan file is written in a way that it uses only the values of explanatory variables from the second element (which is for 2020 February 16.) So inputs of explanatory variables must start from 2020 February 15.
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


# Activate parallel computing. See Matuura (2016), page 46.
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores()) 



# Define file names.
thm <- "COVID_regress_model"
mdl_nm <- eval(parse(text=paste0("\"",thm,".stan\""))) # Name of the model to estimate.
file_summary_nm <- eval(parse(text=paste0("\"",thm,"_fit-summary.csv\""))) # Name of the file for the summary of estimates.
file_results_nm <- eval(parse(text=paste0("\"",thm,"_results.pdf\""))) # Name of the file for estimation results.
file_mcmc_nm <- eval(parse(text=paste0("\"",thm,"_mcmc.data\""))) # Name of the file for the mcmc samples.

# Run a stan model.

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
sink(paste0("para_estim_model.txt"))
# Define the variable names in the model, which are the same order as in the stan file except six R_err_0 before parameters contained in conv_para_vn2.
conv_para_vn1 <- c("$\\sigma_{\\eta}$","$\\alpha_0$","$\\alpha_1$","$\\alpha_2$","$\\beta_1$","$\\beta_2$","$\\beta_0$","$\\gamma_1$","$\\gamma_2$","$\\gamma_3$","$\\gamma_4$","$\\gamma_5$","$\\gamma_6$","$\\gamma_7$","$\\gamma_8$","$\\gamma_9$","$\\theta_1$","$\\theta_2$","$\\theta_3$","$\\theta_4$","$\\theta_5$","$\\theta_6$","$\\theta_7$","$\\theta_8$","$\\theta_9$","$\\psi_{21}$","$\\psi_{22}$","$\\psi_{23}$","$\\psi_{24}$","$\\psi_{25}$","$\\psi_{26}$","$\\psi_{27}$","$\\psi_{28}$","$\\psi_{29}$","$\\psi_{31}$","$\\psi_{32}$","$\\psi_{33}$","$\\psi_{34}$","$\\psi_{35}$","$\\psi_{36}$","$\\psi_{37}$","$\\psi_{38}$","$\\psi_{39}$","$\\psi_{11}$","$\\psi_{12}$","$\\psi_{13}$","$\\psi_{14}$","$\\psi_{15}$","$\\psi_{16}$","$\\psi_{17}$","$\\psi_{18}$","$\\psi_{19}$")
conv_para_vn2 <- c("$\\rho$")

# Extract the posterior mean, 2.5% point, and 97.5% point of time-invariant parameters.
# The error in the observation equation for R does not have serial correlation. The previous value of error is not recorded as a state variable.
temp <- data.frame(summary(fit)$summary)[c(1:length(conv_para_vn1),length(conv_para_vn1)+1+1:length(conv_para_vn2)),c(1,4,8)]
# Replace the data labels in the stan file into those in the paper.
rownames(temp) <- c(conv_para_vn1,conv_para_vn2) 
# Print the table in the latex form.
print(xtable(temp,digits=c(0,rep(3,3))), sanitize.rownames.function = identity)
sink()



