# Draw plots by ggplot2.

# Model 7: Baseline regression model.
# Model 8: Without weather variable in the explanatory variables.

########## Clear global workspace and plots. ################

if( dev.cur() > 1 ) dev.off() # Clear plots.
rm( list = ls( envir = globalenv() ), envir = globalenv() ) # Clear global workspace.

########## Set parameters ###################################

flag_pref_wgt <- 1 # Default value: 1. If = 1, use population share among prefectures to compute average absolute humidity across prefectures. If = 0, use new cases in the past 7 days.

log_abs_hum <- 0 # Default value: 0. If = 1, use a dummy that temperature exceeds 13 Celsius degree. If = 2, use the level of absolute humidity. If = 3, use the level of temperature. If = 4, use no weather date, Otherwise, use a dummy that absolute humidity exceeds 7. Unit of absolute humidity: g/m^3.

log_dist_incub <- 0 #  Default value: 0. If = 1, use a log normal distribution based on Chinese data reported by Stephen A et al. (2020). =0, use the empirical distribution of incubation periods based on Sugishita (2020).

nominal_hes <- 0 # Default value: 0. If = 1, use nominal household expenditure data. If = 0, use real household expenditure data denominated by 2020 average CPI for each item.

exc_online_cloth <- 0 # Default value: 0. If = 1, the online expenditure on clothing and footwear is excluded from household expenditure on clothing and footwear in the regressor.

fcst_td <- 0 # Default value: 0. If = 1, the second state of emergency dummy is set to one for out-of-sample forecasts.

ENG <- 1 # If = 1, write labels in English.

plot_paper <- 1 # If =1 and ENG=1, the main titles of some plots are set for the paper, rather than slides.

Dates_SE_list <- c("2020-04-07","2020-05-25","2021-01-07","2021-03-21","2021-04-25","2021-06-20","2021-07-12","2021-09-30") # Dates of the start and end of states of emergency.

# Set the numbering of the stan model to be estimated.
if (log_abs_hum == 4){
  # No weather data among the regressors.
  mdl_number <- 8
}else{
  # Some weather data among the regressors. The type of weather date is set tby the value of log_abs_hum.
  mdl_number <- 7
}


########## Call libraries and functions, and load data. ########################

library(seasonal)
library(rstan)
library(xtable)
library(tidyverse)
library(viridisLite)

Sys.setlocale("LC_TIME", "English") # Set the English expression for dates.

# Call hand-made functions. 
source("./COVID_def_func.R",encoding="utf-8")

# Load and reform data from the source files. 
source("./COVID_data_upload.R",encoding="utf-8") # Load data from csv files.


########## Load estimates of a linear regression model ################

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
file_mcmc_nm <- eval(parse(text=paste0("\"",thm,mdl_number,"_weathertype",log_abs_hum,"_exconlinecloth",exc_online_cloth,"_mcmc_",incub_sfx,"_",nominal_sfx,"_",weath_sfx,"_",hes_end[1],"-",hes_end[2],".data\""))) # Name of the file for the mcmc samples.

# Load data and estimates of a linear regression model of the reproduction number. The mcmc samples are stored in the S4 object named "fit".
load(file=file_mcmc_nm)

# Extract mcmc samples.
ms <- rstan::extract(fit)

# Extract the fitted value of R from mcmc samples.

name_list<-c("Fitted_R") # Variable name in the stan file.

for (i in 1:length(name_list)){
  
  eval(parse(text=paste0("temp <- Extract_mean_ptl(ms$",name_list[i],", ptl=c(0.025,0.975))")))
  eval(parse(text=paste0(name_list[i],"_pm <- temp[[1]]"))) # posterior mean
  eval(parse(text=paste0(name_list[i],"_025 <- temp[[2]]"))) # 2.5% percentitle
  eval(parse(text=paste0(name_list[i],"_975 <- temp[[3]]"))) # 97.5% percentitle
}



# Save plots of data in a pdf file.
eval(parse(text=paste0("pdf(paste0(\"COVID_figures_weathertype_",log_abs_hum,"_exconlinecloth_",exc_online_cloth,"_dummyfor2ndSE_",fcst_td,"_ENG_",as.logical(ENG),".pdf\"), family=\"Japan1GothicBBB\")")))



############## Plot the sample distribution of incubation periods ##############################

# Define labels for the plot.
if (ENG==1){
  if (plot_paper==1){
    temp_main_label <- ""
  }else{
    temp_main_label <- "Incubation periods"
  }
  temp_xlab <- "Days"  
  temp_ylab <- "Share of 125 cases (%)"
}else{
  temp_main_label <- "潜伏期間"
  temp_xlab <- "日"
  temp_ylab <- "125例中の割合 (%)" 
}

# Define a tibble for plotting. The elements of dist_incub is in the order from 14 days to one day. So it must be reversed for the plot.
temp_df <- tibble(Days = 1:14, value=rev(dist_incub)*100)

# Plot data.
temp_p <- ggplot(temp_df,aes(x=Days,y=value))+
  geom_bar(stat = "identity")+
  labs(title=temp_main_label, x=temp_xlab, y=temp_ylab)

# Print the plot.
print(temp_p)


######### Plot the absolute humidity dummy. ###############

#temp <- 1 - ind_abs_hum_var_ave # Dummy variable for the weighted average of absolute humidity across prefectures.  

# Define labels for the plot.
temp_ylab =""
if (ENG==1){
  temp_xlab <- "Dates"
  if (plot_paper==1){
    temp_main_label <- ""
  }else{
    temp_main_label <- expression(D[paste("AH,t")])
  }
}else{
  temp_xlab <- "日次"
  temp_main_label <- expression(D[paste("AH,t")])
}


# Define a tibble for plotting.
temp_df <- tibble(Dates = as.Date(weath_date_all), value = W_abs_hum_all)

# Plot data.
temp_p <- ggplot(temp_df,aes(x=Dates,y=value))+
  geom_line()+
  labs(title=temp_main_label, x=temp_xlab, y=temp_ylab)+
  scale_x_date(date_breaks = "2 months", date_labels = "%b%d\n%Y")

# Print the plot.
print(temp_p)



######### Plot the L452R share of new cases nationwide and in Tokyo #############################

# Define a tibble to export Tokyo data.
# The sample period for Tokyo data on Tokyo: From the week of May 3 to the week of Nov. 1.
# Drop the first element of L452R_share_Tokyo_w, as it is for April 30 to May 2, 2021, which is shorter than a week, and also the figure for this week is zero.
week_label_L452R_Tokyo <- R_date[which(R_date=="2021/5/3")+seq(0,365-sum(ndays_normal[1:5]),by=7)] # Use the date label for 2021 to create a vector of m/d in 2021 from May 3.
week_label_L452R_Tokyo <- seq(as.Date("2021-05-03"), as.Date(week_label_L452R_Tokyo[length(L452R_share_Tokyo_w)-1]), by="1 week")
                            
# Define the tibble.
tbl_L452R_Tokyo <- tibble(Dates = week_label_L452R_Tokyo, value = L452R_share_Tokyo_w[2:length(L452R_share_Tokyo_w)]*100)

# Define a tibble to export nation-wide data.
# The sample period for nation-wide data: From the week of May 31 to the week of Sep. 27.
# Define the labels for the weeks in the sample period.
temp_week_label <- R_date[which(R_date=="2021/5/31")+seq(0,365-sum(ndays_normal[1:5]),by=7)] # Use the date label for 2020 to create a vector of m/d in 2021 from May 31.
# Define a tibble for plotting.
tbl_L452R <- tibble(Dates = seq(as.Date("2021-05-31"), as.Date(temp_week_label[length(L452R_share_NW_w)]), by="1 week"), value = L452R_share_NW_w*100)


# Define a tibble for plotting the data.
temp_df <- mutate(tbl_L452R_Tokyo, value_nation = c(rep(NA,4),L452R_share_NW_w*100,rep(NA,5)))
# Apply plot longer to plot the nation-wide data and Tokyo data in the same figure.
temp_df <-  gather(temp_df, key = variable, value = value, -Dates)

# Define labels for the plot.
if (ENG==1){
  if (plot_paper==1){
    temp_main_label <- ""
  }else{
    temp_main_label <- "Delta-variant fraction of new cases"
  }
  temp_xlab <- "The first dates of weeks"  
  temp_ylab <- "%"
}else{
  temp_main_label <- "L452R陽性率（全国、週次）"
  temp_xlab <- "各週の初日"
  temp_ylab <- "%" 
}


# Plot data.
temp_p <- ggplot(temp_df,aes(x=Dates,y=value, color=variable))+
  geom_line()+
  geom_point()+
  scale_x_date(date_breaks = "1 month", date_labels = "%b%d\n%Y")+
  labs(title=temp_main_label, x=temp_xlab, y=temp_ylab)+
  theme(legend.position="bottom")+
  scale_color_viridis_d(name="",breaks=c("value_nation","value"), labels=c("Nationwide", "Tokyo"))

# Print the plot.
print(temp_p)



############### Plot the vaccinated share of population. ##################

# Define labels for the plot.
if (ENG==1){
  if (plot_paper){
    temp_main_label <- ""
  }else{
    temp_main_label <- "Fully vaccinated fraction of the population"
  }
  temp_xlab <- "Dates"  
  temp_y_lab <- "%"
}else{
  temp_main_label <- "二回ワクチン接種率（全国）"
  temp_xlab <- "日次"
  temp_ylab <- "%" 
}


# Define a tibble for plotting.
temp_df <- tibble(Dates = c(seq(as.Date("2021/1/1"), as.Date("2021/4/11"), by="day"), as.Date(vac_popu_date)), value = vccn_scnd_share*100)

# Save the tibble for data export.
tbl_vccn <- temp_df

# Plot data.
temp_p <- ggplot(temp_df,aes(x=Dates,y=value))+
  geom_line()+
  labs(title=temp_main_label, x=temp_xlab, y=temp_ylab)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b%d\n%Y")

# Print the plot.
print(temp_p)




############### Compute fitted values of the dependent variable with and without time dummies. ################

# Initialize the matrix that save the fitted values and their decomposition for each case.
Rec_R_fitted <- list()
Rec_R_decomp <- list()

# Compute fitted values with and without dummies.
for (j in 1:2){
  
  # Define the length of the sample period for dependent variables.
  # If the dependent variable is the rate of change in the number of new cases from 7 days ago:  
  # Because the right-hand side of the observation equation includes only the contributor for the current value of the dependent variable if the dependent variable is the effective reproduction number, only 0 is subtracted. 14 is the number of lags for explanatory variables.
  # The first date of samples of explanatory variables is Feb. 16, 2020. 
  H_expvals_fitted <- H_expvals_all[,which(hes_var_date_all=="2020/2/16"):which(hes_var_date_all==hes_end_date_estimation)]
  M_trans_fitted <- mob_var4[which(mob_date=="2020/2/16"):which(mob_date==hes_end_date_estimation),1]
  if (mdl_number==7){
    # Weather date is included among the regression.
    W_abs_hum_fitted <- W_abs_hum_all[which(weath_date_all=="2020/2/16"):which(weath_date_all==hes_end_date_estimation)]
  }else{
    # Weather date is not included among the regression.
    W_abs_hum_fitted <- NULL
  }
  
  # Define time dummies. The first date of each dummy is set to Feb. 15, 2020; so the first element must be dropped to start from Feb. 16, 2020.
  D_NY_fitted <- D_NY[2:length(D_NY)]
  D_SE1_fitted <- D_SE1[2:length(D_SE1)]
  D_SE2_fitted <- D_SE2[2:length(D_SE2)] 
  D_pre_SE1_fitted <- D_pre_SE1[2:length(D_pre_SE1)]
  

  
  # Set 0 for state-of-emergency dummies if j=2.
  if (j == 2){
    D_SE1_fitted <- D_SE2_fitted <- D_pre_SE1_fitted <- rep(0,length(D_NY_fitted))
  }
  
  # If the dependent variable is the rate of change in the number of new cases from 7 days ago:  
  #   The fitted values of the dependent variable within the estimation period end at one day after the sample period for explanatory variables in the estimation.
  #   Construct explanatory variables for out-of-sample forecasts from 14 (= 14 + 1 - 1) days before the last date of the fitted value of the dependent variable, so that the first reproduction number to simulate is on the 2nd date of the next month.
  # Set zeros for D_NY, as the available sample period does not include the new year period.
  temp_simu <- COVID_R_simu(ms_R = ms, H_expvals = H_expvals_fitted, M_trans = M_trans_fitted, W_abs_hum = W_abs_hum_fitted, dist_incub = dist_incub, D_NY = D_NY_fitted, D_SE1 = D_SE1_fitted, D_SE2 = D_SE2_fitted, D_pre_SE1 = D_pre_SE1_fitted)
  
  # Save the decomposition of fitted values.
  Rec_R_decomp[[j]] <- temp_simu$comp_effect
  
  # Extract the mcmc samples of the fitted reproduction number for the sample period beyond the estimation period.
  temp_fitted <- temp_simu$R
  
  # Compute the mean and the percentile values of time-varying parameters in vectors.
  
  name_list<-c("temp_fitted") # Variable name in the stan file.
  
  for (i in 1:length(name_list)){
    
    eval(parse(text=paste0("temp <- Extract_mean_ptl(",name_list[i],", ptl=c(0.025,0.975))")))
    eval(parse(text=paste0(name_list[i],"_pm <- temp[[1]]"))) # posterior mean
    eval(parse(text=paste0(name_list[i],"_025 <- temp[[2]]"))) # 5% percentile
    eval(parse(text=paste0(name_list[i],"_975 <- temp[[3]]"))) # 97.5% percentile
  }
  
  # Compare the fitted value of the dependent variable computed by COVID_R_simu() with that generated by the stan file.
  if (j==1 && sum((temp_fitted_pm-Fitted_R_pm)^2)/sum(Fitted_R_pm^2) > 0.00001){
    dev.off() # Close the pdf.
    stop("Computed fitted values of the dependent variable do not coincide with each other")
  }
  
  # Save the posterior mean and the credible interval of the fitted values.
  Rec_R_fitted[[j]] <- list(temp_fitted_pm, temp_fitted_025, temp_fitted_975)
  
}





############### Compute out-of-sample forecasts. ###############################3

# Check if there is a sample period beyond the estimation period.
if (hes_end[1]+(hes_end[2]-1)/12 < hes_end_all[1]+(hes_end_all[2]-1)/12){
  
  # The number of days in the moving average window on the right hand side of the observation equation for the dependent variable minus 1.   
  num_mv_days_rhs_reg <- 0 # 0 = 1 - 1

  ### Compute the out-of-sample forecast of the dependent variable.
  
  # Define the length of the sample period beyond the estimation period for dependent variables.
  # If the dependent variable is the rate of change in the number of new cases from 7 days ago:  
  #   Because the right-hand side of the observation equation includes only the contributor for the current value of the dependent variable if the dependent variable is the effective reproduction number, only 0 is subtracted. 14 is the number of lags for explanatory variables.
  fcst_end <- dim(H_expvals_all)[2] - (which(hes_var_date_all==hes_end_date_estimation)+2-num_mv_days_rhs_reg-14) + 1 
  
  # Define time dummies.
  D_NY_fcst <- rep(0,fcst_end)
  if (hes_end_all[1]>2021 || hes_end_all[2]==12){
    # There is a year-end and new-year holiday period within the out-of-sample forecast period.
    # The first date of dummies for out-of-sample forecasts is 14 days before Feb. 2, 2021, exclusive. 
    D_NY_fcst[13+sum(ndays_normal[2:11])+29:(31+3)] <- 1 
  }
  D_SE1_fcst <- rep(0,fcst_end)
  D_pre_SE1_fcst <- rep(0,fcst_end)

  if (fcst_td == 1){
    # Set the second state-of-emergency dummy to one during the second state of emergency (until March 21, 2021) included in the out-of-sample forecast period. 
    D_SE2_fcst <- c(rep(1,28-1+21), rep(0,fcst_end-(28-1+21)))
  }else{
    # Set the second state-of-emegency dummy to zero for out-of-sample forecasts. 
    D_SE2_fcst <- rep(0,fcst_end)
  }
  
  # If the dependent variable is the rate of change in the number of new cases from 7 days ago:  
  #   The fitted values of the dependent variable within the estimation period end at one day after the sample period for explanatory variables in the estimation.
  #   Construct explanatory variables for out-of-sample forecasts from 14 (= 14 + 1 - 1) days before the last date of the fitted value of the dependent variable, so that the first reproduction number to simulate is on the 2nd date of the next month.
  # Set zeros for D_NY, as the available sample period does not include the new year period.
  
  # Define weather data for out-of-sample forecasts.
  if (mdl_number == 7){
    # There are weather data among the regressor.
    temp_W <- W_abs_hum_all[(which(weath_date_all==weath_end_date_estimation)+2-num_mv_days_rhs_reg-14)+0:(fcst_end-1)]
  }else{
    # There are not weather data among the regressor.
    temp_W <- NULL
  }

  temp <- COVID_R_simu(ms_R = ms, H_expvals = H_expvals_all[,(which(hes_var_date_all==hes_end_date_estimation)+2-num_mv_days_rhs_reg-14)+0:(fcst_end-1)], M_trans = mob_var4[(which(mob_date==weath_end_date_estimation)+2-num_mv_days_rhs_reg-14)+0:(fcst_end-1), 1], W_abs_hum = temp_W, dist_incub = dist_incub, D_NY = D_NY_fcst, D_SE1 = D_SE1_fcst, D_SE2 = D_SE2_fcst, D_pre_SE1 = D_pre_SE1_fcst)
  
  # Extract the mcmc samples of the fitted reproduction number for the sample period beyond the estimation period.
  R_fcst <- temp$R
  
  # Extract the mcmc samples of the decomposition of fitted reproduction number for the sample period beyond the estimation period.
  R_fcst_decomp <- temp$comp_effect
  
  
  # Compute the mean and the percentile values of time-varying parameters in vectors.
  
  name_list<-c("R_fcst") # Variable name in the stan file.
  
  for (i in 1:length(name_list)){
    
    eval(parse(text=paste0("temp <- Extract_mean_ptl(",name_list[i],", ptl=c(0.025,0.975))")))
    eval(parse(text=paste0(name_list[i],"_pm <- temp[[1]]"))) # posterior mean
    eval(parse(text=paste0(name_list[i],"_025 <- temp[[2]]"))) # 5% percentile
    eval(parse(text=paste0(name_list[i],"_975 <- temp[[3]]"))) # 97.5% percentile
  }
  
  
  
  # Define the dependent variable.
  # The dependent variable is the rate of change in new cases over a week. It starts from 2020 March 1. The fitted value is available from 2020 March 1.
  DEP <- RoC_newcases_data[1:(which(RoC_newcases_date==weath_end_date_allsmpl)+1)] 
  
  
  
  # Set the date label for a plot.
  # If the dependent variable is the rate of change in the number of new cases from 7 days ago:  
  # Define the labels for dates starting from 2020 March 1, to the end of the available sample period for explanatory variables. The last date of fitted values of R is one day after the end of the available sample period for explanatory variables.
  date_label_fcst <- seq(as.Date("2020-03-01"),as.Date(weath_end_date_allsmpl)+1,by="day")

  
  # Set labels for the plot.
  # The dependent variable is the rate of change in the number of new cases from 7 days ago.
    
  if (ENG==1){
    if (plot_paper==1){
      temp_main_label<- ""
      temp_ylab <- "Log difference over 7 days in the number of new confirmed cases" 
    }else{
      temp_main_label <- "Log difference over 7 days in the number of new confirmed cases"
      temp_ylab <- "Log" 
    }
  }else{
    temp_main_label <- "新規陽性者数のログの前週差"
  }
  

  if (ENG==1){
    temp_xlab <- "Dates"
  }else{
    temp_xlab <- "日次"
  }
  
  # Plot the out-of-sample forecasts of the dependent variable with fitted values with and without time dummies.
  # The credible intervals of fitted values without dummies have a wider y axis range. Use the same range to plot fitted values with dummies.
  # For this, run the code from j=2 to j=1.
  
  for (j in 2:1){
    
    # The posterior mean and 95% credible interval for the fitted reproduction number for the estimation period.
    # Choose fitted values between those with and without time dummies. 
    if(j == 1){
      # Set fitted values with time dummies.
      temp_fitted_R_pm <- Fitted_R_pm
      temp_fitted_R_025 <- Fitted_R_025 
      temp_fitted_R_975 <- Fitted_R_975
    }else{
      # Extract fitted values without time dummies.
      temp <- Rec_R_fitted[[2]]
      temp_fitted_R_pm <- temp[[1]]
      temp_fitted_R_025 <- temp[[2]]
      temp_fitted_R_975 <- temp[[3]]
    }
    
    # Fill NAs for the period for out-of-sample forecasts.
    Fitted_R_pm_plot <- c(temp_fitted_R_pm, R_fcst_pm*NA)
    Fitted_R_025_plot <- c(temp_fitted_R_025, R_fcst_025*NA) 
    Fitted_R_975_plot <- c(temp_fitted_R_975, R_fcst_975*NA)
    
    # The posterior mean and 95% credible interval for the fitted reproduction number after the estimation period.
    R_fcst_pm_plot <- c(temp_fitted_R_pm*NA, R_fcst_pm)
    R_fcst_025_plot <- c(temp_fitted_R_pm*NA, R_fcst_025)
    R_fcst_975_plot <- c(temp_fitted_R_pm*NA, R_fcst_975)
    
    # Create a tibble for a time-series plot.
    temp_df <- tibble(Dates=date_label_fcst, DEP=DEP, Fitted_R_pm_plot=Fitted_R_pm_plot, Fitted_R_025=Fitted_R_025_plot, Fitted_R_975=Fitted_R_975_plot, R_fcst_pm_plot=R_fcst_pm_plot, R_fcst_025_plot=R_fcst_025_plot, R_fcst_975_plot=R_fcst_975_plot)
    
    # Set the legend label.
    if (ENG==1){
      lgnd=c("Observed value", "Fitted value", "Out-of-sample forecast")
    }else{
      lgnd=c("観測値", "回帰式のfitted valueの事後平均", "推計期間以降の予測値")
    }
    
    # Set viridis color palette.
    temp_viridis <- viridis(3) 
    
    # Plot the data frame by ggplot.  
    temp_p <- ggplot(temp_df, aes(x=Dates))+
      geom_line(aes(y=DEP, colour="DEP"))+
      geom_line(aes(y=Fitted_R_pm_plot, colour="Fitted_R_pm_plot"))+
      geom_line(aes(y=R_fcst_pm_plot, colour="R_fcst_pm_plot"))+
      scale_x_date(date_breaks = "2 month", date_labels = "%b%d\n%Y")+
      labs(title=temp_main_label, x=temp_xlab, y=temp_ylab, colour="")+
      scale_color_manual(values=temp_viridis,breaks=c("DEP","Fitted_R_pm_plot","R_fcst_pm_plot"),labels=lgnd)+
      theme(legend.position="bottom", plot.margin= margin(c(t=0, r=0.5, b=0, l=0.3), unit="cm"))+ # Without the adjustment of the margin, the x-axis label will not be within the frame of the figure.
      geom_ribbon(aes(ymin = Fitted_R_025_plot, ymax = Fitted_R_975_plot), fill=temp_viridis[2], alpha=0.2)+
      geom_ribbon(aes(ymin = R_fcst_025_plot, ymax = R_fcst_975_plot), fill=temp_viridis[3], alpha=0.2)
    
    if (j==1){
      # Set the same y-axis range as for j==2.
      temp_p <- temp_p + coord_cartesian(ylim = temp_yaxis_range) 
    }
    
    # Add shadows, arrows, and texts to indicate the periods of states of emergency.
    temp_p <- temp_p + annotate("rect",xmin=as.Date(Dates_SE_list[1]),xmax=as.Date(Dates_SE_list[2]),ymin=-Inf,ymax=Inf, alpha=.05,fill="blue")
    #temp_p <- temp_p + annotate("segment",x=as.Date(Dates_SE_list[1]),xend=as.Date(Dates_SE_list[2]),y=2,yend=2, arrow = arrow(ends = "both", angle = 45, length = unit(.2,"cm")))
    #temp_p <- temp_p + annotate("text",x=as.Date("2020-05-01"),y=2.2, label="1st SoE",size=3)
    temp_p <- temp_p + annotate("rect",xmin=as.Date(Dates_SE_list[3]),xmax=as.Date(Dates_SE_list[4]),ymin=-Inf,ymax=Inf, alpha=.05,fill="blue")
    #temp_p <- temp_p + annotate("segment",x=as.Date(Dates_SE_list[3]),xend=as.Date(Dates_SE_list[4]),y=2,yend=2, arrow = arrow(ends = "both", angle = 45, length = unit(.2,"cm")))
    #temp_p <- temp_p + annotate("text",x=as.Date("2021-02-15"),y=2.2, label="2nd SoE",size=3)
    temp_p <- temp_p + annotate("rect",xmin=as.Date(Dates_SE_list[5]),xmax=as.Date(Dates_SE_list[6]),ymin=-Inf,ymax=Inf, alpha=.05,fill="blue")
    #temp_p <- temp_p + annotate("segment",x=as.Date(Dates_SE_list[5]),xend=as.Date(Dates_SE_list[6]),y=2,yend=2, arrow = arrow(ends = "both", angle = 45, length = unit(.2,"cm")))
    #temp_p <- temp_p + annotate("text",x=as.Date("2021-05-25"),y=2.2, label="3rd SoE",size=3)
    temp_p <- temp_p + annotate("rect",xmin=as.Date(Dates_SE_list[7]),xmax=as.Date(Dates_SE_list[8]),ymin=-Inf,ymax=Inf, alpha=.05,fill="blue")
    ##temp_p <- temp_p + annotate("segment",x=as.Date(Dates_SE_list[7]),xend=as.Date(Dates_SE_list[8]),y=2,yend=2, arrow = arrow(ends="both",angle = 45, length = unit(.2,"cm")))
    ##temp_p <- temp_p + annotate("text",x=as.Date("2021-08-25"),y=2.2, label="4th SoE",size=3)

    
    # Print the plot.
    print(temp_p)
    
    if (j==2){
      # Keep the y axis range for j==2 to use it for j==1.
      temp_yaxis_range <- layer_scales(temp_p)$y$range$range
    }
  }
  
  # Compute RMSE^2 from the beginning of the prediction period and the end of June.
  temp <- DEP[(length(Fitted_R_pm)+1):which(date_label_fcst=="2021-06-30")] - R_fcst_pm[1:(which(date_label_fcst=="2021-06-30")-which(date_label_fcst=="2021-02-01"))] # The out-of-sample forecast error.
  
  RMSE_fcst <- sum(temp^2) / length(temp)
  
  R2_fcst <- 1 - sum(temp^2) / sum((DEP[length(Fitted_R_pm)+1:length(temp)] - mean(DEP[length(Fitted_R_pm)+1:length(temp)]))^2)
  
  # Record R^2 for the prediction period.
  sink(paste0("R2_pred_Covid_regress_model",mdl_number,"_weathertype",log_abs_hum,"_exconlinecloth",exc_online_cloth,"_dummyfor2ndSE",fcst_td,"_upto_June2021.txt"))
  print(paste0("RMSE: ", RMSE_fcst))
  print(paste0("R2: ", R2_fcst))  
  sink()
  
}





######## Common block for plotting the level decomposition of the fitted value of the reproduction number for the prediction period. ######


# Plot each component of the decomposition, including both effects via no coefficient dummies and the coefficient dummies of absolute humidity, in one diagram.
# Define the name of each component of decomposition.
if (ENG==1){
  lgnd <- c("Meals at bars and restaurants", "Soft drink, confectionery, and fruits at bars and restaurants", "Alcoholic drink at bars and restaurants", "Non-packaged lodging", "Domestic travel packages", "Clothing and footwear", "Admissions, viewing, game fees")
}else{
  lgnd <- c("食事代", "喫茶代", "飲酒代", "宿泊料", "国内パック旅行費", "被服及び履物", "入場・観覧・ゲーム代")
}
if (ENG==1){
  lgnd <- c(lgnd, "The other household consumption expenditures") 
}else{
  lgnd <- c(lgnd, "その他の家計消費支出")
}

if (log_abs_hum %in% c(1,3)){
  # Weather data are the level of temperature or a dummy that temperature exceeds a threshold.
  if (ENG==1){
    lgnd <- c(lgnd, "Mobility in public transportation", "Outside temperature", "Constant term + Time dummies") 
  }else{
    lgnd <- c(lgnd, "人出（公共交通）", "外気温", "定数項・時間ダミー")
  }
}else{
  # Weather data are absolute humidity.
  if (ENG==1){
    lgnd <- c(lgnd, "Mobility in public transportation", "Absolute humidity", "Constant term + Time dummies") 
  }else{
    lgnd <- c(lgnd, "人出（公共交通）", "絶対湿度", "定数項・時間ダミー")
  }
}


# Locate Clothing and footwear and Admissions, viewing, game fees in the name labels to swap each other's location.
if (ENG==1){
  loc_CandF <- which(lgnd=="Clothing and footwear") # Record the original location of clothing and footwear.
  loc_ACGF <- which(lgnd=="Admissions, viewing, game fees") # Record the original location of admission, viewing, game fees.
  lgnd[loc_ACGF] <- "Clothing and footwear" # Swap the labels.
  lgnd[loc_CandF] <- "Admissions, viewing, game fees" # Swap the labels.
}else{
  loc_CandF  <- which(lgnd=="被服及び履物") # Record the original location of clothing and footwear.
  loc_ACGF <- which(lgnd=="入場・観覧・ゲーム代") # Record the original location of admission, viewing, game fees.
  lgnd[loc_ACGF] <- "被服及び履物" # Swap the labels.
  lgnd[loc_CandF] <- "入場・観覧・ゲーム代" # Swap the labels.
}




######## Plot the level decomposition of the dependent variable. ######

# Define the location of the end of date for the decomposition of out-of-sample forecasts.
#End_date_decomp_fcst <- "2021/6/30"
End_date_decomp_fcst <- as.character(as.Date(weath_end_date_allsmpl)+1)



for (j in 1:4){
  
  if (j == 1){
    
    # Decomposition of out-of-sample forecasts.
    
    if (hes_end[1]+(hes_end[2]-1)/12 == hes_end_all[1]+(hes_end_all[2]-1)/12){
      # No out-of-sample forecasts in this case.
      break
    }
    
    # Define the labels for dates starting from one day after the end of the estimation period to the end of the available sample period. The last date of fitted values of R is one day after the end of the available sample period.
    date_label <- seq(as.Date(weath_end_date_estimation)+2, as.Date(End_date_decomp_fcst), by="day")
    
    # Define the matrix that contains decomposition.
    temp_mat <- R_fcst_decomp[,1:length(date_label)]
    
  }else if (j==2){
    
    # Extract the decomposition for fitted values.
    R_fitted_decomp <- Rec_R_decomp[[1]]
    
    # Decomposition for the period between August 2020 and the end of the Go-To-Travel campaign. 
    date_label <- seq(as.Date("2020-08-01"), as.Date("2020-12-27"), by="day")
    
    # Define the matrix that contains decomposition.
    # The start date of the dependent variable is March 1, 2020.
    temp_mat <- R_fitted_decomp[, (which(RoC_newcases_date=="2020/8/1")):which(RoC_newcases_date=="2020/12/27")]
  }else{ # if(j %in% c(3,4))
    
    # Extract the decomposition for fitted values.
    if (j==3){
      R_fitted_decomp <- Rec_R_decomp[[1]] # With time dummies.
    }else{
      R_fitted_decomp <- Rec_R_decomp[[2]] # Without time dummies.
    }

    # Decomposition of all the fitted values.
    
    # Define the matrix that contains decomposition.
    # Define the labels for dates starting from one day after the end of the estimation period to the end of the available sample period. The last date of fitted values of R is one day after the end of the available sample period.
    date_label <- seq(as.Date("2020-03-01"), as.Date("2021-02-01"), by="day")
    # The start date of the dependent variable is March 1, 2020.
    temp_mat <- R_fitted_decomp[, (which(RoC_newcases_date=="2020/3/1")):which(RoC_newcases_date=="2021/2/1")]
  }
  
  
  # Constant and absolute humidity dummies, which are in the second row of the matrix on the right-hand side.
  decomp <- tibble(Dates=date_label, CSEAH_1=temp_mat[1,], CSEAH_2=temp_mat[2,])
  # Difference from the benchmark date (the first element).
  diff_decomp <- tibble(Dates=date_label, CSEAH_1=temp_mat[1,]-temp_mat[1,1], CSEAH_2=temp_mat[2,]-temp_mat[2,1])
  # Initialize tibbles that record the decomposition of the difference from the benchmark date separately among baseline effects and cross-term effects.
  decomp_base <- decomp_AH <- decomp_SE <- diff_decomp_base <- diff_decomp_AH <- diff_decomp_SE <- tibble(Dates=date_label)
  
  # Initialize the record of variable names in tibble.
  var_name_decomp <- NULL
  
  # Terms that include the absolute humidity coefficient dummies. 
  for (i in 1:(length(lgnd)-2)){
    # Sum the three components for each variable: basic coefficients without coefficient dummies, absolute humidity coefficient dummies, state of emergency coefficient dummies.
    temp_vi_AH <- temp_mat[2+i,] + temp_mat[2+i+(length(lgnd)-2),] # Without cross terms with time dummies.
    temp_vi <- temp_mat[2+i,] + temp_mat[2+i+(length(lgnd)-2),] + temp_mat[2+i+(length(lgnd)-2)*2,] # With all cross terms.
    # Set the benchmark date to compute changes in the contributions from each explanatory variable.
    if (j %in% c(3,4)){
      temp_bchmk_date <- length(temp_vi)
    }else{
      temp_bchmk_date <- 1
    }  
    # Record the sum in a tibble.
    eval(parse(text=paste0("decomp <- tibble(decomp, v", i, "=temp_vi)")))
    # Record the baseline effect of each explanatory variable.
    eval(parse(text=paste0("decomp_base <- tibble(decomp_base, v", i, "=temp_mat[2+i,])")))
    # Record the baseline effect and the cross term with the absolute humidity dummy for each explanatory variable.
    eval(parse(text=paste0("decomp_AH <- tibble(decomp_AH, v", i, "=temp_mat[2+i,]+temp_mat[2+i+(length(lgnd)-2),])")))
    # Difference from the benchmark date (the first element).
    eval(parse(text=paste0("diff_decomp <- tibble(diff_decomp, v", i, "=temp_vi-temp_vi[temp_bchmk_date])")))
    # Difference from the benchmark date (the first element): baseline effect
    eval(parse(text=paste0("diff_decomp_base <- tibble(diff_decomp_base, v", i, "=temp_mat[2+i,]-temp_mat[2+i,temp_bchmk_date])")))
    # Difference from the benchmark date (the first element): cross term with absolute humidty dymmy
    eval(parse(text=paste0("diff_decomp_AH <- tibble(diff_decomp_AH, v", i, "=temp_mat[2+i+(length(lgnd)-2),]-temp_mat[2+i+(length(lgnd)-2),temp_bchmk_date])")))
    # Difference from the benchmark date (the first element): cross term with state of emergency dummy
    eval(parse(text=paste0("diff_decomp_SE <- tibble(diff_decomp_SE, v", i, "=temp_mat[2+i+(length(lgnd)-2)*2,]-temp_mat[2+i+(length(lgnd)-2)*2,temp_bchmk_date])")))
    
    # Record variable names in tibble.
    eval(parse(text=paste0("var_name_decomp <- c(var_name_decomp, \"v",i,"\")")))
  }
  
  # Swap the order of Clothing and footwear and Admissions, viewing, game fees in var_name_decomp and lgnd.
  eval(parse(text=paste0("var_name_decomp[loc_CandF] <- \"v", loc_ACGF,"\"")))
  eval(parse(text=paste0("var_name_decomp[loc_ACGF] <- \"v", loc_CandF,"\"")))
  
  
  # Record variable names in tibble.
  var_name_decomp <- c(var_name_decomp, "CSEAH_2","CSEAH_1")
  
  # Plot the figures.
  for (i in 1:5){
    # First plot: level of each component.
    # Second plot: Changes in each component from a benchmark date.
    # Third plot: Changes in the baseline effect of each component from a benchmark date.
    # Fourth plot: Changes in the cross effect of absolute dummy on each component from a benchmark date.
    # Fifth plot: Changes in the cross effect of SE dummies on each component from a benchmark date.
    
    # Set labels in the plot.
    if (i==1){
      temp_ylab <- ""
    }else{
      temp_ylab <-  paste0("Differences from ", format(date_label[temp_bchmk_date], "%b. %d %Y"))
    }
    
    if (ENG==1){
      
      if (plot_paper==1){
        temp_main_label <- ""
      }else{
        if (i ==1){
          temp_main_label <- "Contributions from explanatory variables to the dependent variable"
        }else if (i==2){
          temp_main_label <- paste0("Contributions to changes in the dependent variable from ", format(date_label[1], "%Y %b. %d"))
        }else if (i==3){
          temp_main_label <- "Contributions to changes in the dependent variable: baseline effect"
        }else if (i==4){
          temp_main_label <- "Contributions to changes in the dependent variable: cross term with abs. hum. dummy"
        }else{
          temp_main_label <- "Contributions to changes in the dependent variable: cross term with state of emergency dummies"
        }
      }
    }else{
      if (i ==1){
        temp_main_label <- "被説明変数への各説明変数の寄与"
      }else if (i==2){
        temp_main_label <- paste0(format(date_label[1], "%Y %b. %d"), "からの被説明変数の変化分への各説明変数の寄与 (", format(date_label[1], "%Y %b. %d"), " = 0)")
      }else if (i==3){
        temp_main_label <- "被説明変数の変化分への各説明変数の寄与: 基本効果"
      }else if (i==4){
        temp_main_label <- "被説明変数の変化分への各説明変数の寄与: 絶対温度ダミーとの交差項"
      }else{
        temp_main_label <- "被説明変数の変化分への各説明変数の寄与: 緊急事態宣言ダミーとの交差項"
      }
    }
    
    if (ENG==1){
      temp_xlab <- "Dates"
    }else{
      temp_xlab <- "日次"
    }
    
    
    # Set the tibble to plot.
    if (i==1){
      # Levels of contributions from explanatory variables.
      # These series are plotted in the same panel for each explanatory variable.
      temp_df <-  decomp
    }else if(i==2){
      # Changes from the benchmark date
      temp_df <-  diff_decomp
    }else if(i==3){
      # Changes from the benchmark date: baseline effect.
      temp_df <-  diff_decomp_base
    }else if(i==4){
      # Changes from the benchmark date: cross term with absolute humidity dummy.
      temp_df <-  diff_decomp_AH
    }else{
      # Changes from the benchmark date: cross term with state of emergency dummies.
      temp_df <-  diff_decomp_SE
    }
    
    if (i == 1 || (i == 2 && j %in% c(3,4))){
      # i == 1: Level decomposition.
      # i == 2 & j == 3: Decomposition of changes in fitted values.
      # i == 2 & j == 4: Decomposition of changes in fitted values without SE dummies (but with new year dummy).
    
      # Set labels for explanatory variables that correspond to variables in the tibble.
      temp_lgnd_select <- lgnd
      if (i == 1 && j %in% c(1,2)){
        temp_lgnd_select[length(temp_lgnd_select)] <- "Constant term" # No time dummies for j = 1 and j = 2.
      }
      
      if (j == 4){
        temp_lgnd_select[length(temp_lgnd_select)] <- "Constant term + The year-end and new-year holiday dummy" # No SE dummies for j = 4.
      }

      # A as_labeller function requires the correspondence between labels and variables.
      names(temp_lgnd_select) <- var_name_decomp 
      
      # Define the labeller function.
      temp_facet_label <- as_labeller(temp_lgnd_select, default=label_wrap_gen(20))
      
    }else if (i == 2 && j %in% c(1,2)){
      # i == 2 & j == 1: Decomposition of changes in out-of-sample forecasts.
      # i == 2 & j == 2: Decomposition of changes in fitted values for a sub-sample period of August-December 2020.
      
      # Changes from the benchmark date.
      temp_df <-  select(temp_df, -CSEAH_1) # No time dummies, so the constant and time dummies term does not change its value. 
      
      # Set labels for explanatory variables that correspond to variables in the tibble.
      temp_lgnd_select <- lgnd[1:(length(lgnd)-1)]
      
      # A as_labeller function requires the correspondence between labels and variables.
      names(temp_lgnd_select) <- var_name_decomp[1:(length(var_name_decomp)-1)]
      
      # Define the labeller function.
      temp_facet_label <- as_labeller(temp_lgnd_select, default=label_wrap_gen(20))
    
    }else{
      # Separate decomposition of changes in out-of-sample forecasts or fitted values.
      # Set labels for explanatory variables that correspond to variables in the tibble.
      # Exclude constant (including state of emergency) and absolute humidity dummy.
      temp_lgnd_select <- lgnd[1:(length(lgnd)-2)]
      # A as_labeller function requires the correspondence between labels and variables.
      names(temp_lgnd_select) <- var_name_decomp[1:(length(lgnd)-2)]
      # Define the labeller function.
      temp_facet_label <- as_labeller(temp_lgnd_select, default=label_wrap_gen(20))
    }
    
    
    # Convert a matrix of Dates x variables into a vector with columns for Dates and variables. 
    temp_df <-  gather(temp_df, key = variable, value = value, -Dates)
    
    if (i==1){
      # Additional data to be plotted together with decomp in the same panel for each explanatory variable.
      temp_df_base <-  gather(decomp_base, key = variable, value = value_base, -Dates)
      temp_df_AH <-  gather(decomp_AH, key = variable, value = value_AH, -Dates)
      # Merge three tibbles for each explanatory variable.
      temp_df <- full_join(temp_df,temp_df_base, by=c("Dates","variable"))
      temp_df <- full_join(temp_df,temp_df_AH, by=c("Dates","variable"))
    }
    
    # Set the order of variables in the melted tibble for plotting.
    temp_df$variable <- factor(temp_df$variable, levels = names(temp_lgnd_select))
    
    # Set viridis color palette.
    temp_viridis <- viridis(3) 

    # Draw the plot.
    if(i==1){
      # For the decomposition of contributions from each explanatory variable to the dependent variable, the total contribution and the baseline contribution are plotted in different colors.
      temp_p <- ggplot(temp_df, aes(x=Dates, y=value, color="value"))
    }else if (i==2){
      # Only one series will be plotted in each panel; The color is the same as that of the total contribution for the level decomposition (i==1).
      temp_p <- ggplot(temp_df, aes(x=Dates, y=value, color=temp_viridis[2]))
    }else{
      # Only one series will be plotted in each panal; so it is unnecessary to specify the color of each plot.
      temp_p <- ggplot(temp_df, aes(x=Dates, y=value))
    }
    
    # Decomposition of total contributions from each explanatory variable to the dependent variable.
    temp_p <- temp_p + geom_line() +
      labs(title=temp_main_label, x=temp_xlab, y=temp_ylab)+
      facet_wrap(~ variable, nrow = 3, labeller=temp_facet_label)
    
    if(i==1){
      
      if (mdl_number==7){
        # The baseline effect of each explanatory variable for the dependent variable.
        # Plot this line only if there are weather data among the regressors, because the line is identical with total contribution.
        temp_p <- temp_p + geom_line(aes(y=value_base,color="value_base"))
      }
      
      if (j == 2){
        # Shift the entire plot to the left to write all the x-axis labels.
        temp_p <- temp_p + theme(legend.position="bottom", plot.margin= margin(c(t=0, r=0.3, b=0, l=-0.3), unit="cm"))
        #temp_p <- temp_p + theme(legend.position="bottom")
      }else{
        #temp_p <- temp_p + theme(legend.position="bottom", plot.margin= margin(c(t=0, r=0.3, b=0, l=-0.3), unit="cm"))
        temp_p <- temp_p + theme(legend.position="bottom")
      }
      
      if (j == 3){
        # The contributions through the baseline effect and the cross term with absolute humidity dummy differ from the total contribution.
        temp_p <- temp_p + geom_line(aes(y=value_AH,color="value_AH"))
        if (mdl_number==7){
          # There is a cross term with absolute humidity dummy.
          if (log_abs_hum %in% c(1,3)){
            # Weather data are temperature data.  
            temp_p <- temp_p + scale_color_manual(name="",breaks=c("value_base","value_AH","value"),labels=c("W/o cross effects of temperature\n or state-of-emergency dummies", "W/o cross effects of\n state-of-emergency dummies", "Total contribution"),values=temp_viridis[c(1,3,2)]) # Keep the color of the total contribution the same across plots.
          }else{
            # Weather data are absolute humidity.
            temp_p <- temp_p + scale_color_manual(name="",breaks=c("value_base","value_AH","value"),labels=c("W/o cross effects of absolute humidity\n or state-of-emergency dummies", "W/o cross effects of\n state-of-emergency dummies", "Total contribution"),values=temp_viridis[c(1,3,2)]) # Keep the color of the total contribution the same across plots.
          }
        }else{
          # There is no cross term with absolute humidity dummy.
          temp_p <- temp_p + scale_color_manual(name="",breaks=c("value_AH","value"),labels=c("W/o cross effects of\n state-of-emergency dummies", "Total contribution"),values=temp_viridis[c(3,2)]) # Keep the color of the total contribution the same across plots.
        }
      }else{
        if (mdl_number==7){
          # No time dummies for the plot. Only the contributions through the baseline effect and the cross term with absolute humidity dummy must be separated.
          if (log_abs_hum %in% c(1,3)){
            # Weather data are temperature data.  
            temp_p <- temp_p + scale_color_manual(name="",breaks=c("value_base","value"),labels=c("W/o cross effect of outside temperature", "Total contribution"),values=temp_viridis[1:2])
          }else{
            # Weather data are absolute humidity.
            temp_p <- temp_p + scale_color_manual(name="",breaks=c("value_base","value"),labels=c("W/o cross effect of absolute humidity", "Total contribution"),values=temp_viridis[1:2])
          }
        }else{
          # No time dummies for the plot. No cross term with absolute humidity dummy.
          temp_p <- temp_p + scale_color_manual(name="",breaks=c("value"),labels=c("Total contribution"),values=temp_viridis[2])
        }
      }
    
    }else if (i==2){
      if (j == 2){
        temp_p <- temp_p + 
          theme(legend.position = "bottom", plot.margin= margin(c(t=0, r=0.3, b=0, l=0), unit="cm"))
      }else{
        temp_p <- temp_p + 
          theme(legend.position = "bottom")
      }
      temp_p <- temp_p + 
        scale_color_manual(name="",labels="Total contribution",values=temp_viridis[2])
    }
    
    # Set the interval of date labels and annotate shadows in the plot.
    if (j == 1){
      temp_p <-  temp_p + scale_x_date(date_breaks = "2 months", date_labels = "%b%d\n%Y") +
        theme(axis.text.x=element_text(size = 8)) # Make the font size of x tick labels smaller to prevent the date labels from overlapping with each other.
      
      # Add shadows to indicate the periods of states of emergency.
      temp_p <- temp_p + annotate("rect",xmin=as.Date("2021-02-02"),xmax=as.Date(Dates_SE_list[4]),ymin=-Inf,ymax=Inf, alpha=.1,fill="blue")
      temp_p <- temp_p + annotate("rect",xmin=as.Date(Dates_SE_list[5]),xmax=as.Date(Dates_SE_list[6]),ymin=-Inf,ymax=Inf, alpha=.1,fill="blue")
      temp_p <- temp_p + annotate("rect",xmin=as.Date(Dates_SE_list[7]),xmax=as.Date(Dates_SE_list[8]),ymin=-Inf,ymax=Inf, alpha=.1,fill="blue")

    }else if (j %in% c(3,4)){
      temp_p <-  temp_p + scale_x_date(date_breaks = "3 months", date_labels = "%b%d\n%Y")
      
      # Add shadows to indicate the periods of states of emergency.
      temp_p <- temp_p + annotate("rect",xmin=as.Date(Dates_SE_list[1]),xmax=as.Date(Dates_SE_list[2]),ymin=-Inf,ymax=Inf, alpha=.1,fill="blue")
      temp_p <- temp_p + annotate("rect",xmin=as.Date(Dates_SE_list[3]),xmax=as.Date("2021-02-01"),ymin=-Inf,ymax=Inf, alpha=.1,fill="blue")
      
    }else{
      temp_p <-  temp_p + scale_x_date(date_breaks = "2 months", date_labels = "%b%d\n%Y")
    }
    
    # Print the plot.
    print(temp_p)
    
  }
  
}



####### Plot the comparison among 2019, 2020, 2021 data for each household expenditure item ##########

# Define labels for each plot.
if (ENG==1){
  temp_xlab <- "Dates"
  if (nominal_hes==1){
    temp_ylab <- "100 yen in the current price"
  }else{
    temp_ylab <- "100 yen in the 2020 average price"
  }
}else{
  temp_xlab <- "日次"
  if (nominal_hes==1){
    temp_ylab <- "100 円（名目、一家計当たり平均）"
  }else{
    temp_ylab <- "100 円（実質、2020年基準、一家計当たり平均）"
  }
}

# Plot the level and the moving average.
for (k in 1:2){
  
  if (plot_paper==1){
    temp_main_label =""
  }else if (k==1){
    if (ENG==1){
      temp_main_label ="Comparison of household expenditures for 2019-2021"
    }else{
      temp_main_label ="各年の家計支出の比較"
    }
  }else{
    if (ENG==1){
      temp_main_label ="Comparison of household expenditures for 2019-2021 (7-day moving average)"
    }else{
      temp_main_label ="各年の家計支出の比較 (7日間移動平均）"
    }
  }
  
  # Initialize a tibble to contain data for a plot.
  # Only the month and the date in the date vector will be used to plot household expenditure on the same date for 2019-2021.
  temp_df_2019 <- temp_df_2020 <- temp_df_2021 <- tibble(Dates=seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by="day"))
  
  # Plot each household expenditure.
  # lgnd contains household expenditures and mobility in public transportation, absolute humidity, and constant and time dummies.
  for (i in 1:(length(lgnd)-3)){
    
    # Extract household expenditure for 2021.
    temp_hes_2021 <- H_expvals_all[i, (sum(ndays_olympic)+1):ncol(H_expvals_all)]
    # Fill NAs in the rest of 2021.
    temp_hes_2021 <- c(temp_hes_2021, rep(NA,365-length(temp_hes_2021)))
    # Extract household expenditure for 2020.
    temp_hes_2020 <- H_expvals_all[i, 1:sum(ndays_olympic)]
    # Remove the leap day in 2020.
    temp_hes_2020 <- temp_hes_2020[-sum(ndays_olympic[1:2])]
    # Extract household expenditure for 2019.
    temp_hes_2019 <- H_expvals_2019[i,]
    
    # Add the extracted household expenditure data to the tibble.
    for (j in 2019:2021){
      if (k==1){
        # Level.
        eval(parse(text=paste0("temp_df_",j," <- tibble(temp_df_",j,", v",i,"=temp_hes_",j,")")))
      }else{
        # Moving average.
        eval(parse(text=paste0("temp_df_",j," <- tibble(temp_df_",j,", v",i,"=stats::filter(temp_hes_",j,",rep(1/7,7)))")))
      }
    }
  } 
  
  # Melt household expenditure data in each year for a plot.
  for (j in 2019:2021){
    eval(parse(text=paste0("temp_df_",j," <- gather(temp_df_",j,", key=variable, value = value_",j,", -Dates)")))
  }
  
  # Merge the tibbles for household expenditures in each year.
  temp_df <- full_join(temp_df_2019,temp_df_2020, by=c("Dates","variable"))
  temp_df <- full_join(temp_df,temp_df_2021, by=c("Dates","variable"))
  
  # Exclude constant and time dummies, absolute humidity dummy, and mobility in public transportation from the name label.
  temp_lgnd_select <- lgnd[1:(length(lgnd)-3)]
  # A as_labeller function requires the correspondence between labels and variables.
  # var_name_decomp is already reordered to put Admissions, viewing, game fees before Clothing and footwear.
  names(temp_lgnd_select) <- var_name_decomp[1:(length(lgnd)-3)]
  # Define the labeller function.
  temp_facet_label <- as_labeller(temp_lgnd_select, default=label_wrap_gen(20))
  
  
  # Set the order of variables in the melted tibble for plotting.
  temp_df$variable <- factor(temp_df$variable, levels = names(temp_lgnd_select))
  
  # Set viridis color palette.
  temp_viridis <- viridis(3) 
  
  # Draw the plot.
  temp_p <- ggplot(temp_df, aes(x=Dates, y=value_2019,color="value_2019"))+
    geom_line()+
    geom_line(aes(y=value_2020,color="value_2020"))+
    geom_line(aes(y=value_2021,color="value_2021"))+
    labs(title=temp_main_label, x=temp_xlab, y=temp_ylab)+
    facet_wrap(~ variable, nrow = 3, labeller=temp_facet_label, scales="free_y")+
    theme(legend.position="bottom")+
    scale_color_manual(name="",breaks=c("value_2019","value_2020","value_2021"),labels=c("2019","2020","2021"),values=temp_viridis)+
    scale_x_date(date_breaks = "3 months", date_labels = "%b%d")
  
  # Print the plot.
  print(temp_p)
  
}




########### Plot real and nominal household expenditures, and mobility in public transportation. ###############

# Initialize a tibble to contain data for a plot.
# temp_df_real: real household expenditures.
# temp_df_nom: nominal household expenditures.
temp_df_real <- temp_df_nom <- tibble(Dates=as.Date(hes_var_date_all))

# Add each type of household expenditure to tibbles.
for (i in 1:nrow(H_expvals)){
  
  eval(parse(text=paste0("temp_df_real <- tibble(temp_df_real, v",i,"=H_expvals_all[i,])")))
  eval(parse(text=paste0("temp_df_nom <- tibble(temp_df_nom, v",i,"=H_expvals_all_nominal[i,]/100)"))) # Divide nominal household expenditures to set the unit to 100 yen.
  
}

# Melt household expenditure data in each year for a plot.
temp_df_real <- gather(temp_df_real, key=variable, value = Real, -Dates)
temp_df_nom <- gather(temp_df_nom, key=variable, value = Nominal, -Dates)

# Merge the tibbles for real and nominal household expenditures.
temp_df_hes <- full_join(temp_df_real,temp_df_nom, by=c("Dates","variable"))


for (j in 1:3){
  # j = 1: Real household expenditures.
  # j = 2: Nominal household expenditures.
  # j = 3: Mobility in public transportation.
  
  if (j < 3){
    
    if (j == 1 && nominal_hes==1){
      # No computation of real household expenditures.
      break
    }
    
    ### Plot household expenditures.
    
    # Set the tibble to plot.
    temp_df <- temp_df_hes
    
    # Define labels for each plot.
    if (ENG==1){
      temp_xlab <- "Dates"
      if (j == 1){
        temp_ylab <- "100 yen in the 2020 average price"
      }else{
        temp_ylab <- "100 yen in the current price"
      }
    }else{
      temp_xlab <- "日次"
      if (j == 1){
        temp_ylab <- "100 円（実質、2020年基準、一家計当たり平均）"
      }else{
        temp_ylab <- "100 円（名目、一家計当たり平均）"
      }
    }
    
    if (plot_paper==1){
      temp_main_label =""
    }else{
      if (ENG==1){
        temp_main_label ="Classification of household expenditures"
      }else{
        temp_main_label ="家計支出の分類"
      }
    }
    
    
    # Exclude constant and time dummies, absolute humidity dummy, and mobility in public transportation from the name label.
    temp_lgnd_select <- lgnd[1:(length(lgnd)-3)]
    # A as_labeller function requires the correspondence between labels and variables.
    # var_name_decomp is already reordered to put Admissions, viewing, game fees before Clothing and footwear.
    names(temp_lgnd_select) <- var_name_decomp[1:(length(lgnd)-3)]
    # Define the labeller function.
    temp_facet_label <- as_labeller(temp_lgnd_select, default=label_wrap_gen(20))
    
    # Set the order of variables in the melted tibble for plotting.
    temp_df$variable <- factor(temp_df$variable, levels = names(temp_lgnd_select))
    
    # Set viridis color palette.
    temp_viridis <- viridis(2) 
    
    
    # Draw the plot.
    if (j == 1){
      # Real household expenditures.
      temp_p <- ggplot(temp_df, aes(x=Dates, y=Real))
    }else{
      # Nominal household expenditures.
      temp_p <- ggplot(temp_df, aes(x=Dates, y=Nominal))
    }
    
    temp_p <- temp_p + geom_line()+
      #geom_line(aes(y=Nominal,color="Nominal"))+
      labs(title=temp_main_label, x=temp_xlab, y=temp_ylab)+
      facet_wrap(~ variable, nrow = 3, labeller=temp_facet_label, scales="free_y")+
      #theme(legend.position="bottom")+
      #scale_color_manual(name="",breaks=c("Real","Nominal"),labels=c("Real","Nominal"),values=temp_viridis)+
      scale_x_date(date_breaks = "4 months", date_labels = "%b%d\n%Y")
    
  }else{
    
    ### j == 3. Plot mobility in public transportation.
    
    if (ENG==1){
      temp_xlab <- "Dates"
      
      temp_ylab <- "% change from the average over Jan. 3 2020 - Feb. 6 2020"
      if (plot_paper==1){
        temp_main_label <- ""
      }else{
        temp_main_label <- "Mobility in public transportation"
      }
    }else{
      temp_xlab <- "日次"
      temp_ylab <- "2020/1/3-2020/2/6の平均からの変化率"
      temp_main_label <- "人出（公共交通）"
    }
    
    # Create a tibble to contain data for a plot.
    temp_df <- tibble(Dates = as.Date(mob_date), value = mob_var4[,1])
    
    # Draw the plot.
    temp_p <- ggplot(temp_df, aes(x=Dates, y=value))+
      geom_line()+
      labs(title=temp_main_label, x=temp_xlab, y=temp_ylab)+
      scale_x_date(date_breaks = "3 months", date_labels = "%b%d\n%Y")
  }
  
  # Add shadows, arrows, and texts to indicate the periods of states of emergency.
  temp_p <- temp_p + annotate("rect",xmin=as.Date(Dates_SE_list[1]),xmax=as.Date(Dates_SE_list[2]),ymin=-Inf,ymax=Inf, alpha=.1,fill="blue")
  temp_p <- temp_p + annotate("rect",xmin=as.Date(Dates_SE_list[3]),xmax=as.Date(Dates_SE_list[4]),ymin=-Inf,ymax=Inf, alpha=.1,fill="blue")
  temp_p <- temp_p + annotate("rect",xmin=as.Date(Dates_SE_list[5]),xmax=as.Date(Dates_SE_list[6]),ymin=-Inf,ymax=Inf, alpha=.1,fill="blue")
  temp_p <- temp_p + annotate("rect",xmin=as.Date(Dates_SE_list[7]),xmax=as.Date(Dates_SE_list[8]),ymin=-Inf,ymax=Inf, alpha=.1,fill="blue")
  
  # if (j==2){
  #   temp_p <- temp_p + annotate("segment",x=as.Date(Dates_SE_list[1]),xend=as.Date(Dates_SE_list[2]),y=0,yend=0, arrow = arrow(ends = "both", angle = 45, length = unit(.2,"cm")))
  #   temp_p <- temp_p + annotate("text",x=as.Date("2020-05-01"),y=2, label="1st SoE",size=3)
  #   temp_p <- temp_p + annotate("segment",x=as.Date(Dates_SE_list[3]),xend=as.Date(Dates_SE_list[4]),y=0,yend=0, arrow = arrow(ends = "both", angle = 45, length = unit(.2,"cm")))
  #   temp_p <- temp_p + annotate("text",x=as.Date("2021-02-15"),y=2, label="2nd SoE",size=3)
  #   temp_p <- temp_p + annotate("segment",x=as.Date(Dates_SE_list[5]),xend=as.Date(Dates_SE_list[6]),y=0,yend=0, arrow = arrow(ends = "both", angle = 45, length = unit(.2,"cm")))
  #   temp_p <- temp_p + annotate("text",x=as.Date("2021-05-25"),y=2, label="3rd SoE",size=3)
  #   
  #   #temp_p <- temp_p + annotate("rect",xmin=as.Date(Dates_SE_list[7]),xmax=as.Date(Dates_SE_list[8]),ymin=-Inf,ymax=Inf, alpha=.05,fill="blue")
  #   #temp_p <- temp_p + annotate("segment",x=as.Date(Dates_SE_list[7]),xend=as.Date(Dates_SE_list[8]),y=2,yend=2, arrow = arrow(ends="both",angle = 45, length = unit(.2,"cm")))
  #   #temp_p <- temp_p + annotate("text",x=as.Date("2021-08-25"),y=2.2, label="4th SoE",size=3)
  #   
  #   temp_p <- temp_p + annotate("segment",x=as.Date("2021-09-01"),xend=as.Date(Dates_SE_list[7]),y=0,yend=0, arrow = arrow(angle = 45, length = unit(.2,"cm")))
  #   temp_p <- temp_p + annotate("text",x=as.Date("2021-08-07"),y=2, label="4th SoE",size=3)
  # }
  
  
  # Print the plot.
  print(temp_p)
  
}



########## Plot CPI indices and explanatory variables. ###################################


# Define labels for each plot.
if (ENG==1){
  temp_xlab <- "Months"
  temp_ylab <- "Index (the average price level in 2020 = 1)"
}else{
  temp_xlab <- "月次"
  temp_ylab <- "Index (2020年の平均価格 = 1)"
}

if (plot_paper==1){
  temp_main_label =""
}else{
  if (ENG==1){
    temp_main_label ="CPI"
  }else{
    temp_main_label ="消費者物価指数"
  }
}


# Define the list of the Japanese names of CPI_items.
if (ENG==1){
  CPI_name_list <- c("Food consumption at bars and restaurants in general", "Lodging", "Admissions, viewing, game fees", "Clothing and footwear")
}else{
  CPI_name_list <- c("一般外食","宿泊料（パック旅行に含まれるものも含む）","入場・観覧・ゲーム代", "被服及び履物")
}

# Add the label for the residual household expenditures.
if (ENG==1){
  CPI_name_list <- c(CPI_name_list, "The other household consumption expenditures")
}else{
  CPI_name_list <- c(CPI_name_list, "その他の家計消費支出")
}

# Create a tibble to plot CPI indices.
temp_df <- tibble(Dates = seq(as.Date("2020-01-01"), as.Date(paste0(hes_end_all[1],"-",hes_end_all[2],"-01")), by="month"))
                  
# Initialize the vector to record the name of variable for each type of household expenditure in the tibble.
var_name_decomp_CPI <- NULL 

# Add the CPI for each type of household expenditure to the tibble.
for (i in c(1,3:nrow(CPI_m_2020_rvs))){ # The second row of CPI_m_2020_rvs is for independent lodging, interpolating the data for the Go-To-Travel period in 2020.
  
  eval(parse(text=paste0("temp_df <- tibble(temp_df, v",i,"=CPI_m_2020_rvs[i,13:ncol(CPI_m_2020_rvs)])"))) # Add CPI.
  eval(parse(text=paste0("var_name_decomp_CPI <- c(var_name_decomp_CPI, \"v",i,"\")"))) # Record the corresponding variable name.

}

# Melt the tibble for CPI data for a plot.
temp_df <- gather(temp_df, key=variable, value = value, -Dates)

# A as_labeller function requires the correspondence between labels and variables.
# var_name_decomp is already reordered to put Admissions, viewing, game fees before Clothing and footwear.
names(CPI_name_list) <- var_name_decomp_CPI
# Define the labeller function.
temp_facet_label <- as_labeller(CPI_name_list, default=label_wrap_gen(30))

# Set the order of variables in the melted tibble for plotting.
temp_df$variable <- factor(temp_df$variable, levels = names(CPI_name_list))

# Draw the plot.
temp_p <- ggplot(temp_df, aes(x=Dates, y=value))+
  geom_line()+
  geom_point()+
  labs(title=temp_main_label, x=temp_xlab, y=temp_ylab)+
  #facet_wrap(~ variable, nrow = 2, labeller=temp_facet_label, scales="free_y")+
  facet_wrap(~ variable, nrow = 2, labeller=temp_facet_label)+
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y")

# Print the plot.
print(temp_p)



############### Estimate a logistic growth model for the L452R share of confirmed cases. ############

temp <- L452R_share_NW_w[1:which.max(L452R_share_NW_w)] # The first value to the peak.
temp_tt <- seq_along(temp) # The time period in the logistic growth model.
L452R_nls <- nls(temp ~ a / (1 + b*exp(-c*temp_tt)), start=list(a=1, b=10, c=1), trace=TRUE) # Estimate the logistic growth model.

### plot the L452R share of confirmed cases and the estimated logistic curve.
# Define a tibble for plotting.
# Define the labels for the weeks in the sample period.
temp_week_label <- R_date[which(R_date=="2021/5/31")+seq(0,365-sum(ndays_normal[1:5]),by=7)] # Use the date label for 2020 to create a vector of m/d in 2021 from May 31.
# Define a tibble for plotting.
temp_df <- tibble(Dates = seq(as.Date("2021-05-31"), as.Date(temp_week_label[length(temp)]), by="1 week"), value = temp, fit = predict(L452R_nls))

# Define axis labels.
if (ENG==1){
  temp_main_label ="Fitting a logistic growth model for the L452R share of confirmed cases"
  temp_xlab <- "First dates of weeks"
  temp_ylab <- "Share"
}else{
  temp_main_label ="デルタ株陽性率のロジスティック回帰モデルによるフィット"
  temp_xlab <- "各週の初日"
  temp_ylab <- "Share"
}

# Set the legend label.
if (ENG==1){
  temp_lgnd=c("Observed value", "Fitted value")
}else{
  temp_lgnd=c("観測値", "推計式のfitted value")
}

# Set viridis color palette.
temp_viridis <- viridis(2) 

# Plot the data frame by ggplot.  
temp_p <- ggplot(temp_df, aes(x=Dates))+
  geom_point(aes(y=value, colour="value"))+
  geom_point(aes(y=fit, colour="fit"))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b%d\n%Y")+
  labs(title=temp_main_label, x=temp_xlab, y=temp_ylab, colour="")+
  scale_color_manual(values=temp_viridis,breaks=c("value","fit"),labels=temp_lgnd)+
  theme(legend.position="bottom")

# Print the plot.
print(temp_p)
  
# Compute daily fitted values from the first sample of L452R share of confirmed cases to an arbitrary distant future date.
L452R_fitted_d <- predict(L452R_nls,list(temp_tt=1+c(0:500)/7))

# Add zeros for the dates between 2021-01-01 and 2021-06-02. L452R_share_w starts from the week of 2021-05-31. Use weekly data for the mid day of each week. So the first data is for 2021-06-03. 
L452R_fitted_d <- c(rep(0,sum(ndays_normal[1:5])+2), L452R_fitted_d)




##################### Plot the forecast error of the regression model for the out-of-sample forecast period. ##########################

# Compute the forecast error for the posterior means of regression coefficients. For the estimation period, R_fcst_pm_plot is zero.
fcst_err <- DEP - R_fcst_pm_plot

# Drop the values for the estimation period.
fcst_err <- fcst_err[which(date_label_fcst=="2021-02-02"):length(fcst_err)]

# Regress forecast error on the lagged vaccination rate 7 days ago and the L452R share of infection.
# Then plot the forecast error with the fitted values.
# Consider two measures of the L452R share of infection:
#  - the estimate by fitting a logistic growth model.
#  - the data reported by the Tokyo metropolitan government.

for (i in 1:2){

  # Define the L452R share of infection used for a regressor.
  # Also set the dates for the out-of-sample forecast period that is part of the sample period for the regression.
  if (i==1){
    # the estimate by fitting a logistic growth model to the nationwide L452R share of new cases.
    L452R_reg <- L452R_fitted_d[-7 + ndays_normal[1] + 1 + 1:length(fcst_err)]*100
    # Set the dates for the sample period for the regression.
    date_label_fcst_err <- date_label_fcst[which(date_label_fcst=="2021-02-02"):length(date_label_fcst)]
  }else{
    # the data reported by the Tokyo metropolitan government from 7 days before Feb. 2, 2021 to 7 days before the end of Oct, 2021 (The last week of the data is only based on 5 tested samples).
    L452R_reg <- L452R_share_Tokyo_d[-7 + (ndays_normal[1] + 2):(length(L452R_share_Tokyo_d)-7)]*100
    # Set the dates for the sample period for the regression.
    date_label_fcst_err <- date_label_fcst[which(date_label_fcst=="2021-02-02"):which(date_label_fcst=="2021-10-31")]
  }  
  
  # Extract the vaccination rate from 7 days before the first date of the out-of-sample forecast period.
  # The first date of vccn_scnd_share is January 1, 2021.
  fcst_err_lm <- lm(fcst_err[1:length(L452R_reg)] ~ -1 + I(vccn_scnd_share[-7 + ndays_normal[1] + 1 + 1:length(L452R_reg)]*100) + L452R_reg)
  
  # Print the summary of the regression.
  temp_fn <- file(paste0("COVID_frcst_err_weathertype",log_abs_hum,"_exconlinecloth",exc_online_cloth,"_dummyfor2ndSE",fcst_td,"_ENG_",as.logical(ENG),"_",i,".txt"), open="w", encoding="UTF-8")
  sink(temp_fn)
  print(summary(fcst_err_lm)) # Print the regression result into a text file.
  sink()
  close(temp_fn)
  
  # Extract the fitted values of the regression with confidence intervals.
  fcst_err_fitted <- as_tibble(predict(fcst_err_lm, interval="confidence", level=0.95))
  
  # Define a tibble for plotting.
  temp_df <- tibble(Dates = as.Date(date_label_fcst_err), value = fcst_err[1:length(L452R_reg)], fit = fcst_err_fitted$fit, lwr=fcst_err_fitted$lwr, upr=fcst_err_fitted$upr)
  
  # Define the figure title.
  if (plot_paper==1){
    temp_main_label =""
  }else{
    if (ENG==1){
      if (i==1){
        temp_main_label ="Forecast error in out-of-sample forecasts (L452R share estimated by a logistic fun.)"
      }else{
        temp_main_label ="Forecast error in out-of-sample forecasts (L452R share reported by the Tokyo gov.)"
      }
    }else{
      if (i==1){
        temp_main_label ="推計期間外予測の誤差(L452陽性率はロジスティック成長モデルで推計）"
      }else{
        temp_main_label ="推計期間外予測の誤差(L452陽性率は東京都のデータ）"
      }
    }
  }
  
  # Define axis labels.
  if (ENG==1){
    temp_xlab <- "Dates"
    temp_ylab <- ""
  }else{
    temp_xlab <- "日次"
    temp_ylab <- ""
  }
  
  # Set the legend label.
  if (ENG==1){
    temp_lgnd=c("Out-of-sample forecast error", "Fitted value")
  }else{
    temp_lgnd=c("推計期間外予測の誤差", "回帰式のfitted value")
  }
  
  # Set viridis color palette.
  temp_viridis <- viridis(2) 
  
  # Plot the data frame by ggplot.  
  temp_p <- ggplot(temp_df, aes(x=Dates))+
    geom_line(aes(y=value, colour="value"))+
    geom_line(aes(y=fit, colour="fit"))+
    scale_x_date(date_breaks = "1 month", date_labels = "%b%d\n%Y")+
    labs(title=temp_main_label, x=temp_xlab, y=temp_ylab, colour="")+
    scale_color_manual(values=temp_viridis,breaks=c("value","fit"),labels=temp_lgnd)+
    theme(legend.position="bottom") +
    geom_ribbon(aes(ymin = lwr, ymax = upr), fill=temp_viridis[2], alpha=0.2)
  
  # Add shadows, arrows, and texts to indicate the periods of states of emergency.
  # date_label_fcst_err starts from the middle of the second state of emergency.
  temp_p <- temp_p + annotate("rect",xmin=as.Date(date_label_fcst_err[1]),xmax=as.Date(Dates_SE_list[4]),ymin=-Inf,ymax=Inf, alpha=.05,fill="blue")
  temp_p <- temp_p + annotate("rect",xmin=as.Date(Dates_SE_list[7]),xmax=as.Date(Dates_SE_list[6]),ymin=-Inf,ymax=Inf, alpha=.05,fill="blue")
  temp_p <- temp_p + annotate("rect",xmin=as.Date(Dates_SE_list[7]),xmax=as.Date(Dates_SE_list[8]),ymin=-Inf,ymax=Inf, alpha=.05,fill="blue")
  
  # Print the plot.
  print(temp_p)
  
}




############### Plot the online share of household expenditures. ##################

# Define labels for the plot.
if (ENG==1){
  if (plot_paper){
    temp_main_label <- ""
  }else{
    temp_main_label <- "Online share of household expenditures"
  }
  temp_xlab <- "Months"  
  temp_ylab <- "%"
}else{
  temp_main_label <- "インターネットを利用した家計支出の割合"
  temp_xlab <- "月次"
  temp_ylab <- "%" 
}


# Define a tibble for plotting.
temp_df <- tibble(Dates = seq(as.Date("2019/1/1"), as.Date(paste0(hes_end_all[1],"/",hes_end_all[2],"/1")), by="month"), 
                  value_total = hes_online_clothing_share_m[1,]*100,
                  value_clothing = hes_online_clothing_share_m[2,]*100)

# Save the tibble for online exporting.
tbl_online_exp <- temp_df
  
# Apply plot longer to plot the nationwide data and Tokyo data in the same figure.
temp_df <-  gather(temp_df, key = variable, value = value, -Dates)

# Plot data.
temp_p <- ggplot(temp_df,aes(x=Dates,y=value, color=variable))+
  geom_line()+
  geom_point()+
  scale_x_date(date_breaks = "3 month", date_labels = "%b%d\n%Y")+
  labs(title=temp_main_label, x=temp_xlab, y=temp_ylab)+
  theme(legend.position="bottom")+
  scale_color_viridis_d(name="",breaks=c("value_clothing","value_total"), labels=c("Clothing and footwear","Total household expenditures"))

# Print the plot.
print(temp_p)




# Close the pdf file.
dev.off()


# ########## Compute correlation coefficients among explanatory variables. ###################################
# 
# # Write a table of correlation coefficients in a tex file.
# sink(paste0("Corr_coef_exp_var_model",mdl_number,"_weathertype",log_abs_hum,"_exconlinecloth",exc_online_cloth,"_ENG",as.logical(ENG),"_",hes_end[1],"-",hes_end[2],".txt"))
# 
# # Swap the rows for "被服及び履物" and "入場・観覧・ゲーム代".
# temp <- H_expvals
# temp[loc_ACGF,] <- H_expvals[loc_CandF,]
# temp[loc_CandF,] <- H_expvals[loc_ACGF,]
# 
# # Compute the cross correlation coefficients among explanatory variables.
# # cor() computes cross correlation coefficients among column vectors.
# # H_expvals starts from January 1, 2020, and mob_var4 starts from February 15, 2020.
# temp <- cor(t(rbind(temp[,(31+15):which(hes_var_date==hes_end_date_estimation)],mob_var4[1:which(mob_date==hes_end_date_estimation)]))) 
# 
# # Replace the data labels in the stan file into those in the paper.
# rownames(temp) <- lgnd[1:(length(lgnd)-2)]
# colnames(temp) <- lgnd[1:(length(lgnd)-2)]
# 
# # Print the table in the latex form.
# print(xtable(temp,digits=c(0,rep(3,length(lgnd)-2))), sanitize.rownames.function = identity)
# sink()



############# Correlation between the dependent variable and large categories of household expenditures #################

# # Record the figure for each household expenditure in a pdf file.
# eval(parse(text=paste0("pdf(file=\"COVID_DEP_nom_hes_ENG_",as.logical(ENG),".pdf\", family=\"Japan1GothicBBB\")")))

# Use the sample period for estimation.
# Define the dependent variable.
# The dependent variable is the rate of change in new cases over a week. It starts from 2020 March 1. The fitted value is available from 2020 March 1.
temp_DEP <- RoC_newcases_data[1:which(RoC_newcases_date=="2021/2/1")]

# Extract the data for the common sample period.
temp_hes <- hes_var2_all[,31+29+1:length(temp_DEP)] # hes_var2_all starts from 2020 January 1. Extract data from 2020 March 1 on.
temp_R <- temp_DEP[1:ncol(temp_hes)] # The dependent variable Reproduction number from 2020 March 1 on.

# Check if the dates of the two data coincide.
print(paste("R:",c(R_date[1],R_date[ncol(temp_hes)]), "; hes:", c(hes_var_date_all[(31+29+1)],hes_var_date_all[31+29+length(temp_DEP)])))

# Find the location of the period of the state of emergency.
date_label <- R_date[1:ncol(temp_hes)] # Define the date label to use.
# Initialize the vector to contain the location of states of emergency.
SE <- NULL
# Locate the dates of states of emergency.
for (j in 1:length(Dates_SE_list)){
  SE <- c(SE,which(date_label==conv_date_format(Dates_SE_list[j])))
}

# Plot data.
rslt_ccf_R_hes<-plot_ts_ccf(temp_R,"被説明変数",temp_hes,hes_var_nm2[,1],date_label,h_val=0,SE=SE)

# # Close the pdf file.
# dev.off()

# Print ccf of all household expenditure items.
# Define items to extract from the list of ccf.
hes_large_nm <- c("食料","住居","光熱・水道","家具・家事用品","被服及び履物","保健医療","交通・通信","教育","教養娯楽","その他の消費支出","飲酒代")
hes_large_nm_ENG <- c("Food","Housing","Fuel, light, and water charges","Furniture and household utensils","Clothing and footwear","Medical care","Transportation and communication","Education","Culture and recreation","The other household consumption expenditures","Bar")
temp_1 <- rep(NA, length(hes_large_nm)) # Data label.
temp_2 <- rep(NA, length(hes_large_nm)) # Largest correlation coefficient with R.
temp_3 <- rep(NA, length(hes_large_nm)) # Location of the lag for the largest correlation coefficient.
for (i in 1:length(hes_large_nm)){
  if (ENG==1){
    temp_1[i] <- hes_large_nm_ENG[i]
  }else{
    temp_1[i] <- hes_large_nm[i]
  }
  temp_2[i] <- mean(rslt_ccf_R_hes[which(hes_var_nm2[,1]==hes_large_nm[i]),1]) # mean() is applied as some labels appear twice.
  temp_3[i] <- mean(rslt_ccf_R_hes[which(hes_var_nm2[,1]==hes_large_nm[i]),2]) # mean() is applied as some labels appear twice.
}
temp <- cbind(temp_2,temp_3)
colnames(temp)<-c("Largest correlation coefficient","lag")
rownames(temp)<-temp_1
temp_fn <- file(paste0("COVID_ccf_DEP_nom_hes_ENG_",as.logical(ENG),".txt"), open="w", encoding="UTF-8")
sink(temp_fn)
print(xtable(temp,digits=c(0,2,0))) # Print the table in the latex form.
sink()
close(temp_fn)




##################### Export data for uploading. ##########################

if (ENG==1){

  ### Classified household expenditure included in explanatory variables. Unit: yen per household (nominal); 100 yen at the 2020 average price per household (real).
  
  # Create the column names of household expenditure items.
  H_exp_colnames <- lgnd[1:(length(lgnd)-3)]
  
  for (k in 1:2){
    
    # Combine 2019 data with 2020- data.
    if (k==1){
      # k=1: Nominal household expenditures.
      temp0 <- temp1 <- cbind(H_expvals_2019_nominal, H_expvals_all_nominal)
      
    }else{
      # k=2: Real household expenditures.
      temp0 <- temp1 <- temp <- cbind(H_expvals_2019, H_expvals_all)
      
    }
    
    # Swap the order of admissions, viewing, and game fees and clothing and footwear, so that it becomes consistent with the order of characters in lgnd.
    temp1[loc_ACGF,] <- temp0[loc_CandF,]
    temp1[loc_CandF,] <- temp0[loc_ACGF,]
    
    # Initialize a tibble for household data series. 
    temp2 <- tibble(Dates=seq(as.Date("2019-01-01"), as.Date(paste0(hes_end_all[1],"-",hes_end_all[2],"-",ndays_normal[hes_end_all[2]])), by="day"))
    
    for (i in seq_along(H_exp_colnames)){
      eval(parse(text=paste0("temp2 <- temp2 %>% mutate(\'",H_exp_colnames[i],"\'=temp1[i,])")))
    }  
    
    # Export the tibble to a csv file.
    if (k==1){
      # k=1: Nominal household expenditures.
      write_csv(temp2,"expvar_nominal_household_expenditures.csv")
    }else{
      # k=2: Real household expenditures.
      write_csv(temp2,"expvar_real_household_expenditures.csv")
    }
  }
  
  
  
  ### CPI monthly. Unit: Index (the 2020 average price=100).
  
  # Initialize a tibble for CPI. 
  temp0 <- tibble(Dates=seq(as.Date("2019-01-01"), as.Date(paste0(hes_end_all[1],"-",hes_end_all[2],"-",ndays_normal[hes_end_all[2]])), by="month"))
  
  # Drop the interpolated index for non-packaged lodging. 
  temp1 <- CPI_m_2020_rvs[-2,]
  
  # Change the label for Lodging (including accommodations in domestic travel packages) to lodging.
  temp2 <- CPI_name_list
  #temp2[2] <- "Lodging"    
  
  # Add each sub-index of CPI to the tibble.
  for (i in seq_along(temp2)){
    eval(parse(text=paste0("temp0 <- temp0 %>% mutate(\'",temp2[i],"\'=temp1[i,])")))
  }
  
  # Export the tibble to a csv file.
  write_csv(temp0,"expvar_CPI.csv")
  
  
  
  ### Mobility in public transportation. (Google)  
  
  # Construct a tibble for transit_stations in Google mobility data.
  temp <- tibble(Dates=as.Date(mob_date), 'transit_stations for Japan' =mob_var4[,1])
  
  # Export the tibble to a csv file.
  write_csv(temp,"expvar_mobility_in_public_transportation.csv")
  
  
  
  ### Number of new confirmed cases of COVID-19. Unit: counts.  
  
  # Construct a tibble for the dependent variable.
  temp <- tibble(Dates = as.Date(newcases_date), 'The number of new confirmed cases of COVID-19' = newcases_data)
  
  # Export the tibble to a csv file.
  write_csv(temp,"depvar_number_of_new_confirmed_cases.csv")

  
  #### Large categories of nominal household expenditures. Unit: yen per household.

  # Initialize a tibble to contain large categories of nominal household expenditures.
  temp <- tibble(Dates=seq(as.Date("2020-01-01"), as.Date(paste0(hes_end_all[1],"-",hes_end_all[2],"-",ndays_normal[hes_end_all[2]])), by="day"))
  
  for (i in 1:(length(hes_large_nm)-1)){ # The last item of hes_large_nm is alcoholic drink at bars and restaurants.
    # Extract each large category of household expenditures.
    eval(parse(text=paste0("temp <- temp %>% mutate(\'",hes_large_nm_ENG[i],"\'=hes_var2_all[min(which(hes_var_nm2[,1]==hes_large_nm[i])),])")))
  }
  
  # Export the tibble to a csv file.
  write_csv(temp,"nominal_household_expenditures_large_categories.csv")
  
  

  #### Weather data across prefectures. Unit: Celsius degree (temperature); % (relative humidity); g/m^3 (absolute humidity).
  
  # Define the English name of the capital of each preference.
  # weath_pref_nm <- c("札幌", "青森", "盛岡", "秋田", "仙台", "山形", "福島", "水戸", "宇都宮", "さいたま", "千葉", "東京", "新潟", "前橋", "長野", "甲府", "横浜", "静岡", "富山", "岐阜", "名古屋", "金沢", "福井", "大津", "津", "奈良", "和歌山", "大阪", "京都", "神戸", "鳥取", "岡山", "松江", "広島", "山口", "高松", "松山", "徳島", "高知", "福岡", "大分", "宮崎", "佐賀", "熊本", "鹿児島", "長崎", "那覇") 
  pref_capital_nm_ENG <- c("Sapporo", "Aomori", "Morioka", "Akita", "Sendai", "Yamagata", "Fukushima", "Mito", "Utsunomiya", "Saitama", "Chiba", "Tokyo", "Niigata", "Maebashi", "Nagano", "Kofu", "Yokohama", "Shizuoka", "Toyama", "Gifu", "Nagoya", "Kanazawa", "Fukui", "Otsu", "Tsu", "Nara", "Wakayama", "Osaka", "Kyoto", "Kobe", "Tottori", "Okayama", "Matsue", "Hiroshima", "Yamaguchi", "Takamatsu", "Matsuyama", "Tokushima", "Kochi", "Fukuoka", "Oita", "Miyazaki", "Saga", "Kumamoto", "Kagoshima", "Nagasaki", "Naha")
  pref_nm_ENG <- c("Hokkaido", "Aomori", "Iwate", "Akita", "Miyagi", "Yamagata", "Fukushima", "Ibaraki", "Tochigi", "Saitama", "Chiba", "Tokyo", "Niigata", "Gunma", "Nagano", "Yamanashi", "Kanagawa", "Shizuoka", "Toyama", "Gifu", "Aichi", "Ishikawa", "Fukui", "Shiga", "Mie", "Nara", "Wakayama", "Osaka", "Kyoto", "Hyogo", "Tottori", "Okayama", "Shimane", "Hiroshima", "Yamaguchi", "Kagawa", "Ehime", "Tokushima", "Kochi", "Fukuoka", "Oita", "Miyazaki", "Saga", "Kumamoto", "Kagoshima", "Nagasaki", "Okinawa")
  
  
  # Initialize a tibble for weather data.
  temp_temper <- temp_relhum <- temp_abshum <- tibble(Dates = as.Date(weath_date_all))
                 
  # Construct the tibble for weather data.
  for (i in 1:47){
    eval(parse(text=paste0("temp_temper <- temp_temper %>% mutate(\'",pref_nm_ENG[i],"\'=temper_var_all[,i])"))) # Daily average of relative tempereture.
    eval(parse(text=paste0("temp_relhum <- temp_relhum %>% mutate(\'",pref_nm_ENG[i],"\'=hum_var_all[,i])"))) # Daily average of relative humidity.
    eval(parse(text=paste0("temp_abshum <- temp_abshum %>% mutate(\'",pref_nm_ENG[i],"\'=abs_hum_var_all[,i])"))) # Daily average of relative humidity.
  }

  # Set each row for an observation (Date x City).
  
  temp_temper <- temp_temper %>% pivot_longer(all_of(pref_nm_ENG), names_to = 'Prefectures', values_to='Celsius tempereture')
  temp_relhum <- temp_relhum %>% pivot_longer(all_of(pref_nm_ENG), names_to = 'Prefectures', values_to='Relative humidity')
  temp_abshum <- temp_abshum %>% pivot_longer(all_of(pref_nm_ENG), names_to = 'Prefectures', values_to='Absolute humidity')

  # Combine prefectural weather data.
  
  temp_weath <- inner_join(temp_temper,temp_relhum, by=c('Dates','Prefectures'))
  temp_weath <- inner_join(temp_weath,temp_abshum, by=c('Dates','Prefectures'))
  
  # Export the tibble to a csv file.
  write_csv(temp_weath,"weather_in_each_prefecure.csv")
  
  
  ### Population-weighted nationwide average of absolute humidity. Unit: %/m^3
  
  # Construct a tibble for the dependent variable.
  temp <- tibble(Dates = as.Date(weath_date_all), 'Population-weighted nationwide average of absolute humidity' = abs_hum_var_ave_all, 'Population-weighted nationwide average of temperature' = temper_var_ave_all)
  
  # Export the tibble to a csv file.
  write_csv(temp,"expvar_nationwide_ave_of_weather_data.csv")


  #### 2019 Population data across prefectures. Unit: 1000 person.
  
  # Construct a tibble for the dependent variable.
  temp <- tibble(Prefectures = pref_nm_ENG, 'Populations' = popu_share*JPN_popu_2019)
  
  # Export the tibble to a csv file.
  write_csv(temp,"population_in_2019_in_each_prefecure.csv")
  

  
  #### The sample distribution of incubation periods. Unit: counts.

  # Create a tibble.
  temp <- tibble('Days of incubation periods'= 1:14, Counts = rev(dist_incub)*125)
  
  # Export the tibble to a csv file.
  write_csv(temp,"sample_distribution_of_incubation_periods.csv")
  

  #### Export the twice vaccinated share of the population and the L452 share of new confirmed cases in Japan. Unit: percent.
  
  # Rename the variables and export the tibbles to csv files.
  tbl_L452R %>% rename('L452 share of new confirmed cases'=value) %>%
    write_csv("L452R_share_of_new_confirmed_cases.csv")

  tbl_L452R_Tokyo %>% rename('L452 share of new confirmed cases in Tokyo'=value) %>%
    write_csv("L452R_share_of_new_confirmed_cases_in_Tokyo.csv")
  
  tbl_vccn %>% rename("Twice vaccinated share of the population"=value) %>%
    write_csv("Twice_vaccinated_share.csv")
  
  #### Export the online shares of total household expenditures and household expenditures for clothing and footwear.
  
  tbl_online_exp %>% rename('Online share of household expenditures for clothing and footwear (%)'=value_clothing) %>%
    rename('Online share of total household expenditures (%)'=value_total) %>%
    write_csv("online_share_of_household_expenditures.csv")

}
    







