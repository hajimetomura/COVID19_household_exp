// Run a linear regression model of the reproduction number of COVID 19.
// The dependent variable is not a function of moving averages.
// See localtrend_COVID_data.R for the definition of variables.
// Set non-negativity conditions on coefficients.
// Include dummies for declaration of a state of emergency.
// Introduce restrictions on coefficients so that household expenditure and mobility have always positive effects on the reproduction number.
// Dummy for the period before the first state of emergency.
// Initial value of unobserved infectious events is drawn from the unconditional distribution.
// Include thaters and theme parks and gaming and sporting, and the residual of household expenditures into the explanatory variables.
// Do not apply the distribution of incubation periods to AR(1) error term. Only include AR(1) error term in the observation equation.
data {
  int R_TT; // From 2020 March 1.
  vector [R_TT] R;
  int H_TT; // From 2020 Jan 1.
  matrix [8, H_TT] H_expvals;
  int M_TT; // From 2020 Feb 15.
  row_vector [M_TT] M_trans;
  int W_TT; // From 2020 Jan. 1.
  vector [W_TT] W_abs_hum; // The weighted average of indicators for absolute humidity.
  int TT_diff_M_H;
  int TT_diff_M_W; // Equal to TT_diff_M_H.
  vector [14] dist_incub; // Log-normal distribution of incubation periods.
  vector [R_TT + 29-14] D_NY; // From 14 days before 2020 March 1.
  vector [R_TT + 29-14] D_SE1; // Dummy variable for the first state of emergency.
  vector [R_TT + 29-14] D_SE2; // Dummy variable for the second state of emergency.
  vector [R_TT + 29-14] D_pre_SE1; // Dummy variable for the preiod before the first state of emergency.
}

transformed data{
  // Take log differences from 2020 January means for household expenditure items:
  // eatout except cafe and drink;
  // cafe;
  // drink at bar and restaurants;
  // staying hotels;
  // packaged tours;
  // apparel shopping;
  // theatres, theme parks, gaming, and sports;
  // other consumption expenditures.

  int TT; // Each infection is affected by events between 1-day and 14 days lags. TT is the number of sample periods from 2020 February 15 to 14 days before the last date of R in the estimation period, exclusive. Or, TT is the number of sample periods of infections from 2020 March 1.

  matrix [50, H_TT - (TT_diff_M_H + 1) + 1] EXPVAR; // The matrix of explanatory variables.

  // TT_diff_M_H days from the beginning of the sample period of H_expvals is 2020 February 15, exclusive, the starting date of mobility data.
  // The first date of the dependent variable is set to March 1, 2020. So TT_diff_M_H + 1 is subtracted from H_TT to begin the explanatory variables from February 16, 2020 (i.e., 14 days before March 1, 2020).
  // Because explanatory variables affect R in 1 day lag, and they consist of raw variables with 14 day lags, the length of the estimation period is H_TT - (TT_diff_M_H + 1)  + 1 periods - 14 lags = H_TT - (TT_diff_M_H + 1) + 1 - 14. It is taken as given that the sample period of R ends after that of household expenditure items.
  TT = H_TT - (TT_diff_M_H + 1) + 1 - 14;

  // Set the explanatory variables for infections on 2020 Februrary 29 to those on the last date of R used in the estimation.
  // t = 1 corresponds to the explanatory variables on 2020 Feburary 16, or those for reported infections on 2020 March 1, (14+1)-th days from 2020 February 16.
  // t = TT corresponds to 14 days before the last date of R in the estimation, which is one day after the last household expenditure sample.
  // Take a weighted average from 14 days onward from t, inclusive. The weight is dist_incub and dummies.
  // For some explanatory variables, TT_diff_M_W + 1 and TT_diff_M_H + 1 are necessary to make the date of the first element equal February 16, 2020 (i.e., when t=1).
  // For dummy variables and M_trans, 1 is added as the first element is set for Feb. 16, 2020.
  for (t in 1:TT){

    EXPVAR[1,t] = dot_product(D_NY[(1+t):(1+t+13)], dist_incub);
    EXPVAR[2,t] = dot_product(W_abs_hum[(TT_diff_M_W + 1 + t):(TT_diff_M_W + 1 + t + 13)], dist_incub);
    EXPVAR[3:11,t] = append_row(H_expvals[,(TT_diff_M_H + 1 + t):(TT_diff_M_H + 1 + t + 13)], M_trans[(1+t):(1+t+13)]) * dist_incub;
    EXPVAR[12:20,t] = append_row(H_expvals[,(TT_diff_M_H + 1 + t):(TT_diff_M_H + 1 + t + 13)], M_trans[(1+t):(1+t+13)]) * (dist_incub .* W_abs_hum[(TT_diff_M_W + 1 + t):(TT_diff_M_W + 1 + t +13)]);
    EXPVAR[21,t] = dot_product(D_SE1[(1+t):(1+t+13)], dist_incub);
    EXPVAR[22,t] = dot_product(D_SE2[(1+t):(1+t+13)], dist_incub);
    EXPVAR[23,t] = dot_product(D_pre_SE1[(1+t):(1+t+13)], dist_incub);
    EXPVAR[24:32,t] = append_row(H_expvals[,(TT_diff_M_H + 1 + t):(TT_diff_M_H + 1 + t + 13)], M_trans[(1+t):(1+t+13)]) * (dist_incub .* D_SE1[(1+t):(1+t+13)]);
    EXPVAR[33:41,t] = append_row(H_expvals[,(TT_diff_M_H + 1 + t):(TT_diff_M_H + 1 + t + 13)], M_trans[(1+t):(1+t+13)]) * (dist_incub .* D_SE2[(1+t):(1+t+13)]);
    EXPVAR[42:50,t] = append_row(H_expvals[,(TT_diff_M_H + 1 + t):(TT_diff_M_H + 1 + t + 13)], M_trans[(1+t):(1+t+13)]) * (dist_incub .* D_pre_SE1[(1+t):(1+t+13)]);
  }

}


parameters {
  real <lower=0> sd_R; // Standard deviation of errors in the observation equation for R.

  real gama; // Constant intercept.

  real <lower=0> coef_NY; // Coefficient for time dummies for the new-year period.

  real <upper=0> coef_AH; // Coefficient for the level effect of absolute humidity. Restricted to be negative, given the existing evidence of medical research.

  real coef_SE1; // Coefficient for time dummies for the first state of emergency.
  real coef_SE2; // Coefficient for time dummies for the second state of emergency.
  real coef_pre_SE1; // Coefficient for time dummies for the period before the first state of emergency.


  // The effects of household expeduture items and mobility in transportation.
  real <lower=0> scl1;
  real <lower=0> scl2;
  real <lower=0> scl3;
  real <lower=0> scl4;
  real <lower=0> scl5;
  real <lower=0> scl6;
  real <lower=0> scl7;
  real <lower=0> scl8;
  real <lower=0> scl9;

  // Coefficient for the cross effect between absolute humidity, and household expeduture items or mobility in transportation. Restricted to be negative, given the existing evidence of medical research.
  real <lower=-scl1, upper=0> scl_AH1;
  real <lower=-scl2, upper=0> scl_AH2;
  real <lower=-scl3, upper=0> scl_AH3;
  real <lower=-scl4, upper=0> scl_AH4;
  real <lower=-scl5, upper=0> scl_AH5;
  real <lower=-scl6, upper=0> scl_AH6;
  real <lower=-scl7, upper=0> scl_AH7;
  real <lower=-scl8, upper=0> scl_AH8;
  real <lower=-scl9, upper=0> scl_AH9;

  // Coefficient for the cross effect between absolute humidity, and household expeduture items or mobility in transportation, and the first state of emergency.
  real <lower=-scl1-scl_AH1> scl_AH_SE11;
  real <lower=-scl2-scl_AH2> scl_AH_SE12;
  real <lower=-scl3-scl_AH3> scl_AH_SE13;
  real <lower=-scl4-scl_AH4> scl_AH_SE14;
  real <lower=-scl5-scl_AH5> scl_AH_SE15;
  real <lower=-scl6-scl_AH6> scl_AH_SE16;
  real <lower=-scl7-scl_AH7> scl_AH_SE17;
  real <lower=-scl8-scl_AH8> scl_AH_SE18;
  real <lower=-scl9-scl_AH9> scl_AH_SE19;

  // Coefficient for the cross effect between absolute humidity, and household expeduture items or mobility in transportation. Restricted to be negative, given the existing evidence of medical research.
  real <lower=-scl1-scl_AH1> scl_AH_SE21;
  real <lower=-scl2-scl_AH2> scl_AH_SE22;
  real <lower=-scl3-scl_AH3> scl_AH_SE23;
  real <lower=-scl4-scl_AH4> scl_AH_SE24;
  real <lower=-scl5-scl_AH5> scl_AH_SE25;
  real <lower=-scl6-scl_AH6> scl_AH_SE26;
  real <lower=-scl7-scl_AH7> scl_AH_SE27;
  real <lower=-scl8-scl_AH8> scl_AH_SE28;
  real <lower=-scl9-scl_AH9> scl_AH_SE29;

  // Coefficient for the cross effect between absolute humidity, and household expeduture items or mobility in transportation. Restricted to be negative, given the existing evidence of medical research.
  real <lower=-scl1-scl_AH1> scl_AH_pre_SE11;
  real <lower=-scl2-scl_AH2> scl_AH_pre_SE12;
  real <lower=-scl3-scl_AH3> scl_AH_pre_SE13;
  real <lower=-scl4-scl_AH4> scl_AH_pre_SE14;
  real <lower=-scl5-scl_AH5> scl_AH_pre_SE15;
  real <lower=-scl6-scl_AH6> scl_AH_pre_SE16;
  real <lower=-scl7-scl_AH7> scl_AH_pre_SE17;
  real <lower=-scl8-scl_AH8> scl_AH_pre_SE18;
  real <lower=-scl9-scl_AH9> scl_AH_pre_SE19;


  real R_err_0; // Error in the observation equation for R before the start of the estimation period.

  real <lower=-1, upper=1> rho; // AR(1) term for unobseved infectious events.

//  real <lower=0> sd_XF; // Standard deviation of structural shcoks to unobserved infectious events.

//  real XF_0; // Initial value of unobserved infectious events.

//  vector [TT+12] XF_err; // Structural shocks to infectious events.
}

transformed parameters {

  vector [TT] mean_R; // mean of the dependent variable.
  vector [TT+1] R_err; // Measuerment errors.
  vector [TT] R_obs_err; // Observed measurement errors.
  //vector [TT+13] XF; // Unobserved infectious events.
  //vector [TT] sum_XF; // The product of XF and dist_incub.
  row_vector [9] scl; // row vector of scli for i = 1,2,...10.
  row_vector [9] scl_AH; // row vector of scl_AHi for i = 1,2,...10.
  row_vector [9] scl_AH_SE1; // row vector of scl_AH_SE1i for i = 1,2,...10.
  row_vector [9] scl_AH_SE2; // row vector of scl_AH_SE1i for i = 1,2,...10.
  row_vector [9] scl_AH_pre_SE1; // row vector of scl_AH_pre_SE1i for i = 1,2,...10.

  // Bundle reals into row vectors.
  scl = [scl1,scl2,scl3,scl4,scl5,scl6,scl7,scl8,scl9];
  scl_AH = [scl_AH1,scl_AH2,scl_AH3,scl_AH4,scl_AH5,scl_AH6,scl_AH7,scl_AH8,scl_AH9];
  scl_AH_SE1 = [scl_AH_SE11,scl_AH_SE12,scl_AH_SE13,scl_AH_SE14,scl_AH_SE15,scl_AH_SE16,scl_AH_SE17,scl_AH_SE18,scl_AH_SE19];
  scl_AH_SE2 = [scl_AH_SE21,scl_AH_SE22,scl_AH_SE23,scl_AH_SE24,scl_AH_SE25,scl_AH_SE26,scl_AH_SE27,scl_AH_SE28,scl_AH_SE29];
  scl_AH_pre_SE1 = [scl_AH_pre_SE11,scl_AH_pre_SE12,scl_AH_pre_SE13,scl_AH_pre_SE14,scl_AH_pre_SE15,scl_AH_pre_SE16,scl_AH_pre_SE17,scl_AH_pre_SE18,scl_AH_pre_SE19];

  // Set the values of R_err before 2020 March 6 (i.e., the start of the estimation period).
  R_err[1] = R_err_0;

  // Simulate structural shocks to infectious events.
  // XF[1] = XF_0;
  // for (t in 1:(TT+12)){
  //   XF[t+1] = rho * XF[t] + sd_XF * XF_err[t];
  // }

  // The mean of the growth of infection in each period.
  // For R, the first element (t=1) is to 2020 March 1.
  // For H_expvals and M_trans, the (TT_diff_M_H+1)-th and (TT_diff_M_M+1)-th element correspond to 2020 February 16.
  // For mean R, the first element is the weighted average of past 14-days effects from 2020 February 16 to 2020 Februrary 29.
  // For W_abs_hum, the first element is 2020 February 16.
  // For D_NY, the first element is 2020 February 16.
  // For R_err, the first element is 2020 February 29.
  // For R_obs_err, the first element is 2020 March 1.


  for (t in 1:TT){
    mean_R[t] = gama +  coef_NY * EXPVAR[1,t] + coef_AH * EXPVAR[2,t] + scl * EXPVAR[3:11,t] + scl_AH * EXPVAR[12:20,t] + coef_SE1 * EXPVAR[21,t] + coef_SE2 * EXPVAR[22,t] +  coef_pre_SE1 * EXPVAR[23,t] + scl_AH_SE1 * EXPVAR[24:32,t] + scl_AH_SE2 * EXPVAR[33:41,t] + scl_AH_pre_SE1 * EXPVAR[42:50,t];
  }

  // TT is the number of sample periods from 2020 February 15 to 14 days before the last date of R in the estimation period, exclusive. Create the weighted averages of structural shocks for this sample length.
  // for (t in 1:TT){
  //   sum_XF[t] = dot_product(XF[t:(t+13)], dist_incub);
  // }

  // Compute the comtemporaneous value of R_err.
  // R[t+6-1]: -1 is added because the first date of R, i.e., 2020 March 1, is one day after 2020 February 29, i.e., 14-days after 2020 February 15, exclusive.
  for (t in 1:TT){

    R_obs_err[t] = R[t] - mean_R[t] - rho * R_err[t];
    R_err[t+1] = R_obs_err[t] + rho * R_err[t];

  }

}



model {

  R_err_0 ~ normal(0, sd_R/sqrt(1-rho^2)); // First six elements of R_err before the estimation period.

  target += normal_lpdf(R_obs_err | 0, sd_R); // Observation equatoin for R.

//  XF_err ~ std_normal(); // Structural shocks to unobserved infectious events.

//  XF_0 ~ normal(0, sd_XF/sqrt(1-rho^2)); // Initial value of unobserved infectious events.
}

generated quantities{

  vector [TT] Fitted_R; // Fitted values.

  Fitted_R =  mean_R;

}
