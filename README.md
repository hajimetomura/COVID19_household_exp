# COVID19_household_exp (Data branch)

1. Dependent variable of the regression.

depvar_number_of_new_confirmed_cases.csv: This file contains the number of new confirmed cases of COVID-19 in Japan. The unit: counts. The data frequency: daily. The source: the Ministry of Health, Labor, and Welfare, the Government of Japan. Available from: https://covid19.mhlw.go.jp/en/.


2. Explanatory variables of the regression.

expvar_nominal_household_expenditures.csv: This file contains the nominal values of classified household expenditures included in the explanatory variables of the regression model. The unit: yen per household. The data frequency: daily. The source: the Family Income and Expenditure Survey, the Statistics Bureau, the Ministry of Internal Affairs and Communications, the Government of Japan. Available from: https://www.stat.go.jp/english/data/kakei/index.html.


nominal_household_expenditures_large_categories.csv: This file contains the large categories of nominal household expenditures in Japan. The unit: yen per household. The data frequency: daily. The source: the Family Income and Expenditure Survey, the Statistics Bureau, the Ministry of Internal Affairs and Communications, the Government of Japan. Available from: https://www.stat.go.jp/english/data/kakei/index.html.


expvar_real_household_expenditures.csv: This file contains the real values of classified household expenditures included in the explanatory variables of the regression model. The unit: 100 yen per household. The data frequency: daily. The source: the author's calculation.


expvar_CPI.csv: This file contains the consumer price indices used to convert nominal household expenditures into real household expenditures in the construction of explanatory variables. The unit: each index is normalized at 100 for the average in 2020. The data frequency: monthly. The source: the Consumer Price Index, the Statistics Bureau, the Ministry of Internal Affairs and Communications, the Government of Japan. Available from: https://www.stat.go.jp/english/data/cpi/index.html.


expvar_mobility_in_public_transportation.csv: This file contains transit_stations for Japan (nationwide) in the COVID-19 Community Mobility Reports published by Google. The unit: percent. The data frequency: daily. Available from: https://www.google.com/covid19/mobility/.
  

expvar_nationwide_ave_of_weather_data.csv: This file contains the population-weighted average of absolute humidity and outside temperature at the capitals of prefectures. The unit: g/m^3 and Celsius degree. The data frequency: daily. The source: the author's calculation.


weather_in_each_prefecure.csv: This file contains relative temperature, relative humidity, absolute humidity at the capital of each prefecture in Japan. The unit: Celsius degree (temperature); percent (relative humidity); g/m^3 (absolute humidity). The data frequency: daily. The sources: the Japan Meteorological Agency, the Government of Japan. Available from https://www.data.jma.go.jp/gmd/risk/obsdl/index.php; and the author's calculation.


  
population_in_2019_in_each_prefecure.csv: This file contains the population of each prefecture as of October 1, 2019. The unit: 1000 persons. The source: Current Population Estimates, the Statistics Bureau, the Ministry of Internal Affairs and Communications, the Government of Japan. Available from: https://www.stat.go.jp/english/data/jinsui/2019np/index.html.


sample_distribution_of_incubation_periods.csv: This file contains the samples of incubation periods in Japan. The number of cases with incubation periods greater than 14 days are included in the number of cases with the incubation period being 14 days in this file. The unit: counts. The source: Sugishita Y, Kurita J, Sugawara T, Ohkusa Y., "Effects of voluntary event cancellation and school closure as countermeasures against COVID-19 outbreak in Japan", PLOS ONE 2020;15(12):e0239455.


3. Other data.

L452R_share_of_new_confirmed_cases.csv: This file contains the L452R share of new confirmed cases in Japan. The unit: percent. The source: the Ministry of Health, Labor, and Welfare, the Government of Japan. The link to the online source dissappeared from the website: https://www.mhlw.go.jp/stf/covid-19/kokunainohasseijoukyou.html.

L452R_share_of_new_confirmed_cases_in_Tokyo.csv: This file contains the L452R share of new confirmed cases in Tokyo. The unit: percent. The source: the Tokyo Metropolitan government. Available from: https://www.bousai.metro.tokyo.lg.jp/taisaku/saigai/1010035/index.html.

Twice_vaccinated_share.csv: This file contains the twice vaccinated share of the population in Japan. The unit: percent. The source: the Ministry of Health, Labor, and Welfare, the Government of Japan. Available from: https://covid19.mhlw.go.jp/en/.

online_share_of_household_expenditures.csv:  This file contains the online share of household expenditures per household and that of household expenditures for clothing and footwear per household in Japan. The unit: 100 yen in the current price. The source: the Statistics Bureau, the Ministry of Internal Affairs and Communications, the Government of Japan. Available from: https://www.stat.go.jp/english/data/joukyou/index.html.
