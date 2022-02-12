# Load and reform data for a linear regression of the reproduction number.
# This code must be called from a main code.

########## Set the parameter #####################

hes_end_all <- c(2021,12) # The end of the sample period available (year, month).

hes_end <- c(2021,1) # The end of the sample period for estimation (year, month).

ah_ub <- 9 # The threshold for dummies for absolute humidity.

temper_ub <- 18 # The threshold for dummies for temperature.

# Number of residents in Japan (so jinko) as of Jan. 1, 2020: 127128905
# Number of residents in Japan (so jinko) as of Jan 1, 2021: 126645025
# https://cio.go.jp/c19vaccine_dashboard
# Number of residents in Japan (so jinko)
JPN_popu <- 126645025


# Set the generation time in days.
gen_time <- 7

# Define the number of days in each month.
ndays_olympic <- c(31,29,31,30,31,30,31,31,30,31,30,31) # Number of days in an olympic year.
ndays_normal <- c(31,28,31,30,31,30,31,31,30,31,30,31) # Number of days in an non-olympic year.



########## Load data from 2020 Jan. ###########################


#### The number of reported new cases, nationwide; Daily; 
# MHLW: 2020 Jan. 29-; Unit; persons.

temp <- read.csv("./data/newly_confirmed_cases_daily.csv", header=T, stringsAsFactors=FALSE) 

# There was a change in the format of data. Restore the old format by tibble.
temp <- pivot_longer(temp,colnames(temp[2:ncol(temp)]),names_to="Prefecture",values_to="Newly confirmed cases")

# Extract only nationwide data. 
temp <- temp[temp[,2]=="ALL",] 

# Extract the date of each element. Remove the data label (which is in Japanese) by c(). "yyyy/m/d" format.
newcases_date <- temp$Date 

# Extract data from February 29, 2020, as the number of new cases was small before then, which destabilizes the rate of increase in the number of new cases.
newcases_data <- temp$`Newly confirmed cases` 

# The rate of change in the number of new cases over a week from March 1, 2020.
RoC_newcases_data <- log(newcases_data[which(newcases_date=="2020/3/1"):length(newcases_data)]) - log(newcases_data[which(newcases_date=="2020/2/23"):(length(newcases_data)-7)])

# Set the dates corresponding to the rates of change in the number of new cases.
RoC_newcases_date <- newcases_date[which(newcases_date=="2020/3/1"):length(newcases_date)]


#### Google Mobility report data; Daily; 2020 Feb. 15- 2021 March 13; Unit: %. 

mob_data <- read.csv("./data/2020_JP_Region_Mobility_Report.csv", header=T, stringsAsFactors=FALSE) 
mob_ndays <- length(mob_data[[1]])/48 # Number of days in time series.
mob_loc_nm <- rep(NA,48) # Initialize the vectors of names of locations.
mob_var_nm <- c("retail_and_recreation", "grocery_and_pharmacy", "parks", "transit_stations", "workplaces", "residential") #names(mob_data)[10:15] # Names of variables in the original data set.
# Extract each type of mobility data.
for (j in 1:6){
  # Rows are days; columns are locations. 
  eval(parse(text=paste0("mob_var",j,"<-matrix(mob_data[[9+",j,"]],nr=mob_ndays)")))
}
mob_date <- mob_data[[9]][1:mob_ndays] # Date of each row of mob_varj for j=1,2,3,..,6.
for (i in 1:48){
  # Record the names of locations.
  if (i == 1){
    mob_loc_nm[1] <- "Nationwide"  
  }else{
    mob_loc_nm[i] <- mob_data[[3]][(i-1)*mob_ndays+1] 
  }
}

# Transform mob_date into yyyy/m/d format.
mob_date <- conv_date_format(mob_date)


#### Google Mobility report data; Daily; 2021 Jan. 1 - ; Unit: %. 

mob_data_2021 <- read.csv("./data/2021_JP_Region_Mobility_Report.csv", header=T, stringsAsFactors=FALSE) 
mob_ndays_2021 <- length(mob_data_2021[[1]])/48 # Number of days in time series.
mob_loc_nm_2021 <- rep(NA,48) # Initialize the vectors of names of locations.
if (sum(!(names(mob_data)[10:15] == names(mob_data_2021)[10:15]))>0){
  stop("The order of indicators in google mobility data may have changed.")
}
# Extract each type of mobility data.
for (j in 1:6){
  # Rows are days; columns are locations. 
  eval(parse(text=paste0("mob_var",j,"_2021<-matrix(mob_data_2021[[9+",j,"]],nr=mob_ndays_2021)")))
}
mob_date_2021 <- mob_data_2021[[9]][1:mob_ndays_2021] # Date of each row of mob_varj for j=1,2,3,..,6.
# Transform mob_date into yyyy/m/d format.
mob_date_2021 <- conv_date_format(mob_date_2021)

for (i in 1:48){
  # Record the names of locations.
  if (i == 1){
    mob_loc_nm_2021[1] <- "Nationwide"  
  }else{
    mob_loc_nm_2021[i] <- mob_data_2021[[3]][(i-1)*mob_ndays_2021+1] 
  }
}
if (sum(!(mob_loc_nm == mob_loc_nm_2021))>0){
  stop("The order of prefectures in google mobility data may have changed.")
}

# Connect 2021 data to 2020 data.
for (j in 1:6){
  eval(parse(text=paste0("if(!(mob_var",j,"[which(mob_date==\"2021/3/13\"),1]==mob_var",j,"_2021[which(mob_date_2021==\"2021/3/13\"),1])){stop(\"New google mobility report data are not consistent with the existing one.\")}"))) # Extend rows with new data.
  eval(parse(text=paste0("mob_var",j,"<-rbind(mob_var",j,", mob_var",j,"_2021[(which(mob_date_2021==\"2021/3/13\")+1):mob_ndays_2021,])"))) # Extend rows with new data.
}

# Transform mob_date_2021 into yyyy/m/d format.
mob_date_2021 <- conv_date_format(mob_date_2021)

# Extend the date label.
mob_date <- c(mob_date,mob_date_2021[(which(mob_date_2021=="2021/3/13")+1):mob_ndays_2021])

# Revise the number of periods.
mob_ndays <- length(mob_date)




### Household expenditure survey; Daily; 2020 Jan. 01; Unit: current yen. 

hes_var_all <- NULL # Initialize a matrix for household expenditure item variables.
hes_var2_all <- NULL # Initialize a matrix for detailed household expenditure item variables.
hes_var_nm <- NULL # Initialize a matrix for the names of household expenditure item variables.
hes_var_nm2 <- NULL # Initialize a matrix for the names of detailed household expenditure item variables.
hes_var_date_all <- NULL # Initialize a matrix for the dates of household expenditure item variables.

for (i in 2020:hes_end_all[1]){
  if (i == hes_end_all[1]) {
    # The last year in the sample.
    temp_mt <- c(1,hes_end_all[2]) # The first and last month of the year available in the sample.
  }else{
    temp_mt <- c(1,12) # The first and last month of the year available in the sample.
  }
  for (j in temp_mt[1]:temp_mt[2]){
    # Extract household expenditure data for month j, year i.
    # For an unknown reason, an empty column is read at the end. The last data column is the fourth to the last column.
    eval(parse(text=paste0("temp_dt <- read.csv(\"./data/household survey/a615_",i,"_",j,".csv\", header=F,stringsAsFactors=FALSE)"))) 
    eval(parse(text=paste0("temp_dt2 <- read.csv(\"./data/household survey/a616_",i,"_",j,".csv\", header=F,stringsAsFactors=FALSE)"))) 
    # Convert the list containing data into a numeric matrix. The format of the table changes from 2020 Nov. 
    if (i==2020 && j<11){
      temp_numeric <- conv_list(temp_dt[14:135,16:(ncol(temp_dt)-2-is.na(temp_dt[1,ncol(temp_dt)]))]) # Sometimes the last columns are NAs. In that case, exclude that column.
      temp_numeric2 <- conv_list(temp_dt2[14:679,11:(ncol(temp_dt2)-2-is.na(temp_dt2[1,ncol(temp_dt2)]))]) # Sometimes the last columns are NAs. In that case, exclude that column.
    }else{
      temp_numeric <- conv_list(temp_dt[10:131,14:(ncol(temp_dt)-2-is.na(temp_dt[1,ncol(temp_dt)]))]) # Sometimes the last columns are NAs. In that case, exclude that column.
      temp_numeric2 <- conv_list(temp_dt2[10:675,14:(ncol(temp_dt2)-2-is.na(temp_dt2[1,ncol(temp_dt2)]))]) # Sometimes the last columns are NAs. In that case, exclude that column.
    }
    # Extend the time series of household expenditure items. Remove commas from the matrix
    hes_var_all <- cbind(hes_var_all,temp_numeric)
    hes_var2_all <- cbind(hes_var2_all,temp_numeric2)
    # Extend the names of household expenditure items for each period.
    temp_nm <- rep(NA,135-14+1) # Initialize the vector for variable names for month j, year i.
    for (k in 14:135){ 
      # The format of the table changes from 2020 Nov.
      if (i==2020 && j<11){
        temp_nm[k-13] <- paste(temp_dt[k,11:14], collapse="")
      }else{
        temp_nm[k-13] <- paste(temp_dt[k-4,12], collapse="")
      }
    }
    hes_var_nm <- cbind(hes_var_nm, temp_nm)
    
    # Record the vector for variable names for month j, year i.
    if (i==2020 && j<11){
      # Remove double blanks.
      temp_nm2 <- sub(" ","",temp_dt2[14:679,9])
      temp_nm2 <- sub(" ","",temp_nm2)
    }else{
      temp_nm2 <- temp_dt2[10:675,12]
    }
    hes_var_nm2 <- cbind(hes_var_nm2, temp_nm2)
    
    # Extract the dates of each month from the first row of temp_dt.
    if (i==2020 && j<11){
      temp_dates <- conv_list(temp_dt[1,16:(ncol(temp_dt)-2-is.na(temp_dt[1,ncol(temp_dt)]))]) # Sometimes the last columns are NAs. In that case, exclude that column.
    }else{
      temp_dates <- conv_list(temp_dt[1,14:(ncol(temp_dt)-2-is.na(temp_dt[1,ncol(temp_dt)]))]) # Sometimes the last columns are NAs. In that case, exclude that column.
    }
    # Extend the time series of dates. 
    hes_var_date_all <- c(hes_var_date_all, paste0(i,"-",j,"-",temp_dates))
  }
}

# Convert hes_var_date_all into yyyy/m/d format.
hes_var_date_all <- conv_date_format_2(hes_var_date_all)


# Check if the data labels are the same for all months.
for (i in 2:ncol(hes_var_nm)){
  if (sum(!(hes_var_nm[,i]==hes_var_nm[,1]))>0){
    print((hes_var_nm[,i]==hes_var_nm[,1]))
    print(i)
    stop("Data labels for household expeditures are not the same for all month,")
  }
  if (sum(!(hes_var_nm2[,i]==hes_var_nm2[,1]))>0){
    print((hes_var_nm2[,i]==hes_var_nm2[,1]))
    print(i)
    stop("Data labels for detailed household expeditures are not the same for all month,")
  }
}


##### Compute the effective reproduction number using the simplified formula presented by Robert Koch Institute.

# Extract date from March 1, 2020.
R_date <- newcases_date[which(newcases_date=="2020/3/1"):length(newcases_date)]



#### Temperature and humidity data; Daily; 2020 Jan. 1-; Unit: %. 

temper_var_all <- NULL # Initialize a matrix for temperatures across the capitals of prefectures.
hum_var_all <- NULL # Initialize a matrix for humidity across the capitals of prefectures.
weath_date_all <- NULL # Initialize a matrix for the dates of weather data.

# Set the orders of the capitals of prefectures.
weath_pref_nm <- c("札幌", "青森", "盛岡", "秋田", "仙台", "山形", "福島", "水戸", "宇都宮", "さいたま", "千葉", "東京", "新潟", "前橋", "長野", "甲府", "横浜", "静岡", "富山", "岐阜", "名古屋", "金沢", "福井", "大津", "津", "奈良", "和歌山", "大阪", "京都", "神戸", "鳥取", "岡山", "松江", "広島", "山口", "高松", "松山", "徳島", "高知", "福岡", "大分", "宮崎", "佐賀", "熊本", "鹿児島", "長崎", "那覇") 
# Translate the names of capitals of prefectures into the names of prefectures.
weath_pref_nm2 <- c("北海道", "青森県", "岩手県", "秋田県", "宮城県", "山形県", "福島県", "茨城県", "栃木県", "埼玉県", "千葉県", "東京都", "新潟県", "群馬県", "長野県", "山梨県", "神奈川県", "静岡県", "富山県", "岐阜県", "愛知県", "石川県", "福井県", "滋賀県", "三重県", "奈良県", "和歌山県", "大阪府", "京都府", "兵庫県", "鳥取県", "岡山県", "島根県", "広島県", "山口県", "香川県", "愛媛県", "徳島県", "高知県", "福岡県", "大分県", "宮崎県", "佐賀県", "熊本県", "鹿児島県", "長崎県", "沖縄県") 

for (i in 2020:hes_end_all[1]){ # Weather data are used only up to the last month of the sample period for household expenditure data. 

  if (i == hes_end_all[1]) {
    # The last year in the sample of weather data.
    # Weather data are used only up to the last month of the sample period for household expenditure data. 
    temp_mt <- c(1,hes_end_all[2]) # The first and last month of the year available in the sample.
  }else{
    temp_mt <- c(1,12) # The first and last month of the year available in the sample.
  }
  for (j in temp_mt[1]:temp_mt[2]){
    # Extract temperature and humidity data for month j, year i.
    # For an unknown reason, an empty column is read at the end. The last data column is the fourth to the last column.
    eval(parse(text=paste0("temp_dt <- read.csv(\"./data/weather/weather_",i,"_",j,".csv\", header=F,stringsAsFactors=FALSE)"))) 
    
    # Convert the list containing data into a numeric matrix for temperature.
    # Blank rows in the csv file are already excluded by read.csv().
    temp <- conv_list(temp_dt[5:(nrow(temp_dt)-(temp_dt[nrow(temp_dt),1]=="")),seq(2, 1+47*6-3, by=6)]) # Exclude the last row, if it is empty.
    # Sort weather data in the order of weath_pref_nm. 
    temp_temper <- temp * NA # Initialize the container of sorted temperature data for the current month.
    for (k in 1:47){
      temp_temper[,k] <- temp[,as.character(temp_dt[2,seq(2, 1+47*6-3, by=6)])==weath_pref_nm[k]]
    }
    
    # Convert the list containing data into a numeric matrix for humidity.
    # Blank rows in the csv file are already excluded by read.csv().
    temp <- conv_list(temp_dt[5:(nrow(temp_dt)-(temp_dt[nrow(temp_dt),1]=="")),seq(5, 1+47*6, by=6)]) # Exclude the last row, if it is empty.
    # Sort weather data in the order of weath_pref_nm. 
    # No humidity data for Saitama and Ohtsu. Replace them by the data for Tokyo and Kyoto, respectively.
    temp_hum <- temp * NA # Initialize the container of sorted humidity data for the current month.
    for (k in 1:47){
      if (weath_pref_nm[k]=="さいたま"){
        temp_hum[,k] <- temp[,as.character(temp_dt[2,seq(5, 1+47*6, by=6)])=="東京"]
      }else if (weath_pref_nm[k]=="大津"){
        temp_hum[,k] <- temp[,as.character(temp_dt[2,seq(5, 1+47*6, by=6)])=="京都"]
      }else {
        temp_hum[,k] <- temp[,as.character(temp_dt[2,seq(5, 1+47*6, by=6)])==weath_pref_nm[k]]
      }
    }
    # Extend the time series of weather data.
    temper_var_all <- rbind(temper_var_all,temp_temper)
    hum_var_all <- rbind(hum_var_all,temp_hum)
    
    # Extract the dates of each month from the first row of temp_dt.
    temp_dates <- temp_dt[5:(nrow(temp_dt)-(temp_dt[nrow(temp_dt),1]=="")),1] # Sometimes the last columns are NAs. In that case, exclude that column.
    # Extend the time series of dates. 
    weath_date_all <- c(weath_date_all, temp_dates)
  }   
}



#### Population data; Annual. October 1 in 2019 (the most recently available). Unit: a thousand persons.

# Set the orders of prefectures, which must be in accordance with weath_pref_nm.
popu_pref_nm <- c("北海道", "青森県", "岩手県", "秋田県", "宮城県", "山形県", "福島県", "茨城県", "栃木県", "埼玉県", "千葉県", "東京都", "新潟県", "群馬県", "長野県", "山梨県", "神奈川県", "静岡県", "富山県", "岐阜県", "愛知県", "石川県", "福井県", "滋賀県", "三重県", "奈良県", "和歌山県", "大阪府", "京都府", "兵庫県", "鳥取県", "岡山県", "島根県", "広島県", "山口県", "香川県", "愛媛県", "徳島県", "高知県", "福岡県", "大分県", "宮崎県", "佐賀県", "熊本県", "鹿児島県", "長崎県", "沖縄県")


# Extract population share across prefectures.
temp_dt <- read.csv("./data/population_share_2019.csv", header=F,stringsAsFactors=FALSE)
# Extract prefectures' populations for all genders, all populations, all residents.
temp <- as.numeric(temp_dt[(temp_dt[,4]=="男女計" & temp_dt[,6]=="総数" & temp_dt[,8]=="総人口"), 14])
# Extract prefectures' names for all genders, all populations, all residents.
temp_nm <- temp_dt[(temp_dt[,4]=="男女計" & temp_dt[,6]=="総数" & temp_dt[,8]=="総人口"), 10]
# Record the total population in 2019.
JPN_popu_2019 <- temp[which(temp_nm=="全国")]
# Divide each prefecture's population by the nationwide population to compute population shares across prefectures.
temp <- temp[-which(temp_nm=="全国")]/JPN_popu_2019
# Sort the order of prefectures.
popu_share <- rep(NA,47) # Initialize the vector to contain population shares across prefectures.s
for (i in 1:47){
  popu_share[i] <- temp[temp_nm[-(temp_nm=="全国")]==popu_pref_nm[i]]
}

### Load monthly CPI data to convert 2019 prices into 2020 prices. Monthly; Jan. 1970-; Unit: 2015 average = 100.

# Define the data to extract.
CPI_nm <- c("一般外食", "宿泊料", "被服及び履物","入場・観覧・ゲーム代","総合")

# Initialize a matrix for CPI for household expenditure item variables. # Rows: household expenditure items. Columns: month.
CPI_m <- NULL 

# Initialize a vector for CPI weights for household expenditure item variables. # the same order as household expenditure items in CPI_m.
CPI_weight_m <- NULL 

# Load data.
temp_dt <- read.csv("./data/CPI/zmi2015a.csv", header=F,stringsAsFactors=FALSE)

# Extract data from Jan 2019 onward.
for (i in 1:length(CPI_nm)){
  # Extract CPI.
  temp <- temp_dt[which(temp_dt[,1]==201901):nrow(temp_dt),which(temp_dt[1,]==CPI_nm[i])]
  if (length(dim(temp))>1){
    temp <- temp[,1] # If there are two columns with the same title, use the first one.
  }
  CPI_m <- cbind(CPI_m,temp) 
  
  # Extract the CPI weight.
  temp <- temp_dt[which(temp_dt[,1]=="ウエイト(Weight)"),which(temp_dt[1,]==CPI_nm[i])]
  if (length(dim(temp))>1){
    temp <- temp[,1] # If there are two columns with the same title, use the first one.
  }
  CPI_weight_m <- c(CPI_weight_m,temp) 
  
}


# Turn characters into numeric. Also transpose it to place months over columns.
CPI_m <- t(matrix(as.numeric(CPI_m),nc=length(CPI_nm)))

# Turn characters into numeric.
CPI_weight_m <- as.numeric(CPI_weight_m)






########## Reform the 2020-21 data ###########################

# Substitute missing data in hum_var. Interpolate them with data for neighboring dates.
# 2020/5/6: Kobe.
# 2020/12/10: Kumamoto.
# 2021/6/20: Yokohama.
hum_var_all[weath_date_all=="2020/5/6", weath_pref_nm=="神戸"] <- (hum_var_all[weath_date_all=="2020/5/5", weath_pref_nm=="神戸"] + hum_var_all[weath_date_all=="2020/5/7", weath_pref_nm=="神戸"]) / 2
hum_var_all[weath_date_all=="2020/12/10", weath_pref_nm=="熊本"] <- (hum_var_all[weath_date_all=="2020/12/9", weath_pref_nm=="熊本"] + hum_var_all[weath_date_all=="2020/12/11", weath_pref_nm=="熊本"]) / 2
hum_var_all[weath_date_all=="2021/6/20", weath_pref_nm=="横浜"] <- (hum_var_all[weath_date_all=="2021/6/19", weath_pref_nm=="横浜"] + hum_var_all[weath_date_all=="2021/6/21", weath_pref_nm=="横浜"]) / 2

# Convert relative humidity into absolute humidity.
abs_hum_var_all <- calc_abs_hum(temper_var_all,hum_var_all)

# Compute the nationwide weighted averages of temperature and humidity.

if (flag_pref_wgt == 0){
  # Use the number of new cases in the past 7 days for the weights for prefectures. 
  temp <- rbind(matrix(1,nr=31+14,nc=1)%*%TstPstv_pref_share[1,],TstPstv_pref_share) # Fill the first row of the data for the weights between 2020 Jan. 1 and 2020 Feb. 14, one day before the first date of the sample period for the data. 
  # Extract the weights for the duration of weather data.
  temp <- temp[1:(31+14+which(TstPstv_pref_share_date==weath_date[length(weath_date)])),]
  temper_var_ave_all <- rowSums(temper_var_all * temp) 
  hum_var_ave_all <- rowSums(hum_var_all * temp) 
  abs_hum_var_ave_all <- rowSums(abs_hum_var_all * temp) 
  ind_abs_hum_var_ave_all <- rowSums((abs_hum_var_all<ah_ub) * temp) # = 1 if absolute humidity < ah_ub g/m^3, which is based on Nottmeyer et al (2020). Then take the nationwide average of the indicator. 
  ind_temper_var_ave_all <- rowSums((temper_var_all<temper_ub) * temp) # = 1 if temperature < temper_ub Celsius degree, which is based on Nottmeyer et al (2020). Then take the nationwide average of the indicator. 
}else{
  # Use population shares for the weights for prefectures. 
  temper_var_ave_all <- c(temper_var_all %*% popu_share) 
  hum_var_ave_all <- c(hum_var_all %*% popu_share) 
  abs_hum_var_ave_all <- c(abs_hum_var_all %*% popu_share) 
  ind_abs_hum_var_ave_all <- c((abs_hum_var_all<ah_ub) %*% popu_share) # = 1 if absolute humidity < ah_ub g/m^3, which is based on Nottmeyer et al (2020). Then take the nationwide average of the indicator. 
  ind_temper_var_ave_all <- c((temper_var_all<temper_ub) %*% popu_share) # = 1 if temperature < temper_ub Celsius degree, which is based on Nottmeyer et al (2020). Then take the nationwide average of the indicator. 
}




# Compute the 2020 average price for each item. 13th column is Jan 2020.
temp <- apply(CPI_m[,13:24],1,mean) # Compute the 2020 average.
CPI_m_2020 <- apply(CPI_m,2,function(x){x/temp}) # Set the benchmark of CPI to 2020.

# Interpolate the CPI for hotel services during the GO-TO-TRAVEL period in order to convert "宿泊料" in household expenditure into real term. 
# "一般外食” in CPI is used for "食事代", "喫茶代", "飲酒代"; interporated "宿泊料" in CPI for "宿泊料" in household expenditure; "宿泊料" in CPI for "国内パック旅行" in household expenditure.; "被服及び履物" in CPI for "被服及び履物" in household expenditure.
# GO-TO-TRAVEL period: 2020 July 22 to 2020 Dec 27. 
temp <- CPI_m_2020[2,12+7] + (CPI_m_2020[2,25]-CPI_m_2020[2,12+7])/6 * 1:5 # Linear interpolation of CPI for "宿泊料".
CPI_m_2020_rvs <- rbind(CPI_m_2020[1,],CPI_m_2020[2,],CPI_m_2020[2,],CPI_m_2020[3,]) # Insert one more row for "宿泊料".
CPI_m_2020_rvs[2,(12+8):24] <- temp # Insert the interporated CPI series for "宿泊料" in household expenditures

# Add CPI for "入場・観覧・ゲーム代" to CPI_m_2020_rvs.
CPI_m_2020_rvs <- rbind(CPI_m_2020_rvs,CPI_m_2020[which(CPI_nm=="入場・観覧・ゲーム代"),])




# Construct CPI for the residual household expenditures after removing those included in CPI_nm except "総合".
temp_num <- CPI_m_2020[which(CPI_nm=="総合"),] * CPI_weight_m[which(CPI_nm=="総合")] # Initialize the numerator of the CPI index.
temp_den <- CPI_weight_m[which(CPI_nm=="総合")] # Initialize the denominator of the CPI index.

for (i in 1:length(CPI_nm)){
  if (CPI_nm[i]!="総合"){
    temp_num <- temp_num - CPI_m_2020[i,] * CPI_weight_m[i] # Multiply each individual CPI index with the corresponding weights.
    temp_den <- temp_den - CPI_weight_m[i] # Add up the weight of each individual CPI index.
  }
}



# Add CPI for the residual household expenditures after removing those included in CPI_nm except "総合".
CPI_m_2020_rvs <- rbind(CPI_m_2020_rvs, temp_num/temp_den)




# Convert monthly CPI into daily CPI from Jan 2019.

temp_date <- c(ndays_normal,ndays_olympic,rep(ndays_normal,(hes_end_all[1]-2021)),ndays_normal[1:hes_end_all[2]]) # Create a series of date from Jan. 2020 to the end of the last month in the sample period.
CPI_d_2020 <- matrix(1,nc=temp_date[1]) %x% CPI_m_2020_rvs[,1] # Initialize a matrix to contain daily CPI.
for (i in 2: length(temp_date)){
  CPI_d_2020 <- cbind(CPI_d_2020, matrix(1,nc=temp_date[i]) %x% CPI_m_2020_rvs[,i]) # Fill all the dates in a month by the month's CPI for each item.
}





########## Set up household expenditure variables for the regression part 1/2 ###########################

# Set the last date of the estimation period.
hes_end_date_estimation <- paste0(hes_end[1],"/",hes_end[2],"/",ndays_normal[hes_end[2]]) # For household expenditures.
weath_end_date_estimation <- paste0(hes_end[1],"/",hes_end[2],"/",ndays_normal[hes_end[2]]) # For absolute humidity.

# Define the end of the available sample period in the weather date format.
weath_end_date_allsmpl <- paste0(hes_end_all[1],"/",hes_end_all[2],"/",ndays_normal[hes_end_all[2]])

# Compute the residual household expenditure in nominal terms.
H_expvals_all_nominal <- rbind(hes_var2_all[hes_var_nm2[,1]=="食事代",],
                               hes_var2_all[hes_var_nm2[,1]=="喫茶代",],
                               hes_var2_all[hes_var_nm2[,1]=="飲酒代",],
                               hes_var2_all[hes_var_nm2[,1]=="宿泊料",],
                               hes_var2_all[hes_var_nm2[,1]=="国内パック旅行費",],
                               t(apply(hes_var2_all[hes_var_nm2[,1]=="被服及び履物",],2,mean)), #This label appears multiple times.
                               hes_var2_all[hes_var_nm2[,1]=="入場・観覧・ゲーム代",]
)


# Add the residual household expenditure.
temp <- colSums(H_expvals_all_nominal) # Compute the sum of household expenditures listed above for each date.
H_expvals_all_nominal <- rbind(H_expvals_all_nominal, hes_var2_all[hes_var_nm2[,1]=="消費支出",] - temp) # Add the residual household expenditure.



# Create a matrix of household expenditure items for estimation. 
if (nominal_hes == 1){
  
  # Nominal household expenditures are used for explanatory variables.
  H_expvals_all <- H_expvals_all_nominal
  
}else{
  
  # Compute real household expenditure values in 2020 CPI average for each item.
  # CPI_d_2020 contains "一般外食”, interpolated "宿泊料" to remove the effect of GO-TO-TRAVEL, "宿泊料", "被服及び履物" in CPI.
  # The sample period of CPI_d_2020 starts from Jan. 1, 2019.
  
  H_CPI_all <- rbind(CPI_d_2020[1, 366:dim(CPI_d_2020)[2]], #From 2020 Jan 1. For "食事代”
                     CPI_d_2020[1, 366:dim(CPI_d_2020)[2]], #For "喫茶代”
                     CPI_d_2020[1, 366:dim(CPI_d_2020)[2]], #For "飲酒代”
                     CPI_d_2020[2, 366:dim(CPI_d_2020)[2]], #For "宿泊料”
                     CPI_d_2020[3, 366:dim(CPI_d_2020)[2]], #For "国内パック旅行費”　
                     CPI_d_2020[4, 366:dim(CPI_d_2020)[2]], #For "被服及び履物”
                     CPI_d_2020[5, 366:dim(CPI_d_2020)[2]], #For "入場・観覧・ゲーム代”
                     CPI_d_2020[6, 366:dim(CPI_d_2020)[2]]) #For the residual household expenditure.
  
  
  H_expvals_all <- matrix(NA, nr=nrow(H_CPI_all), nc=ncol(H_CPI_all)) # Initialize the matrix to contain real household expenditures included in explanatory variables for estimation.
  
  # Fulfill each row with each real household expenditure.
  for (i in 1:nrow(H_CPI_all)){
    H_expvals_all[i,] <- H_expvals_all_nominal[i,] / H_CPI_all[i,]
  }
  
}

# To increase the number of digit of coefficients to avoid the effect of possible rounding error. The unit is 100 yen (in current prices if nominal or 2020 average prices if real.) 
H_expvals_all <- H_expvals_all / 100 

# Ensure that the number of columns of CPI_d_2020 is the same as that of H_expvals_all to indentify error.
if (nrow(CPI_d_2020) != 6){
  stop("There is error in the construction of CPI_d_2020.")
}




########## Set up weather variables for the regression ###########################

# Choose the absolute humidity data for the use of the estimation.
# log_abs_hum must be 0, 1, 2, 3, 4. If log_abs_hum =4, no weather data is included among the regressors.
if(log_abs_hum == 1){
  W_abs_hum_all <- 1 - ind_temper_var_ave_all # Use the dummy that temperature exceeds 13 Celsius degree.
}else if(log_abs_hum == 2){
  W_abs_hum_all <- abs_hum_var_ave_all # Use the level of absolute humidity.
}else if(log_abs_hum == 3){
  W_abs_hum_all <- temper_var_ave_all # Use the level of temperature.
}else{
  W_abs_hum_all <- 1 - ind_abs_hum_var_ave_all # Use the dummy that absolute humidity exceeds 9 g/m^3.
}

# Define weather data for the estimation period.
hes_var_date <- hes_var_date_all[1:which(hes_var_date_all==hes_end_date_estimation)] # Dates for household expenditures data up to the date defined by hes_end.

W_abs_hum <- W_abs_hum_all[1:which(weath_date_all==weath_end_date_estimation)] # Absolute humidity data up to the date defined by hes_end.
weath_date <- weath_date_all[1:which(weath_date_all==weath_end_date_estimation)] # Dates for weather data up to the date defined by hes_end.

temper_var_ave <- temper_var_ave_all[1:which(weath_date_all==weath_end_date_estimation)] # Nation-wide average temperature data up to the date defined by hes_end.  
hum_var_ave <- hum_var_ave_all[1:which(weath_date_all==weath_end_date_estimation)] # Nation-wide average relative humidity data up to the date defined by hes_end.  
ind_abs_hum_var_ave <- ind_abs_hum_var_ave_all[1:which(weath_date_all==weath_end_date_estimation)] # 7-day backward moving averages of nation-wide dummy for absolute humidity data up to the date defined by hes_end.


# Create the distribution of lags between an infection and a symptom for the use of the estimation. 
# The empirical distribution of incubation periods from 1 days to 14 days based on MHLW data, published by Sugishita (2020).
dist_incub <- c(3,5,19,22,11,21,9,11,7,4,4,1,4,4) 
dist_incub <- rev(dist_incub/sum(dist_incub)) # Compute sample probabilities and reverse the order.


# Construct a new year dummy between 12/29-1/3 for the use of the estimation. The first date of D_NY is 2020 Feb. 15, the same as mobility report data.
D_NY <- c(rep(0, length(R_date) + 29-14)) 
D_NY[which(R_date=="2020/12/28") + 29-14 + 1:6] <- 1 # New year period.

# Construct a dummy variable for the declaration of each state of emergency for the use of the estimation. The first date is 2020 Feb. 15, the same as mobility report data.
D_SE1 <- D_SE2 <- D_SE3 <- D_SE4 <- c(rep(0, length(R_date) + 29-14)) 
D_SE1[which(R_date=="2020/4/7"):which(R_date=="2020/5/25") + 29-14] <- 1 # First declaration: 2020/4/7-2020/5/25. 

# Second declaration: 2021/1/7-2021/3/21. 
# Set time dummies for the second state of emergency only up to Jan. 2021, as the out-of-sample prediction of model estimated with data only up to Jan. 2021 shows a good fit with the realized effective reproduction number.
D_SE2[which(R_date=="2021/1/7"):which(R_date=="2021/1/31") + 29-14] <- 1

# Third declaration: 2021/4/25-2021/6/20.
D_SE3[which(R_date=="2021/4/25"):which(R_date=="2021/6/20") + 29-14] <- 1

# Fourth declaration: 2021/7/12-2021/9/30.
D_SE4[which(R_date=="2021/7/12"):min(length(D_SE4),which(R_date=="2021/9/30") + 29-14)] <- 1

# Construct a dummy variable for the period before the first state of emergency. The first date is 2020 Feb. 15, the same as mobility report data.
D_pre_SE1 <- c(rep(0, length(R_date) + 29-14)) 
D_pre_SE1[1:(which(R_date=="2020/4/6") + 29-14)] <- 1 # First declaration: The first date to 2020/4/7. 




##################### Load 2019 data ###########################

### Household expenditure survey for counter-factual; Daily; 2019 Jan. 01 - Dec.31; Unit: current yen.
# Extract household expenditure items that are used
hes_var_2019_nominal <- NULL # Initialize a matrix for nominal household expenditure item variables.
hes_var_date_2019 <- NULL # Initialize a matrix for the dates of household expenditure item variables.


# Define the names of items to extract from the the loaded table.
hes_var_nm_2019 <- c("食事代", "喫茶代","飲酒代","宿泊料","国内パック旅行費","被服及び履物","入場・観覧・ゲーム代","消費支出")

for (j in 1:12){
  # Extract household expenditure data for each month in 2019.
  # The format of the table has changed from Jan 2020.
  # For an unknown reason, an empty column is read at the end. The last data column is the fourth to the last column.
  eval(parse(text=paste0("temp_dt <- read.csv(\"./data/household survey/a616_2019_",j,".csv\", header=F,stringsAsFactors=FALSE)")))
  # Convert the list containing data into a numeric matrix.
  temp_ind <- rep(NA,length(hes_var_nm_2019)) # Initialize a vector to record the locations of items to extract.
  for (k in 1:length(hes_var_nm_2019)){
    temp_ind[k] <- min(which(temp_dt[,9]==hes_var_nm_2019[k])) # If the same label appears multiple times, choose the smallest row number.
  }
  # Extract the specified household expenditure items.
  temp_numeric <- conv_list(temp_dt[temp_ind, 11:(ncol(temp_dt)-2-is.na(temp_dt[1,ncol(temp_dt)]))]) # Sometimes the last columns are NAs. In that case, exclude that column.
  # Extend the time series of household expenditure items. Remove commas from the matrix
  hes_var_2019_nominal <- cbind(hes_var_2019_nominal,temp_numeric)
  
  # Extract the dates of each month from the first row of temp_dt.
  temp_dates <- conv_list(temp_dt[1,11:(ncol(temp_dt)-2-is.na(temp_dt[1,ncol(temp_dt)]))]) # Sometimes the last columns are NAs. In that case, exclude that column.
  # Extend the time series of dates.
  hes_var_date_2019 <- c(hes_var_date_2019, paste0(2019,"-",j,"-",temp_dates))
}

# Keep the nominal expenditures in 2019 for "食事代", "喫茶代","飲酒代","宿泊料","国内パック旅行費","被服及び履物","入場・観覧・ゲーム代".
H_expvals_2019_nominal <- hes_var_2019_nominal[1:7,]


# Add the residual household expenditure.
temp <- colSums(H_expvals_2019_nominal) # Compute the sum of household expenditures listed above for each date.
H_expvals_2019_nominal <- rbind(H_expvals_2019_nominal, hes_var_2019_nominal[hes_var_nm_2019=="消費支出",] - temp) # Add the residual household expenditure.






### Convert nominal household expenditure in 2019 into real values in 2020 CPI average price for each item.
# CPI_d_2020 starts from Jan 1. 2019.

H_CPI_2019 <- rbind(CPI_d_2020[1, 1:365], #From 2019 Jan 1. For "食事代”
                    CPI_d_2020[1, 1:365], #For "喫茶代”
                    CPI_d_2020[1, 1:365], #For "飲酒代”
                    CPI_d_2020[2, 1:365], #For "宿泊料”
                    CPI_d_2020[3, 1:365], #For "国内パック旅行費”　
                    CPI_d_2020[4, 1:365], #For "被服及び履物”
                    CPI_d_2020[5, 1:365], #For "入場・観覧・ゲーム代”
                    CPI_d_2020[6, 1:365]) #For the residual household expenditure.


if (nominal_hes == 1){
  
  # Nominal household expenditures are used for explanatory variables.
  H_expvals_2019 <- H_expvals_2019_nominal
  
}else{
  # Compute real household expenditure values in 2020 CPI average for each item.
  
  H_expvals_2019 <- matrix(NA, nr=nrow(H_CPI_2019), nc=ncol(H_CPI_2019)) # Initialize the matrix to contain real household expenditures included in explanatory variables for estimation.
  
  # Fulfill each row with each real household expenditure.
  for (i in 1:nrow(H_CPI_all)){
    H_expvals_2019[i,] <- H_expvals_2019_nominal[i,] / H_CPI_2019[i,]
  }
  
}  

# The same treatment as H_expvals: To increase the number of digit of coefficients to avoid the effect of possible rounding error.
H_expvals_2019 <- H_expvals_2019 / 100 







########  Load Survey of household economy; Daily; 2019 Jan. 01; Unit: current yen. #############
# This dataset contains household expenditures online per month and household.
#  Compute the offline share of household expenditure on clothing and footwear. 

# Initilize the matrix to record the online fraction of household expenditure on clothing and footwear.
hes_online_clothing_share_d <- NULL # The matrix in daily frequency.
hes_online_clothing_share_m <- NULL # The matrix in monthly frequency.

# Extract the data for each month.
for (i in 2019:hes_end_all[1]){
  if (i == hes_end_all[1]) {
    # The last year in the sample.
    temp_mt <- c(1,hes_end_all[2]) # The first and last month of the year available in the sample.
  }else{
    temp_mt <- c(1,12) # The first and last month of the year available in the sample.
  }
  for (j in temp_mt[1]:temp_mt[2]){
    # Extract online household expenditure on clothing and footwear for month j, year i.
    # For an unknown reason, an empty column is read at the end. The last data column is the fourth to the last column.
    eval(parse(text=paste0("temp_dt <- read.csv(\"./data/survey of household economy/1-1_",i,"_",j,".csv\", header=F,stringsAsFactors=FALSE)"))) 
    # Obtain the locations of cells to be extracted in the csv file. The format of the table changes from 2020 Nov. 
    if (i + j/12 < 2020 + 11/12){
      # The location of the column.
      temp_loc_col <- which(temp_dt[13,]=="All Japan")
      # The location of the row.
      temp_loc_rows <- !is.na(match(temp_dt[,temp_loc_col-1], c("インターネットを利用した支出総額（２２品目計）", "　５７～５９計（衣類・履物）")))
      #temp_loc_row <- which(temp_dt[,temp_loc_col-1] == "　５７～５９計（衣類・履物）")
    }else{
      # The location of the column.
      temp_loc_col <- which(temp_dt[8,]=="全国") 
      # The location of the row.
      temp_loc_rows <- !is.na(match(temp_dt[,temp_loc_col-2], c("インターネットを利用した支出総額（２２品目計）", "〔自宅用〕計（衣類・履物）")))
      #temp_loc_row <- which(temp_dt[,temp_loc_col-2] == "〔自宅用〕計（衣類・履物）")
    }
    
    # Extract the list containing data and convert it into a numeric matrix.  
    temp_numeric <- conv_list(temp_dt[temp_loc_rows,temp_loc_col]) 
    
    # Locate the dates in the month in the matrix for each type of household expenditure for which online expenditure data exist.
    # For i = 2019, the matrix is for 2019 household expenditures.
    # For i > 2019, the matrix is for 2020-2021 household expenditures.
    if (i==2019){
      if (j>2){
        loc_hes_month <- sum(ndays_normal[1:j-1])+1:ndays_normal[j]
      }else{
        loc_hes_month <- 1:ndays_normal[1]
      }
    }else if(i==2020){
      if (j>2){
        loc_hes_month <- sum(ndays_olympic[1:j-1])+1:ndays_olympic[j]
      }else{
        loc_hes_month <- 1:ndays_olympic[1]
      }
    }else{
      if (j>2){
        loc_hes_month <- 366 + (i-2021)*365 + sum(ndays_normal[1:j-1])+1:ndays_normal[j]
      }else{
        loc_hes_month <- 366 + (i-2021)*365 + 1:ndays_normal[1]
      }
    }
    
    # Compute the monthly total of the nominal household expenditure and that for clothing and footwear.
    # The first element is the total nominal household expenditure in the month to be compatible with the corresponding vector for online expenditures.
    if (i == 2019){
      temp_hes <- c(sum(H_expvals_2019_nominal[,loc_hes_month]), sum(H_expvals_2019_nominal[hes_var_nm_2019=="被服及び履物",loc_hes_month]))
    }else{
      temp_hes <- c(sum(H_expvals_all_nominal[,loc_hes_month]), sum(H_expvals_all_nominal[hes_var_nm_2019=="被服及び履物",loc_hes_month])) # hes_var_nm_2019 contains the names of household expenditure items included among the regressors in order.
    } 
    
    # Compute the online fraction of each type of household expenditure in each month, and distribute the number to each date in the month. The columns correspond to dates.
    if (i==2020){
      hes_online_clothing_share_d <- cbind(hes_online_clothing_share_d, matrix(rep(temp_numeric/temp_hes, ndays_olympic[j]), nc=ndays_olympic[j]))
    }else{
      hes_online_clothing_share_d <- cbind(hes_online_clothing_share_d, matrix(rep(temp_numeric/temp_hes, ndays_normal[j]), nc= ndays_normal[j]))
    }
    
    # Record the online fraction of each type of household expenditure in each month. 
    hes_online_clothing_share_m <- cbind(hes_online_clothing_share_m, matrix(rep(temp_numeric/temp_hes),nc=1))
  }
}



# Reform household expenditure data for the regressors.
# The online share of household expenditure for clothing and footwear is in the second row of the matrix.
# hes_var_nm_2019 contains the names of household expenditure items included among the regressors in order.
# If exc_online_cloth==1, only the offline fraction of household expenditure on clothing and footwear is included in the regressor.
if (exc_online_cloth == 1){
  H_expvals_all[hes_var_nm_2019=="被服及び履物",] <- H_expvals_all[hes_var_nm_2019=="被服及び履物",] * (1-hes_online_clothing_share_d[2,366:ncol(hes_online_clothing_share_d)])
  H_expvals_2019[hes_var_nm_2019=="被服及び履物",] <- H_expvals_2019[hes_var_nm_2019=="被服及び履物",] * (1-hes_online_clothing_share_d[2,1:365])
}

########## Set up household expenditure variables for the regression part 2/2  ###########################

# Define household expenditure data for the regression.
H_expvals <- H_expvals_all[,1:which(hes_var_date_all==hes_end_date_estimation)] # Household expenditures data up to the date defined by hes_end.


############## Load the delta-variant share of new reported cases in Tokyo ####################################
# Weekly average. For April 30 - May 2, 2021, and weekly average from May 3, 2021.

# Load data. First column: Dates (yyyy/m/d); Second column; Number of tested cases; Third column: Number of L452R detected.
temp_dt <- read.csv("./data/vaccination/TokyoDeltaScreening.csv", header=T,stringsAsFactors=FALSE)

# Extract the delta-variant share of new reported cases, including the projected numbers for future dates.
L452R_share_Tokyo_w <- temp_dt[,4]

# Remove rows with NAs.
if (sum(is.na(L452R_share_Tokyo_w))>0){
  L452R_share_Tokyo_w <- L452R_share_Tokyo_w[1:(which(is.na(L452R_share_Tokyo_w))[1]-1)]
}

# Distribute the weekly number to each date up to Nov 7, 2021.
for (i in 1:length(L452R_share_Tokyo_w)){
  if (i ==1){
    # The first period is between April 30 and May 2, 2021.
    L452R_share_Tokyo_d <- rep(L452R_share_Tokyo_w[1],3)
  }else{
    # Distribute the weekly average to each date in the week.
    L452R_share_Tokyo_d <- c(L452R_share_Tokyo_d, rep(L452R_share_Tokyo_w[i],7))
  }
}

# Fulfill zeros for the previous dates for earlier dates in 2021 up to April 29, 2021.
L452R_share_Tokyo_d <- c(rep(0, sum(ndays_normal[1:3])+29), L452R_share_Tokyo_d)




############## Load the delta-variant share of new reported cases nationwide ####################################
# Weekly average from May 31 - June 6, 2021.
# Prediction by a fitted 2nd polynomial is fulfilled for future dates.

# Load data. First column: Dates (yyyy/m/d); Second column; Number of the L452R share of new cases tested for variants.
temp_dt <- read.csv("./data/vaccination/NWDeltaScreening.csv", header=T,stringsAsFactors=FALSE)

# Extract the delta-variant share of new reported cases, including the projected numbers for future dates.
L452R_share_NW_w <- temp_dt[,2]

# Remove rows with NAs.
if (sum(is.na(L452R_share_NW_w))>0){
  L452R_share_NW_w <- L452R_share_NW_w[1:(which(is.na(L452R_share_NW_w))[1]-1)]
}

# Back out the L452R share of infection for each week.
# The weekly average of L452 share of reported new cases is regarded as the value for the mid-date of the week.
L452R_share_NW_w_infctn <- rep(NA, length(L452R_share_NW_w)) # Initialize the vector to contain the L452R share of infection on each day.
for (i in 1:length(L452R_share_NW_w)){
  if (i == 1){
    # For simplicity, assume that the first 14 days of the sample have the same value, so that it equals the L452 share of reported new cases in the next day.
    # L452R_share_Tokyo_w_infctn[1] is the average L452R share of infection for the 7 days before the mid-date of the first week of L452R_share_Tokyo_w.
    L452R_share_NW_w_infctn[1] <- L452R_share_NW_w[1]
  }else{
    # The L452R share of reported new cases = sum(dist_incub[1:7]) * The L452R share of infection for the past 8th to 14th days
    # +sum(dist_incub[8:14]) * The L452R share of infection for the past 1st to 7th days
    L452R_share_NW_w_infctn[i] <- (L452R_share_NW_w[i] - L452R_share_NW_w_infctn[i-1] * sum(dist_incub[1:7])) / sum(dist_incub[8:14])
  }
  # If the element exceeds one, it is corrected to one.
  L452R_share_NW_w_infctn[i] <- L452R_share_NW_w_infctn[i]*(L452R_share_NW_w_infctn[i] < 1) + (L452R_share_NW_w_infctn[i]>=1)
}

# Distribute the weekly number to each date.
for (i in 1:length(L452R_share_NW_w_infctn)){
  if (i ==1){
    # For simplicity, assume that the first 14 days of the sample have the same value, so that it equals the L452 share of reported new cases in the next day.
    L452R_share_NW_d <- rep(L452R_share_NW_w_infctn[1],14)
  }else{
    # Distribute the weekly average to each date in the week.
    L452R_share_NW_d <- c(L452R_share_NW_d, rep(L452R_share_NW_w_infctn[i],7))
  }
}

# If the element exceeds one, it is corrected to one.
L452R_share_NW_d <- L452R_share_NW_d*(L452R_share_NW_d<1) + (L452R_share_NW_d>=1)

# Fulfill zeros for the previous dates for earlier dates in 2021 up to May 19 (i.e., 14+1 days before the mid-date of the first week of L452R share of reported new cases, June 3), 2021.
L452R_share_NW_d <- c(rep(0, sum(ndays_normal[1:4])+19), L452R_share_NW_d)



# #################### The number of vaccinated populations in Japan for each type of vaccines ############
 
# Load the number of vaccinations for medical staff. From April 12, 2021. First column: Dates (yyyy/m/d); Second column: Number of first pfizar vaccinations; Third column: Number of first moderna vaccinations; Fourth column: Number of second pfizar vaccinations; Fifth column: Number of second moderna vaccinations;
vac_med <- read.csv("./data/vaccination/vaccination_data5_iryo.csv", header=F,stringsAsFactors=FALSE)

# Convert characters into numerics.
vac_med <- conv_list(vac_med[, 2:5])

# Remove rows with NAs.
if (sum(is.na(vac_med[,1]))>0){
  vac_med <- vac_med[1:(which(is.na(vac_med[,1]))[1]-1),]
}


# Load the number of vaccinations before April 9, 2021. From February 17, 2021. First column: Dates (yyyy/m/d); Second column: Number of first pfizar vaccinations; Third column: Number of second pfizar vaccinations;
vac_pre <- read.csv("./data/vaccination/vaccination_Feb_Apr2021.csv", header=F,stringsAsFactors=FALSE)

# Convert characters into numerics.
vac_pre <- conv_list(vac_pre[, 2:3])

# Remove rows with NAs.
if (sum(is.na(vac_pre[,1]))>0){
  vac_pre <- vac_pre[1:(which(is.na(vac_pre[,1]))[1]-1),]
}



#################### The number of vaccinated populations in Japan ############
# Data: From April, 12, 2021. Daily.

# Load data. First column: Dates (yyyy/m/d); Second column; Number of one vaccinations; Third column: Number of second vaccinations.
vac_popu <- read.csv("./data/vaccination/summary_by_date.csv", header=T,stringsAsFactors=FALSE)

# Remove rows with NAs.
if (sum(is.na(vac_popu[,1]))>0){
  vac_popu <- vac_popu[1:(which(is.na(temp_dt[,1]))[1]-1),]
}

# Keep the date (in yyyy/m/d format).
vac_popu_date <- vac_popu[,1]

### Compute the vaccinated shares of population.

# Fulfill zeros for the previous dates for earlier dates in 2021 up to April 11, 2021.
# Drop the first column, which contain dates.
vccn_persons <- rbind(matrix(0, nr=sum(ndays_normal[1:3])+11,nc=2), as.matrix(vac_popu[,2:3]))

# Add the number of vaccinations during February 17 - April 9, 2021.
vccn_persons[(31+17):(sum(ndays_normal[1:3])+9),] <- vccn_persons[(31+17):(sum(ndays_normal[1:3])+9),] + vac_pre

# Add the number of vaccinations for medical staff for April 12 - July 30, 2021.
vccn_persons[(sum(ndays_normal[1:3])+12):(sum(ndays_normal[1:6])+30),] <- vccn_persons[(sum(ndays_normal[1:3])+12):(sum(ndays_normal[1:6])+30),] + cbind(rowSums(vac_med[,1:2]), rowSums(vac_med[,3:4])) # Sum the first pfizer and moderna, and the second pfizer and moderna separately.

# Compute the first-vaccinated share of populations.
vccn_frst_share <- cumsum(vccn_persons[,1]) / JPN_popu

# Compute the second-vaccinated share of populations.
vccn_scnd_share <- cumsum(vccn_persons[,2]) / JPN_popu



####### Save the loaded data. ##############
save.image(file="COVID_data.RData")

