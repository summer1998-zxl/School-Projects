packages.used <- c("shiny","leaflet","ggplot2","tidyr","tibble","tidyverse","shinythemes",
                   "shinydashboard","sf","jsonlite","gganimate","magick","plotly","dplyr","numDeriv","DT","stringr")

# check packages that need to be installed.
packages.needed <- setdiff(packages.used, 
                           intersect(installed.packages()[,1], 
                                     packages.used))

# install additional packages
if(length(packages.needed) > 0){
  install.packages(packages.needed, dependencies = TRUE)
}

library(shiny)
library(leaflet)
library(ggplot2)
library(tidyr)
library(tibble)
library(tidyverse)
library(shinythemes)
library(shinydashboard)
library(sf)
library(jsonlite)
library(gganimate)
library(magick)
library(plotly)
library(dplyr)
library(numDeriv)
library(DT)
library(stringr)


## NYC Outdoor Activity Data ###################################

covid_vaccination = read.csv("data/Covid_Vaccine.csv") 
flu_vaccination = read.csv("data/Seasonal_Flu_Vaccinations_location.csv") 
wifi = read.csv("data/LinkNYC_Kiosk_location.csv") 
food = read.csv("data/Food_Stamp_Centers_location.csv") 
drop_in = read.csv("data/Homeless_Drop-In_Centers_location.csv") 
job = read.csv("data/After_school_programs_Jobs_and_Internships.csv") 
youth_drop_in = read.csv("data/After_school_programs_Runaway_And_Homeless_Youth.csv") 

## Covid Confirmed Cases Data for map ###################################

zip_code_database <- read.csv("output/zip_code_database.csv")
last7days.by.modzcta <-read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/latest/last7days-by-modzcta.csv")
last7days.by.modzcta <- as.data.frame(last7days.by.modzcta)
names(last7days.by.modzcta)[names(last7days.by.modzcta)=="modzcta"]<-"zip"
data <- left_join(last7days.by.modzcta, zip_code_database, by="zip")
write.csv(data,"output/casebyzipcode.csv")

data2 <- read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/totals/data-by-modzcta.csv")
load("output/geo_data.RData")

# crime data
crime_count = read.csv("data/crime_by_precinct.csv", encoding = "UTF-8") 
shape <-  read_sf('data/Police Precincts/geo_export_bd71aa2e-95c8-494a-b0db-8f41326bf675.shp')

## case plot data ###################################
#Get the raw data about the active case rate by zip code.
case_zipcode_url <- "https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/caserate-by-modzcta.csv"
case_zipcode <- read.csv(case_zipcode_url, header = TRUE, sep = ",", quote = "\"'")
case_zipcode <- as.data.frame(case_zipcode)
case_zipcode <- case_zipcode%>%
  dplyr::rename(week = week_ending,#change variables to a more clear format.
                Citywide = CASERATE_CITY,
                Bronx = CASERATE_BX,
                Brooklyn = CASERATE_BK,
                Manhattan = CASERATE_MN,
                Queens = CASERATE_QN,
                `Staten Island` = CASERATE_SI)
for (i in 8:ncol(case_zipcode)){
  colnames(case_zipcode)[i] <-  sub(".*E_", "", colnames(case_zipcode)[i])#change the zip code to a more clear format
}
case_zipcode$week <- as.Date(case_zipcode$week,"%m/%d/%Y")

perp_zipcode_url <- "https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/percentpositive-by-modzcta.csv"
perp_zipcode <- read.csv(perp_zipcode_url, header = TRUE, sep = ",", quote = "\"'")
perp_zipcode <- perp_zipcode%>%
  dplyr::rename(week = week_ending,#change variables to a more clear format.
                Citywide = PCTPOS_CITY,
                Bronx = PCTPOS_BX,
                Brooklyn = PCTPOS_BK,
                Manhattan = PCTPOS_MN,
                Queens = PCTPOS_QN,
                `Staten Island` = PCTPOS_SI)
for (i in 8:ncol(perp_zipcode)){
  colnames(perp_zipcode)[i] <-  sub(".*S_", "", colnames(perp_zipcode)[i])#change the zip code to a more clear format
}
perp_zipcode$week <- as.Date(perp_zipcode$week,"%m/%d/%Y")

case_merged <- case_zipcode[1:2]
case_merged$zipcode <- colnames(case_zipcode)[2] #Convert column names to a single variable called zipcode.
colnames(case_merged)[2] <- "Case.rate"
for (i in 3:ncol(case_zipcode)){
  new <-  case_zipcode[c(1,i)]
  new$zipcode <- colnames(case_zipcode)[i]
  colnames(new)[2] <- "Case.rate"
  case_merged <- rbind(case_merged, new)
}
case_merged$Case.rate <- case_merged$Case.rate/1000 #change the rate from case per 100,000 to case percentage.


perp_merged <- perp_zipcode[1:2]
perp_merged$zipcode <- colnames(perp_zipcode)[2]
colnames(perp_merged)[2] <- "Positive.test.rate"#Convert column names to a single variable called zipcode.
for (i in 3:ncol(perp_zipcode)){
  new <-  perp_zipcode[c(1,i)]
  new$zipcode <- colnames(perp_zipcode)[i]
  colnames(new)[2] <- "Positive.test.rate"
  perp_merged <- rbind(perp_merged, new)
}

Positive.test.rate <- perp_merged$Positive.test.rate
cp_merged <- cbind(case_merged, Positive.test.rate)#merge the case rate data with the positive test rate data.
zipcode <- colnames(case_zipcode)[2:ncol(case_zipcode)]
ratetype <- c("Case.rate", "Positive.test.rate")

#assign each zip code to its borough.
Manhattan.zip <- c("Manhattan","10026", "10027", "10030", "10037", "10039","10001", "10011", "10018", "10019", "10020", "10036","10029", "10035", "10010", "10016", "10017", "10022","10012", "10013", "10014","10004", "10005", "10006", "10007", "10038", "10280","10002", "10003", "10009",	"10021", "10028", "10044", "10065", "10075", "10128","10023", "10024", "10025","10031", "10032", "10033", "10034", "10040")

Queens.zip <- c("Queens","11361", "11362", "11363", "11364",	"11354", "11355", "11356", "11357", "11358", "11359", "11360","11365", "11366", "11367","11412", "11423", "11432", "11433", "11434", "11435", "11436","11101", "11102", "11103", "11104", "11105", "11106","11374", "11375", "11379", "11385","11691", "11692", "11693", "11694", "11695", "11697", "11004", "11005", "11411", "11413", "11422", "11426", "11427", "11428", "11429","11414", "11415", "11416", "11417", "11418", "11419", "11420", "11421","11368", "11369", "11370", "11372", "11373", "11377", "11378")

Brooklyn.zip <- c("Brooklyn","11212", "11213", "11216", "11233", "11238","11209", "11214", "11228","11204", "11218", "11219", "11230","11234", "11236", "11239",	"11223", "11224", "11229", "11235","11201", "11205", "11215", "11217", "11231","11203", "11210", "11225", "11226",	"11207", "11208",	"11211", "11222",	"11220", "11232",	"11206", "11221", "11237")

Bronx.zip <- c(	"Bronx","10453", "10457", "10460","10458", "10467", "10468","10451", "10452", "10456",	"10454", "10455", "10459", "10474",	"10463", "10471","10466", "10469", "10470", "10475","10461", "10462","10464", "10465", "10472", "10473")

Staten.Island.zip <- c ("Staten Island","10302", "10303", "10310","10306", "10307", "10308", "10309", "10312","10301", "10304", "10305","10314")

Borough <- c("Manhattan", "Queens", "Bronx", "Brooklyn", "Staten Island")

#create a variable called borough to assign each zip code to its corresponding borough in the case rate data set.
cp_merged$Borough <- ifelse(cp_merged$zipcode %in% Manhattan.zip, "Manhattan", ifelse(cp_merged$zipcode %in% Queens.zip, "Queens", ifelse(cp_merged$zipcode %in% Brooklyn.zip, "Brooklyn", ifelse(cp_merged$zipcode %in% Bronx.zip, "Bronx", ifelse(cp_merged$zipcode %in% Staten.Island.zip, "Staten Island", "Citywide")))))

#get the raw data about daily cases in NYC.
cbd_url <- "https://raw.githubusercontent.com/nychealth/coronavirus-data/master/latest/now-cases-by-day.csv"
cbd <- read.csv(cbd_url, header = TRUE, sep = ",", quote = "\"'")
cbd$date_of_interest <- as.Date(cbd$date_of_interest,"%m/%d/%Y")
cbd <- cbd%>%
  dplyr::rename(date = date_of_interest,
                Citywide = CASE_COUNT,
                Bronx = BX_CASE_COUNT,
                Brooklyn = BK_CASE_COUNT,
                Manhattan = MN_CASE_COUNT,
                Queens = QN_CASE_COUNT,
                `Staten Island` = SI_CASE_COUNT)%>%
  select(date,Citywide,Bronx,Brooklyn, Manhattan,Queens, `Staten Island`)

cbd_merged <- cbd[1:2]
cbd_merged$Borough <- colnames(cbd_merged)[2]#convert column names to a single column called Borough
colnames(cbd_merged)[2] <- "Case"
for (i in 3:ncol(cbd)){
  new <-  cbd[c(1,i)]
  new$Borough <- colnames(cbd)[i]
  colnames(new)[2] <- "Case"
  cbd_merged <- rbind(cbd_merged, new)
}

Borough_case <- c("Citywide","Manhattan", "Queens", "Bronx", "Brooklyn", "Staten Island")

#prediction function
source("trend.R")

#create predictions
predictions_perp <-  perp_zipcode[0,]
pred_vector = list(as.Date(perp_zipcode[nrow(perp_zipcode),1])+7)
for(i in names(perp_zipcode)){
   if(i=="week"){
     next
   }
   pred_vector = append(pred_vector,round(perp_zipcode[nrow(perp_zipcode),i]+ trend(perp_zipcode,perp_zipcode[i]),digits=2))
 }
 predictions_perp[1,] = pred_vector

 predictions_case2 <-  case_zipcode[0,]
 pred_vector = list(as.Date(case_zipcode[nrow(case_zipcode),1])+7)
 for(i in names(case_zipcode)){
   if(i=="week"){
     next
   }
   pred_vector = append(pred_vector,round(case_zipcode[nrow(case_zipcode),i]+ trend(case_zipcode,case_zipcode[i]),digits=2)/1000)
 }
 predictions_case2[1,] = pred_vector


 predictions_case <-  case_zipcode[0,]
 pred_vector = list(as.Date(case_zipcode[nrow(case_zipcode),1])+7)
 for(i in names(case_zipcode)){
   if(i=="week"){
     next
   }
   pred_vector = append(pred_vector,round(case_zipcode[nrow(case_zipcode),i]+ trend(perp_zipcode,perp_zipcode[i]),digits=2))
 }
 predictions_case[1,] = pred_vector

 #combine predictions_perp and predictions_case2 for plotting reasons
 predictions_combo = rbind(predictions_perp,predictions_case2)

 convert = function(type){
   if(type == "Case.rate"){
     return(2)
   }else if(type == "Positive.test.rate"){
     return(1)
   }
 }
 

 
 #prediction function
  source("trend.R")
  
  #create predictions
  predictions_perp <-  perp_zipcode[0,]
  pred_vector = list(as.Date(perp_zipcode[nrow(perp_zipcode),1])+7)
  for(i in names(perp_zipcode)){
    if(i=="week"){
      next
    }
    pred_vector = append(pred_vector,round(perp_zipcode[nrow(perp_zipcode),i]+ trend(perp_zipcode,perp_zipcode[i]),digits=2))
  }
  predictions_perp[1,] = pred_vector
  
  predictions_case2 <-  case_zipcode[0,]
  pred_vector = list(as.Date(case_zipcode[nrow(case_zipcode),1])+7)
  for(i in names(case_zipcode)){
    if(i=="week"){
      next
    }
    pred_vector = append(pred_vector,round(case_zipcode[nrow(case_zipcode),i]+ trend(case_zipcode,case_zipcode[i]),digits=2)/1000)
  }
  predictions_case2[1,] = pred_vector
 
 
  predictions_case <-  case_zipcode[0,]
  pred_vector = list(as.Date(case_zipcode[nrow(case_zipcode),1])+7)
  for(i in names(case_zipcode)){
    if(i=="week"){
      next
    }
    pred_vector = append(pred_vector,round(case_zipcode[nrow(case_zipcode),i]+ trend(perp_zipcode,perp_zipcode[i]),digits=2))
  }
  predictions_case[1,] = pred_vector
  
  #combine predictions_perp and predictions_case2 for plotting reasons
  predictions_combo = rbind(predictions_perp,predictions_case2)
  
  convert = function(type){
    if(type == "Case.rate"){
      return(2)
    }else if(type == "Positive.test.rate"){
      return(1)
    }
  }
  
  
  
  
### Statistical Analysis
  
  
  
  ########################Read data
  
  covid<-read.csv('data/Covid.csv')
  dhs<-read.csv('data/DHS_Report.csv')
  crime<-read.csv('data/18_21_Crime.csv')
  th<-read.csv('data/Students_In_Temporary_Housing.csv')
  li<-read.csv('data/Housing_for_low_income.csv')
  li_sum<-read.csv('data/Summary_of_Housing.csv')
  
  
  ###ALL VISUALIZATION WE WANT TO MAKE
  ###Plots
  ##Crime
  #I. # of crime of all 3 years vs Covid data(explain? Covid cause lower crime?)
  #II. <24 years,crime data by (UI:Borough, sex,race,crime type), vs Covid Data (Chose top 10)
  ##Temporary Housing
  #III. Connection between covid data, Housing for low income data and shelter data
  #IV. 19-21 top 10 school with most temp housing (connect with covid data)
  
  ####I. # of crime of all 3 years vs Covid data(explain? Covid cause lower crime?)
  crime$Date<-as.Date(crime$Date)
  freq1<-crime%>%filter(Age_group %in% c('<18','18-24'))
  freq1<-as.data.frame(table(freq1$Date))
  freq2<-as.data.frame(table(crime$Date))
  plotdf1<-cbind(freq1,freq2$Freq)
  names(plotdf1)<-c('Date','Youth_crime','Total_crime')
  plotdf1$Date<-as.Date(plotdf1$Date)
  
  plotdf1$Case_7_avg<-c(rep(0,752),covid$Case_7days_avg)
  avg7<-function(data){
    len<-length(data)
    res<-c()
    window<-c()
    for (i in 1:len){
      window<-c(window,data[i])
      if (length(window)>7){
        window<-window[-1]
      }
      res<-c(res,mean(window))
    }
    return(res)
  }
  plotdf1$Youth_7_avg<-avg7(plotdf1$Youth_crime)
  plotdf1$All_7_avg<-avg7(plotdf1$Total_crime)
  plotdf1$Case_7_avg[plotdf1$Case_7_avg>6000]<-6000 #For better graphing
  
  ####II. <24 years,crime data by (UI:Borough, sex,race,crime type), vs Covid Data (Chose top 10)
  plotdf2<-crime%>%filter(Age_group %in% c('<18','18-24'))
  
  crimedf<-crime%>%filter(Crime%in%c('BURGLARY','FELONY ASSAULT','GRAND LARCENY',"ROBBERY"),Age_group%in%c('<18','18-24'),Date>='2019-01-01')
  crimedf<-as.data.frame(table(crimedf$Year,crimedf$Month,crimedf$Crime))
  names(crimedf)<-c("Year","Month","Crime",'Count')
  crimedf$Date<-as.Date(paste0(crimedf$Year,'-',crimedf$Month,'-01'))
  crimedf<-crimedf%>%select(Date,Crime,Count)%>%arrange(Date)
  
  covdf3<-covid[,c(15,17,2)]
  names(covdf3)<-c('Year','Month','DailyCase')
  covdf3<-covdf3%>%group_by(Year,Month)%>%summarize(Count=sum(DailyCase))%>%filter(Year<2022)
  tp<-data.frame(Year=c(rep(2019,12),2020),Month=c(seq(1,12,1),1),Count=rep(0,13))
  covdf3<-rbind(tp,covdf3)
  covdf3$Date<-as.Date(paste0(covdf3$Year,'-',covdf3$Month,'-01'))
  covdf3<-covdf3%>%select(Date,Count)
  
  
  ####III. Connection between covid data and Housing for low income data
  ###Low income people in shelters take 8% of population in shelters in NYC
  cov_3<-covid%>%
    select(Date,Daily_Case,Case_7days_avg)%>%
    filter(Date<='2021-06-30')
  plotdf3<-li%>%
    group_by(Date)%>%
    summarize(sum_li=sum(Low_income),sum_all=sum(Total))%>%
    right_join(cov_3)%>%
    arrange(Date)%>%
    replace(.,is.na(.),0)%>%
    mutate(cum_li=cumsum(sum_li),cum_all=cumsum(sum_all))
  plotdf3$Date<-as.Date(plotdf3$Date)
  
  ####IV. 19-21 top n school with most temp housing(connect with covid data)
  th_used<-th%>%select(School,Temp_Housing,Percentage,Year)
  names(th_used)<-c("School","Count_of_Students","Percentage","Year")
  
