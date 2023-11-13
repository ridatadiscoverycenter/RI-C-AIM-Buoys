library('plyr')
library("tidyverse")
library("lubridate")
library("oce")
library('scales')
library('ggplot2')
library('ggpubr')
library('cowplot')
library('readr')
library('httr')
library('jsonlite')
library('gmailr')
library("httpuv")
library('sendmailR')
library('RMariaDB')
library('pracma')
library('dotenv')



start_time <-Sys.time() - hours(12) #subtracts 6 hours to get the time range since the last buoy data push
start_time <- parse_date_time(start_time, c("%y/%m/%d %H:%M:%S"), tz='UTC')
qc_date_range = Sys.Date()
#https://api.riddc.brown.edu/telemetry/Buoy-620/CoreMetrics/lastday
#Pull the last week of data from the Brown API for Buoy-720
System_720 =GET("https://api.riddc.brown.edu/telemetry/Buoy-720/System/lastday")
System_720 = fromJSON(rawToChar(System_720$content))

Core_720 = GET("https://api.riddc.brown.edu/telemetry/Buoy-720/CoreMetrics/lastday")
Core_720 = fromJSON(rawToChar(Core_720$content))

GPS_720= GET("https://api.riddc.brown.edu/telemetry/Buoy-720/GPSData/lastday")
GPS_720 = fromJSON(rawToChar(GPS_720$content))

Suna_720 = Core_720$SUNA
Hydrocat_720 = Core_720$Hydrocat
ECO_720 = Core_720$ECO
Hydrocycle_720 = Core_720$Hydrocycle
MetData_720 = Core_720$MetData
PAR_720 = Core_720$PAR


#Pull the last week of data from the Brown API for Buoy-620
System_620 = GET("https://api.riddc.brown.edu/telemetry/Buoy-620/System/lastday")
System_620 = fromJSON(rawToChar(System_620$content))

Core_620 = GET("https://api.riddc.brown.edu/telemetry/Buoy-620/CoreMetrics/lastday")
Core_620 = fromJSON(rawToChar(Core_620$content))

GPS_620= GET("https://api.riddc.brown.edu/telemetry/Buoy-620/GPSData/lastday")
GPS_620 = fromJSON(rawToChar(GPS_620$content))

Suna_620 = Core_620$SUNA
Hydrocat_620 = Core_620$Hydrocat
ECO_620 = Core_620$ECO
Hydrocycle_620 = Core_620$Hydrocycle
MetData_620 = Core_620$MetData
PAR_620 = Core_620$PAR


month_range <- (range(month(PAR_720$`TmStamp`))) #create month range of collected data to be used for filtering Narragansett Bay historical data by season
load_dot_env(file = "C:/Users/kgomes1/Desktop/Buoy_QC_Reports/.env")
mysql_db <- dbConnect(MariaDB(), user = Sys.getenv("MYSQLUSER"), password = Sys.getenv("MYSQLPASS") , dbname = Sys.getenv("MYSQLDATAB"), host = Sys.getenv("MYSQLHOST"), port=Sys.getenv("MYSQLPORT"))


#Perform range check for applicable variables against global historical measurement range, flag with QC Flags (1=low, 2=high, 0=good) 
global_range_data <- read.csv('C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/data_qc_global_range_values.csv', header=TRUE, sep=",", na.strings=c(""))
##global_range_data <- read.csv('C:/Users/1600x/Documents/GitHub/RI-C-AIM-Buoys/data_qc_global_range_values.csv', header=TRUE, sep=",", na.strings=c(""))

global_range_data_wide <-global_range_data %>% 
  pivot_wider(names_from = ParameterID_R, values_from =c(GlobalRangeMax, GlobalRangeMin))

global_psu_max <- max(range(global_range_data_wide$GlobalRangeMax_practical_salinity))
global_psu_min <- min(range(global_range_data_wide$GlobalRangeMin_practical_salinity))
global_temp_max <- max(range(global_range_data_wide$GlobalRangeMax_seawater_temperature))
global_temp_min <- min(range(global_range_data_wide$GlobalRangeMin_seawater_temperature))
global_chla_max <- min(range(global_range_data_wide$GlobalRangeMax_fluorometric_chlorophyll_a))
global_chla_min <- min(range(global_range_data_wide$GlobalRangeMin_fluorometric_chlorophyll_a))
global_ph_max <- min(range(global_range_data_wide$GlobalRangeMax_ph_seawater))
global_ph_min <- min(range(global_range_data_wide$GlobalRangeMin_ph_seawater))
global_oxy_max <- max(range(global_range_data_wide$GlobalRangeMax_dissolved_oxygen))
global_oxy_min <- min(range(global_range_data_wide$GlobalRangeMin_dissolved_oxygen))
global_fluoro_max <- max(range(global_range_data_wide$GlobalRangeMax_fluorometric_cdom))
global_fluoro_min <- min(range(global_range_data_wide$GlobalRangeMin_fluorometric_cdom))
global_nitrate_max <- max(range(global_range_data_wide$GlobalRangeMax_nitrate_concentration))
global_nitrate_min <- min(range(global_range_data_wide$GlobalRangeMin_nitrate_concentration))

#Perform range check for applicable variables against historical seasonal Narray Bay measurement range, flag with QC Flags  (0=within range, 1=below range, 2=above range)
nb_hist_phys_data <- read.csv('C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/NB_PhysicalData_8.10.20201.csv', header=TRUE, sep=",", na.strings=c("","nd")) #Read in historical physical data from GSO LTS measurements
##nb_hist_phys_data <- read.csv('C:/Users/1600x/Documents/GitHub/RI-C-AIM-Buoys/NB_PhysicalData_8.10.20201.csv', header=TRUE, sep=",", na.strings=c("","nd"))

nb_hist_phys_data$Date <- parse_date_time(nb_hist_phys_data$Date, c("%d-%m-%y")) #Parse Date column into date objects

nb_hist_phys_seasonal_filter <- nb_hist_phys_data %>% filter(month(nb_hist_phys_data$Date) >= min(month_range) & month(nb_hist_phys_data$Date) <= max(month_range))   #Filter historical data to seasonal time frame of buoy data

seasonal_nb_psu_range <- range(nb_hist_phys_seasonal_filter$Surface_Salinity,na.rm = TRUE)

seasonal_nb_surface_temp_range <- range(nb_hist_phys_seasonal_filter$Surface_Temp,na.rm = TRUE)


nb_hist_nutr_data <- read.csv('C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/nutrientdata_01.18.2021.csv', header=TRUE, sep=",", na.strings=c("","nd")) #Read in historical nutrient data from GSO LTS measurements
##nb_hist_nutr_data <- read.csv('C:/Users/1600x/Documents/GitHub/RI-C-AIM-Buoys/nutrientdata_01.18.2021.csv', header=TRUE, sep=",", na.strings=c("","nd"))

nb_hist_nutr_data$Date <- parse_date_time(nb_hist_nutr_data$Date, c("%m/%d/%y")) #Parse Date column into date objects

nb_hist_nutr_seasonal_filter <- nb_hist_nutr_data %>% filter(month(nb_hist_nutr_data$Date) >= min(month_range) & month(nb_hist_nutr_data$Date) <= max(month_range))   #Filter historical data to seasonal time frame of buoy data

seasonal_nb_nitrate_range <- range(nb_hist_nutr_seasonal_filter$NO3,na.rm = TRUE)

seasonal_nb_DIP_range <- range(nb_hist_nutr_seasonal_filter$DIP,na.rm = TRUE)



if((all(is.na((Hydrocat_720$hydrocatTemperature)))==FALSE)){
  Hydrocat_720$`TmStamp` <- parse_date_time(Hydrocat_720$TmStamp, c("%y/%m/%d %H:%M:%S"))
  Hydrocat_720 <- filter(Hydrocat_720,TmStamp > start_time)
  Hydrocat_720$Oxygen_despike <- despike(Hydrocat_720$`hydrocatDissOxygen`,reference = c("median"),n = 2,k = 7,replace = c("reference"))
  
  Hydrocat_720$ph_despike <- despike(Hydrocat_720$`hydrocatPH`,reference = c("median"),n = 2,k = 7,replace = c("reference"))
  
  Hydrocat_720$Temperature_despike <- despike(Hydrocat_720$`hydrocatTemperature`,reference = c("median"),n = 2,k = 7,replace = c("reference"))
  
  Hydrocat_720$salinity_despike <- despike(Hydrocat_720$`hydrocatSalinity`,reference = c("median"),n = 2,k = 7,replace = c("reference"))
  
  Hydrocat_720$turbidity_despike <- despike(Hydrocat_720$`hydrocatTurbidity`,reference = c("median"),n = 2,k = 7,replace = c("reference"))
  
  Hydrocat_720$fluorescence_despike <- despike(Hydrocat_720$`hydrocatFluorescence`,reference = c("median"),n = 2,k = 7,replace = c("reference"))
  
  Hydrocat_720 <-mutate(Hydrocat_720,
                        Salinity_Global_Range_QC =ifelse(Hydrocat_720$`salinity_despike` > global_psu_max,
                                                         "2",ifelse( Hydrocat_720$`salinity_despike` < global_psu_min,"1","0"))) 
  Hydrocat_720 <-mutate(Hydrocat_720,
                        Temp_Global_Range_QC =ifelse(Hydrocat_720$`Temperature_despike` > global_temp_max,
                                                     "2",ifelse( Hydrocat_720$`Temperature_despike` < global_temp_min,"1","0")))
  Hydrocat_720 <-mutate(Hydrocat_720,
                        ph_Global_Range_QC =ifelse(Hydrocat_720$`ph_despike` > global_ph_max,
                                                   "2",ifelse( Hydrocat_720$`ph_despike` < global_ph_min,"1","0")))
  Hydrocat_720 <-mutate(Hydrocat_720,
                        oxy_Global_Range_QC =ifelse(Hydrocat_720$`Oxygen_despike` > global_oxy_max,
                                                    "2",ifelse( Hydrocat_720$`Oxygen_despike` < global_oxy_min,"1","0")))
  Hydrocat_720 <-mutate(Hydrocat_720,
                        Salinity_Seasonal_NB_Range_QC =ifelse(Hydrocat_720$`salinity_despike` >  max(seasonal_nb_psu_range),
                                                              "2",ifelse( Hydrocat_720$`salinity_despike` < min(seasonal_nb_psu_range),"1","0"))) 
  Hydrocat_720 <-mutate(Hydrocat_720,
                        Temp_Seasonal_NB_Range_QC =ifelse(Hydrocat_720$`Temperature_despike` > max(seasonal_nb_surface_temp_range),
                                                          "2",ifelse( Hydrocat_720$`Temperature_despike` < min(seasonal_nb_surface_temp_range),"1","0")))
  #pH_720
  i<-0
  pH_Stuck_Value_QC <- c()
  for (value in Hydrocat_720$ph_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(Hydrocat_720$ph_despike[i+1],Hydrocat_720$ph_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(Hydrocat_720$ph_despike[i+1],Hydrocat_720$ph_despike[i+2],Hydrocat_720$ph_despike[i+3],Hydrocat_720$ph_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((Hydrocat_720$ph_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((Hydrocat_720$ph_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((Hydrocat_720$ph_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((Hydrocat_720$ph_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    pH_Stuck_Value_QC <- c(pH_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  Hydrocat_720$pH_Stuck_Value_QC <-pH_Stuck_Value_QC
  
  #Salinity_720
  i<-0
  Salinity_Stuck_Value_QC <- c()
  for (value in Hydrocat_720$salinity_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(Hydrocat_720$salinity_despike[i+1],Hydrocat_720$salinity_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(Hydrocat_720$salinity_despike[i+1],Hydrocat_720$salinity_despike[i+2],Hydrocat_720$salinity_despike[i+3],Hydrocat_720$salinity_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((Hydrocat_720$salinity_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((Hydrocat_720$salinity_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((Hydrocat_720$salinity_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((Hydrocat_720$salinity_despike[i]),times=4), lead_4)==FALSE)) {
      Stuck_Value_QC = 0
    }
    Salinity_Stuck_Value_QC <- c(Salinity_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  Hydrocat_720$Salinity_Stuck_Value_QC <-Salinity_Stuck_Value_QC
  
  #Oxygen_720
  i<-0
  Oxygen_Stuck_Value_QC <- c()
  for (value in Hydrocat_720$Oxygen_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(Hydrocat_720$Oxygen_despike[i+1],Hydrocat_720$Oxygen_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(Hydrocat_720$Oxygen_despike[i+1],Hydrocat_720$Oxygen_despike[i+2],Hydrocat_720$Oxygen_despike[i+3],Hydrocat_720$Oxygen_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((Hydrocat_720$Oxygen_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((Hydrocat_720$Oxygen_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((Hydrocat_720$Oxygen_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((Hydrocat_720$Oxygen_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    Oxygen_Stuck_Value_QC <- c(Oxygen_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  Hydrocat_720$Oxygen_Stuck_Value_QC <-Oxygen_Stuck_Value_QC
  
  
  #Fluorescence_720
  i<-0
  fluorescence_Stuck_Value_QC <- c()
  for (value in Hydrocat_720$fluorescence_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(Hydrocat_720$fluorescence_despike[i+1],Hydrocat_720$fluorescence_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(Hydrocat_720$fluorescence_despike[i+1],Hydrocat_720$fluorescence_despike[i+2],Hydrocat_720$fluorescence_despike[i+3],Hydrocat_720$fluorescence_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((Hydrocat_720$fluorescence_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((Hydrocat_720$fluorescence_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((Hydrocat_720$fluorescence_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((Hydrocat_720$fluorescence_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    fluorescence_Stuck_Value_QC <- c(fluorescence_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  Hydrocat_720$fluorescence_Stuck_Value_QC <-fluorescence_Stuck_Value_QC
  
  #Turbidity_720
  i<-0
  turbidity_Stuck_Value_QC <- c()
  for (value in Hydrocat_720$turbidity_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(Hydrocat_720$turbidity_despike[i+1],Hydrocat_720$turbidity_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(Hydrocat_720$turbidity_despike[i+1],Hydrocat_720$turbidity_despike[i+2],Hydrocat_720$turbidity_despike[i+3],Hydrocat_720$turbidity_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((Hydrocat_720$turbidity_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((Hydrocat_720$turbidity_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((Hydrocat_720$turbidity_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((Hydrocat_720$turbidity_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    turbidity_Stuck_Value_QC <- c(turbidity_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  Hydrocat_720$turbidity_Stuck_Value_QC <-turbidity_Stuck_Value_QC
  
  #Temperature_720
  i<-0
  Temperature_Stuck_Value_QC <- c()
  for (value in Hydrocat_720$Temperature_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(Hydrocat_720$Temperature_despike[i+1],Hydrocat_720$Temperature_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(Hydrocat_720$Temperature_despike[i+1],Hydrocat_720$Temperature_despike[i+2],Hydrocat_720$Temperature_despike[i+3],Hydrocat_720$Temperature_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((Hydrocat_720$Temperature_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((Hydrocat_720$Temperature_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((Hydrocat_720$Temperature_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((Hydrocat_720$Temperature_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    Temperature_Stuck_Value_QC <- c(Temperature_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  Hydrocat_720$Temperature_Stuck_Value_QC <-Temperature_Stuck_Value_QC
  Hydrocat_720_ph_max <- max(Hydrocat_720$ph_despike,na.rm = TRUE)
  Hydrocat_720_ph_min <-min(Hydrocat_720$ph_despike,na.rm = TRUE)
  Hydrocat_720_oxy_max <- max(Hydrocat_720$Oxygen_despike,na.rm = TRUE)
  Hydrocat_720_oxy_min <-min(Hydrocat_720$Oxygen_despike,na.rm = TRUE)
  Hydrocat_720_salinity_max <- max(Hydrocat_720$salinity_despike,na.rm = TRUE)
  Hydrocat_720_salinity_min <-min(Hydrocat_720$salinity_despike,na.rm = TRUE)
  Hydrocat_720_temp_max <- max(Hydrocat_720$Temperature_despike,na.rm = TRUE)
  Hydrocat_720_temp_min <-min(Hydrocat_720$Temperature_despike,na.rm = TRUE)
  Hydrocat_720_turb_max <- max(Hydrocat_720$turbidity_despike,na.rm = TRUE)
  Hydrocat_720_turb_min <- min(Hydrocat_720$turbidity_despike,na.rm = TRUE)
  Hydrocat_720_fluor_max <- max(Hydrocat_720$fluorescence_despike,na.rm = TRUE)
  Hydrocat_720_fluor_min <- min(Hydrocat_720$fluorescence_despike,na.rm = TRUE)
  #Hydrocat_720 <-Hydrocat_720[,c(1,2,11,16,20,26,3,4,9,18,23,5,12,15,19,22,6,14,24,7,13,25,8,10,17,21)]
  
  Hydrocat_720_qc <- sprintf("C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/Hydrocat_720_QC_%s.csv",qc_date_range)
  ##Hydrocat_720_qc <- sprintf("C:/Users/1600x/Documents/GitHub/RI-C-AIM-Buoys/Hydrocat_720_QC_%s.csv",qc_date_range)
  write.csv(Hydrocat_720,Hydrocat_720_qc)
  
  
  
  dbAppendTable(mysql_db, "QC-Buoy-720_Hydrocat",Hydrocat_720, append=TRUE)
  
  
}

if((all(is.na((Hydrocat_620$hydrocatTemperature)))==FALSE)){
  Hydrocat_620$`TmStamp` <- parse_date_time(Hydrocat_620$TmStamp, c("%y/%m/%d %H:%M:%S"))
  Hydrocat_620 <- filter(Hydrocat_620,TmStamp > start_time)
  Hydrocat_620$Oxygen_despike <- despike(Hydrocat_620$`hydrocatDissOxygen`,reference = c("median"),n = 2,k = 7,replace = c("reference"))
  
  Hydrocat_620$ph_despike <- despike(Hydrocat_620$`hydrocatPH`,reference = c("median"),n = 2,k = 7,replace = c("reference"))
  
  Hydrocat_620$Temperature_despike <- despike(Hydrocat_620$`hydrocatTemperature`,reference = c("median"),n = 2,k = 7,replace = c("reference"))
  
  Hydrocat_620$salinity_despike <- despike(Hydrocat_620$`hydrocatSalinity`,reference = c("median"),n = 2,k = 7,replace = c("reference"))
  
  Hydrocat_620$turbidity_despike <- despike(Hydrocat_620$`hydrocatTurbidity`,reference = c("median"),n = 2,k = 7,replace = c("reference"))
  
  Hydrocat_620$fluorescence_despike <- despike(Hydrocat_620$`hydrocatFluorescence`,reference = c("median"),n = 2,k = 7,replace = c("reference"))
  
  Hydrocat_620 <-mutate(Hydrocat_620,
                        Salinity_Global_Range_QC =ifelse(Hydrocat_620$`salinity_despike` > global_psu_max,
                                                         "2",ifelse( Hydrocat_620$`salinity_despike` < global_psu_min,"1","0"))) 
  Hydrocat_620 <-mutate(Hydrocat_620,
                        Temp_Global_Range_QC =ifelse(Hydrocat_620$`Temperature_despike` > global_temp_max,
                                                     "2",ifelse( Hydrocat_620$`Temperature_despike` < global_temp_min,"1","0")))
  Hydrocat_620 <-mutate(Hydrocat_620,
                        ph_Global_Range_QC =ifelse(Hydrocat_620$`ph_despike` > global_ph_max,
                                                   "2",ifelse( Hydrocat_620$`ph_despike` < global_ph_min,"1","0")))
  Hydrocat_620 <-mutate(Hydrocat_620,
                        oxy_Global_Range_QC =ifelse(Hydrocat_620$`Oxygen_despike` > global_oxy_max,
                                                    "2",ifelse( Hydrocat_620$`Oxygen_despike` < global_oxy_min,"1","0")))
  Hydrocat_620 <-mutate(Hydrocat_620,
                        Salinity_Seasonal_NB_Range_QC =ifelse(Hydrocat_620$`salinity_despike` >  max(seasonal_nb_psu_range),
                                                              "2",ifelse( Hydrocat_620$`salinity_despike` < min(seasonal_nb_psu_range),"1","0"))) 
  Hydrocat_620 <-mutate(Hydrocat_620,
                        Temp_Seasonal_NB_Range_QC =ifelse(Hydrocat_620$`Temperature_despike` > max(seasonal_nb_surface_temp_range),
                                                          "2",ifelse( Hydrocat_620$`Temperature_despike` < min(seasonal_nb_surface_temp_range),"1","0")))
  #pH_620
  i<-0
  pH_Stuck_Value_QC <- c()
  for (value in Hydrocat_620$ph_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(Hydrocat_620$ph_despike[i+1],Hydrocat_620$ph_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(Hydrocat_620$ph_despike[i+1],Hydrocat_620$ph_despike[i+2],Hydrocat_620$ph_despike[i+3],Hydrocat_620$ph_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((Hydrocat_620$ph_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((Hydrocat_620$ph_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((Hydrocat_620$ph_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((Hydrocat_620$ph_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    pH_Stuck_Value_QC <- c(pH_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  Hydrocat_620$pH_Stuck_Value_QC <-pH_Stuck_Value_QC
  
  #Salinity_620
  i<-0
  Salinity_Stuck_Value_QC <- c()
  for (value in Hydrocat_620$salinity_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(Hydrocat_620$salinity_despike[i+1],Hydrocat_620$salinity_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(Hydrocat_620$salinity_despike[i+1],Hydrocat_620$salinity_despike[i+2],Hydrocat_620$salinity_despike[i+3],Hydrocat_620$salinity_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((Hydrocat_620$salinity_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((Hydrocat_620$salinity_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((Hydrocat_620$salinity_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((Hydrocat_620$salinity_despike[i]),times=4), lead_4)==FALSE)) {
      Stuck_Value_QC = 0
    }
    Salinity_Stuck_Value_QC <- c(Salinity_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  Hydrocat_620$Salinity_Stuck_Value_QC <-Salinity_Stuck_Value_QC
  
  #Oxygen_620
  i<-0
  Oxygen_Stuck_Value_QC <- c()
  for (value in Hydrocat_620$Oxygen_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(Hydrocat_620$Oxygen_despike[i+1],Hydrocat_620$Oxygen_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(Hydrocat_620$Oxygen_despike[i+1],Hydrocat_620$Oxygen_despike[i+2],Hydrocat_620$Oxygen_despike[i+3],Hydrocat_620$Oxygen_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((Hydrocat_620$Oxygen_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((Hydrocat_620$Oxygen_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((Hydrocat_620$Oxygen_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((Hydrocat_620$Oxygen_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    Oxygen_Stuck_Value_QC <- c(Oxygen_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  Hydrocat_620$Oxygen_Stuck_Value_QC <-Oxygen_Stuck_Value_QC
  
  
  #Fluorescence_620
  i<-0
  fluorescence_Stuck_Value_QC <- c()
  for (value in Hydrocat_620$fluorescence_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(Hydrocat_620$fluorescence_despike[i+1],Hydrocat_620$fluorescence_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(Hydrocat_620$fluorescence_despike[i+1],Hydrocat_620$fluorescence_despike[i+2],Hydrocat_620$fluorescence_despike[i+3],Hydrocat_620$fluorescence_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((Hydrocat_620$fluorescence_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((Hydrocat_620$fluorescence_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((Hydrocat_620$fluorescence_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((Hydrocat_620$fluorescence_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    fluorescence_Stuck_Value_QC <- c(fluorescence_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  Hydrocat_620$fluorescence_Stuck_Value_QC <-fluorescence_Stuck_Value_QC
  
  #Turbidity_620
  i<-0
  turbidity_Stuck_Value_QC <- c()
  for (value in Hydrocat_620$turbidity_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(Hydrocat_620$turbidity_despike[i+1],Hydrocat_620$turbidity_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(Hydrocat_620$turbidity_despike[i+1],Hydrocat_620$turbidity_despike[i+2],Hydrocat_620$turbidity_despike[i+3],Hydrocat_620$turbidity_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((Hydrocat_620$turbidity_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((Hydrocat_620$turbidity_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((Hydrocat_620$turbidity_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((Hydrocat_620$turbidity_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    turbidity_Stuck_Value_QC <- c(turbidity_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  Hydrocat_620$turbidity_Stuck_Value_QC <-turbidity_Stuck_Value_QC
  
  #Temperature_620
  i<-0
  Temperature_Stuck_Value_QC <- c()
  for (value in Hydrocat_620$Temperature_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(Hydrocat_620$Temperature_despike[i+1],Hydrocat_620$Temperature_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(Hydrocat_620$Temperature_despike[i+1],Hydrocat_620$Temperature_despike[i+2],Hydrocat_620$Temperature_despike[i+3],Hydrocat_620$Temperature_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((Hydrocat_620$Temperature_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((Hydrocat_620$Temperature_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((Hydrocat_620$Temperature_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((Hydrocat_620$Temperature_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    Temperature_Stuck_Value_QC <- c(Temperature_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  Hydrocat_620$Temperature_Stuck_Value_QC <-Temperature_Stuck_Value_QC
  Hydrocat_620_ph_max <- max(Hydrocat_620$ph_despike,na.rm = TRUE)
  Hydrocat_620_ph_min <-min(Hydrocat_620$ph_despike,na.rm = TRUE)
  Hydrocat_620_oxy_max <- max(Hydrocat_620$Oxygen_despike,na.rm = TRUE)
  Hydrocat_620_oxy_min <-min(Hydrocat_620$Oxygen_despike,na.rm = TRUE)
  Hydrocat_620_salinity_max <- max(Hydrocat_620$salinity_despike,na.rm = TRUE)
  Hydrocat_620_salinity_min <-min(Hydrocat_620$salinity_despike,na.rm = TRUE)
  Hydrocat_620_temp_max <- max(Hydrocat_620$Temperature_despike,na.rm = TRUE)
  Hydrocat_620_temp_min <-min(Hydrocat_620$Temperature_despike,na.rm = TRUE)
  Hydrocat_620_turb_max <- max(Hydrocat_620$turbidity_despike,na.rm = TRUE)
  Hydrocat_620_turb_min <- min(Hydrocat_620$turbidity_despike,na.rm = TRUE)
  Hydrocat_620_fluor_max <- max(Hydrocat_620$fluorescence_despike,na.rm = TRUE)
  Hydrocat_620_fluor_min <- min(Hydrocat_620$fluorescence_despike,na.rm = TRUE)
  Hydrocat_620 <-Hydrocat_620[,c(1,2,11,16,20,26,3,4,9,18,23,5,12,15,19,22,6,14,24,7,13,25,8,10,17,21)]
  
  Hydrocat_620_qc <- sprintf("C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/Hydrocat_620_QC_%s.csv",qc_date_range)
  ##Hydrocat_620_qc <- sprintf("C:/Users/1600x/Documents/GitHub/RI-C-AIM-Buoys/Hydrocat_620_QC_%s.csv",qc_date_range)
  write.csv(Hydrocat_620,Hydrocat_620_qc)
  
  
  
  dbAppendTable(mysql_db, "QC-Buoy-620_Hydrocat",Hydrocat_620, append=TRUE)
  
  
}

if((all(is.na((Suna_720$sunaNitrateMicroMol)))==FALSE)){
  Suna_720$`TmStamp` <- parse_date_time(Suna_720$`TmStamp`, c("%y/%m/%d %H:%M:%S"))
  Suna_720 <- filter(Suna_720, TmStamp > start_time)
  Suna_720$Nitrate_despike <- despike(Suna_720$`sunaNitrateMicroMol`,reference = c("median"), n=2, k=7, replace = c("reference"))
  Suna_720 <- mutate(Suna_720,
                     Nitrate_Global_Range_QC = ifelse(Suna_720$`Nitrate_despike` > global_nitrate_max,
                                                      "2", ifelse(Suna_720$`Nitrate_despike` < global_nitrate_min,"1","0")))
  Suna_720 <- mutate(Suna_720,
                     Nitrate_Seasonal_NB_Range_QC = ifelse(Suna_720$`Nitrate_despike` > max(seasonal_nb_nitrate_range),
                                                      "2", ifelse(Suna_720$`Nitrate_despike` < min(seasonal_nb_nitrate_range),"1","0")))
  
  #Nitrate_720
  i<-0
  Nitrate_Stuck_Value_QC <- c()
  for (value in Suna_720$Nitrate_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(Suna_720$Nitrate_despike[i+1],Suna_720$Nitrate_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(Suna_720$Nitrate_despike[i+1],Suna_720$Nitrate_despike[i+2],Suna_720$Nitrate_despike[i+3],Suna_720$Nitrate_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((Suna_720$Nitrate_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((Suna_720$Nitrate_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((Suna_720$Nitrate_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((Suna_720$Nitrate_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    Nitrate_Stuck_Value_QC <- c(Nitrate_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  Suna_720$Nitrate_Stuck_Value_QC <-Nitrate_Stuck_Value_QC
  
  SUNA_720_nitrate_max <- max(Suna_720$Nitrate_despike,na.rm = TRUE)
  SUNA_720_nitrate_min <- min(Suna_720$Nitrate_despike,na.rm = TRUE)
  Suna_720_qc <- sprintf("C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/SUNA_720_QC_%s.csv",qc_date_range)
  ##Suna_720_qc <- sprintf("C:/Users/1600x/Documents/GitHub/RI-C-AIM-Buoys/SUNA_720_QC_%s.csv",qc_date_range)
  write.csv(Suna_720,Suna_720_qc)
  dbAppendTable(mysql_db, "QC-Buoy-720_SUNA",Suna_720, append=TRUE)
  
  
}

if((all(is.na((Suna_620$sunaNitrateMicroMol)))==FALSE)){
  Suna_620$`TmStamp` <- parse_date_time(Suna_620$`TmStamp`, c("%y/%m/%d %H:%M:%S"))
  Suna_620 <- filter(Suna_620, TmStamp > start_time)
  Suna_620$Nitrate_despike <- despike(Suna_620$`sunaNitrateMicroMol`,reference = c("median"), n=2, k=7, replace = c("reference"))
  Suna_620 <- mutate(Suna_620,
                     Nitrate_Global_Range_QC = ifelse(Suna_620$`Nitrate_despike` > global_nitrate_max,
                                                      "2", ifelse(Suna_620$`Nitrate_despike` < global_nitrate_min,"1","0")))
  Suna_620 <- mutate(Suna_620,
                     Nitrate_Seasonal_NB_Range_QC = ifelse(Suna_620$`Nitrate_despike` > max(seasonal_nb_nitrate_range),
                                                      "2", ifelse(Suna_620$`Nitrate_despike` < min(seasonal_nb_nitrate_range),"1","0")))
  
  #Nitrate_620
  i<-0
  Nitrate_Stuck_Value_QC <- c()
  for (value in Suna_620$Nitrate_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(Suna_620$Nitrate_despike[i+1],Suna_620$Nitrate_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(Suna_620$Nitrate_despike[i+1],Suna_620$Nitrate_despike[i+2],Suna_620$Nitrate_despike[i+3],Suna_620$Nitrate_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((Suna_620$Nitrate_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((Suna_620$Nitrate_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((Suna_620$Nitrate_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((Suna_620$Nitrate_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    Nitrate_Stuck_Value_QC <- c(Nitrate_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  Suna_620$Nitrate_Stuck_Value_QC <-Nitrate_Stuck_Value_QC
  
  SUNA_620_nitrate_max <- max(Suna_620$Nitrate_despike,na.rm = TRUE)
  SUNA_620_nitrate_min <- min(Suna_620$Nitrate_despike,na.rm = TRUE)
  Suna_620_qc <- sprintf("C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/SUNA_620_QC_%s.csv",qc_date_range)
  ##Suna_620_qc <- sprintf("C:/Users/1600x/Documents/GitHub/RI-C-AIM-Buoys/SUNA_620_QC_%s.csv",qc_date_range)
  write.csv(Suna_620,Suna_620_qc)
  dbAppendTable(mysql_db, "QC-Buoy-620_SUNA",Suna_620, append=TRUE)
  
  
}

if((all(is.na((ECO_720$ecoReadingRaw)))==FALSE)){
  ECO_720$`TmStamp` <- parse_date_time(ECO_720$TmStamp, c("%y/%m/%d %H:%M:%S"))
  ECO_720 <- filter(ECO_720,TmStamp > start_time)
  ECO_720$FDOM_despike<- despike(ECO_720$`ecoFDOM`,reference = c("median"), n=2, k=7, replace = c("reference"))
  ECO_720 <- mutate(ECO_720,
                    FDOM_Global_Range_QC = ifelse(ECO_720$`FDOM_despike` > global_fluoro_max,
                                                  "2", ifelse(ECO_720$`FDOM_despike` < global_fluoro_min,"1","0")))
  #FDOM_720
  i<-0
  FDOM_Stuck_Value_QC <- c()
  for (value in ECO_720$FDOM_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(ECO_720$FDOM_despike[i+1],ECO_720$FDOM_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(ECO_720$FDOM_despike[i+1],ECO_720$FDOM_despike[i+2],ECO_720$FDOM_despike[i+3],ECO_720$FDOM_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if (is.na(ECO_720$FDOM_despike[i]==TRUE)) {
      Stuck_Value_QC=0
    }
    else if (ECO_720$FDOM_despike[i]==0) {
      Stuck_Value_QC=0
    }
    else if(identical(rep((ECO_720$FDOM_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((ECO_720$FDOM_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((ECO_720$FDOM_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((ECO_720$FDOM_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    FDOM_Stuck_Value_QC <- c(FDOM_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  ECO_720$FDOM_Stuck_Value_QC <-FDOM_Stuck_Value_QC
  ECO_720_FDOM_max <- max(ECO_720$FDOM_despike,na.rm = TRUE)
  ECO_720_FDOM_min <- min(ECO_720$FDOM_despike,na.rm = TRUE)
  ECO_720_qc <- sprintf("C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/ECO_720_QC_%s.csv",qc_date_range)
  ##ECO_720_qc <- sprintf("C:/Users/1600x/Documents/GitHub/RI-C-AIM-Buoys/ECO_720_QC_%s.csv",qc_date_range)
  write.csv(ECO_720,ECO_720_qc)
  dbAppendTable(mysql_db, "QC-Buoy-720_ECO",ECO_720, append=TRUE)
  
}

if((all(is.na((ECO_620$ecoReadingRaw)))==FALSE)){
  ECO_620$`TmStamp` <- parse_date_time(ECO_620$TmStamp, c("%y/%m/%d %H:%M:%S"))
  ECO_620 <- filter(ECO_620,TmStamp > start_time)
  ECO_620$FDOM_despike<- despike(ECO_620$`ecoFDOM`,reference = c("median"), n=2, k=7, replace = c("reference"))
  ECO_620 <- mutate(ECO_620,
                    FDOM_Global_Range_QC = ifelse(ECO_620$`FDOM_despike` > global_fluoro_max,
                                                  "2", ifelse(ECO_620$`FDOM_despike` < global_fluoro_min,"1","0")))
  #FDOM_620
  i<-0
  FDOM_Stuck_Value_QC <- c()
  for (value in ECO_620$FDOM_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(ECO_620$FDOM_despike[i+1],ECO_620$FDOM_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(ECO_620$FDOM_despike[i+1],ECO_620$FDOM_despike[i+2],ECO_620$FDOM_despike[i+3],ECO_620$FDOM_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if (is.na(ECO_620$FDOM_despike[i]==TRUE)) {
      Stuck_Value_QC=0
    }
    else if (ECO_620$FDOM_despike[i]==0) {
      Stuck_Value_QC=0
    }
    else if(identical(rep((ECO_620$FDOM_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((ECO_620$FDOM_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((ECO_620$FDOM_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((ECO_620$FDOM_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    FDOM_Stuck_Value_QC <- c(FDOM_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  ECO_620$FDOM_Stuck_Value_QC <-FDOM_Stuck_Value_QC
  ECO_620_FDOM_max <- max(ECO_620$FDOM_despike,na.rm = TRUE)
  ECO_620_FDOM_min <- min(ECO_620$FDOM_despike,na.rm = TRUE)
  ECO_620_qc <- sprintf("C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/ECO_620_QC_%s.csv",qc_date_range)
  ##ECO_620_qc <- sprintf("C:/Users/1600x/Documents/GitHub/RI-C-AIM-Buoys/ECO_620_QC_%s.csv",qc_date_range)
  write.csv(ECO_620,ECO_620_qc)
  dbAppendTable(mysql_db, "QC-Buoy-620_ECO",ECO_620, append=TRUE)
  
}

if((all(is.na((Hydrocycle_720$CAPO4)))==FALSE)){
  Hydrocycle_720$`TmStamp` <- parse_date_time(Hydrocycle_720$`TmStamp`, c("%y/%m/%d %H:%M:%S"))
  Hydrocycle_720 <- filter(Hydrocycle_720,TmStamp > start_time)
  Hydrocycle_720$Phosphate_despike <- despike(Hydrocycle_720$'CAPO4',reference = c("median"), n=2, k=7, replace = c("reference"))
  Hydrocycle_720 <- mutate(Hydrocycle_720,
                           Phosphate_Seasonal_NB_Range_QC = ifelse(Hydrocycle_720$Phosphate_despike > max(seasonal_nb_DIP_range),
                                                                   "2", ifelse(Hydrocycle_720$Phosphate_despike < min(seasonal_nb_DIP_range),"1","0")))
  #Phosphate_720
  i<-0
  Phosphate_Stuck_Value_QC <- c()
  for (value in Hydrocycle_720$Phosphate_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(Hydrocycle_720$Phosphate_despike[i+1],Hydrocycle_720$Phosphate_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(Hydrocycle_720$Phosphate_despike[i+1],Hydrocycle_720$Phosphate_despike[i+2],Hydrocycle_720$Phosphate_despike[i+3],Hydrocycle_720$Phosphate_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((Hydrocycle_720$Phosphate_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((Hydrocycle_720$Phosphate_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((Hydrocycle_720$Phosphate_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((Hydrocycle_720$Phosphate_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    Phosphate_Stuck_Value_QC <- c(Phosphate_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  Hydrocycle_720$Phosphate_Stuck_Value_QC <-Phosphate_Stuck_Value_QC
  
  Hydrocycle_720_phosphate_max <- max(Hydrocycle_720$Phosphate_despike,na.rm = TRUE)
  Hydrocycle_720_phosphate_min <- min(Hydrocycle_720$Phosphate_despike,na.rm = TRUE)
  Hydrocycle_720_qc <- sprintf("C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/Hydrocycle_720_QC_%s.csv",qc_date_range)
  ##Hydrocycle_720_qc <- sprintf("C:/Users/1600x/Documents/GitHub/RI-C-AIM-Buoys/Hydrocycle_720_QC_%s.csv",qc_date_range)
  write.csv(Hydrocycle_720,Hydrocycle_720_qc)
  dbAppendTable(mysql_db, "QC-Buoy-720_Hydrocycle",Hydrocycle_720, append=TRUE)
}

if((all(is.na((Hydrocycle_620$CAPO4)))==FALSE)){
  Hydrocycle_620$`TmStamp` <- parse_date_time(Hydrocycle_620$`TmStamp`, c("%y/%m/%d %H:%M:%S"))
  Hydrocycle_620 <- filter(Hydrocycle_620,TmStamp > start_time)
  Hydrocycle_620$Phosphate_despike <- despike(Hydrocycle_620$'CAPO4',reference = c("median"), n=2, k=7, replace = c("reference"))
  Hydrocycle_620 <- mutate(Hydrocycle_620,
                           Phosphate_Seasonal_NB_Range_QC = ifelse(Hydrocycle_620$Phosphate_despike > max(seasonal_nb_DIP_range),
                                                                   "2", ifelse(Hydrocycle_620$Phosphate_despike < min(seasonal_nb_DIP_range),"1","0")))
  #Phosphate_620
  i<-0
  Phosphate_Stuck_Value_QC <- c()
  for (value in Hydrocycle_620$Phosphate_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(Hydrocycle_620$Phosphate_despike[i+1],Hydrocycle_620$Phosphate_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(Hydrocycle_620$Phosphate_despike[i+1],Hydrocycle_620$Phosphate_despike[i+2],Hydrocycle_620$Phosphate_despike[i+3],Hydrocycle_620$Phosphate_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((Hydrocycle_620$Phosphate_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((Hydrocycle_620$Phosphate_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((Hydrocycle_620$Phosphate_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((Hydrocycle_620$Phosphate_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    Phosphate_Stuck_Value_QC <- c(Phosphate_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  Hydrocycle_620$Phosphate_Stuck_Value_QC <-Phosphate_Stuck_Value_QC
  
  Hydrocycle_620_phosphate_max <- max(Hydrocycle_620$Phosphate_despike,na.rm = TRUE)
  Hydrocycle_620_phosphate_min <- min(Hydrocycle_620$Phosphate_despike,na.rm = TRUE)
  Hydrocycle_620_qc <- sprintf("C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/Hydrocycle_620_QC_%s.csv",qc_date_range)
  ##Hydrocycle_620_qc <- sprintf("C:/Users/1600x/Documents/GitHub/RI-C-AIM-Buoys/Hydrocycle_620_QC_%s.csv",qc_date_range)
  write.csv(Hydrocycle_620,Hydrocycle_620_qc)
  dbAppendTable(mysql_db, "QC-Buoy-620_Hydrocycle",Hydrocycle_620, append=TRUE)
}

if((all(is.na((PAR_720$PARcalibrated)))==FALSE)){
  PAR_720$`TmStamp` <- parse_date_time(PAR_720$`TmStamp`, c("%y/%m/%d %H:%M:%S"))
  PAR_720 <- filter(PAR_720,TmStamp > start_time)
  PAR_720$PARcalibrated_despike <- despike(PAR_720$PARcalibrated,reference = c("median"), n=2, k=7, replace = c("reference"))
  
  #PAR Inst Range check
  PAR_720 <- mutate(PAR_720,
                    PARcalibrated_Inst_Range_QC = ifelse(PAR_720$PARcalibrated_despike > 4000 ,
                                                         "2", ifelse(PAR_720$PARcalibrated_despike < 0,"1","0")))
  #PAR_720
  i<-0
  PARcalibrated_Stuck_Value_QC <- c()
  for (value in PAR_720$PARcalibrated_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(PAR_720$PARcalibrated_despike[i+1],PAR_720$PARcalibrated_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(PAR_720$PARcalibrated_despike[i+1],PAR_720$PARcalibrated_despike[i+2],PAR_720$PARcalibrated_despike[i+3],PAR_720$PARcalibrated_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((PAR_720$PARcalibrated_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((PAR_720$PARcalibrated_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((PAR_720$PARcalibrated_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((MetData_720$avgWindSpeed_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    PARcalibrated_Stuck_Value_QC <- c(PARcalibrated_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  PAR_720$PARcalibrated_Stuck_Value_QC <-PARcalibrated_Stuck_Value_QC
  
  
  PAR_720_qc<- sprintf("C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/PAR_720_QC_%s.csv",qc_date_range)
  write.csv(PAR_720, PAR_720_qc)
  dbAppendTable(mysql_db, "QC-Buoy-720_PAR",PAR_720, append=TRUE)
}

if((all(is.na((PAR_620$PARcalibrated)))==FALSE)){
  PAR_620$`TmStamp` <- parse_date_time(PAR_620$`TmStamp`, c("%y/%m/%d %H:%M:%S"))
  PAR_620 <- filter(PAR_620,TmStamp > start_time)
  PAR_620$PARcalibrated_despike <- despike(PAR_620$PARcalibrated,reference = c("median"), n=2, k=7, replace = c("reference"))
  
  #PAR Inst Range check
  PAR_620 <- mutate(PAR_620,
                    PARcalibrated_Inst_Range_QC = ifelse(PAR_620$PARcalibrated_despike > 4000 ,
                                                         "2", ifelse(PAR_620$PARcalibrated_despike < 0,"1","0")))
  #PAR_620
  i<-0
  PARcalibrated_Stuck_Value_QC <- c()
  for (value in PAR_620$PARcalibrated_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(PAR_620$PARcalibrated_despike[i+1],PAR_620$PARcalibrated_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(PAR_620$PARcalibrated_despike[i+1],PAR_620$PARcalibrated_despike[i+2],PAR_620$PARcalibrated_despike[i+3],PAR_620$PARcalibrated_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((PAR_620$PARcalibrated_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((PAR_620$PARcalibrated_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((PAR_620$PARcalibrated_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((MetData_620$avgWindSpeed_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    PARcalibrated_Stuck_Value_QC <- c(PARcalibrated_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  PAR_620$PARcalibrated_Stuck_Value_QC <-PARcalibrated_Stuck_Value_QC
  
  
  PAR_620_qc<- sprintf("C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/PAR_620_QC_%s.csv",qc_date_range)
  write.csv(PAR_620, PAR_620_qc)
  dbAppendTable(mysql_db, "QC-Buoy-620_PAR",PAR_620, append=TRUE)
}

if((all(is.na((MetData_720$avgWindSpeed)))==FALSE)){
  MetData_720$`TmStamp` <- parse_date_time(MetData_720$`TmStamp`, c("%y/%m/%d %H:%M:%S"))
  MetData_720 <- filter(MetData_720,TmStamp > start_time)
  MetData_720$avgWindSpeed_despike <- despike(MetData_720$avgWindSpeed,reference = c("median"), n=2, k=7, replace = c("reference"))
  MetData_720$gustWindSpeed_despike <- despike(MetData_720$gustWindSpeed,reference = c("median"), n=2, k=7, replace = c("reference"))
  MetData_720$maximetTemperature_despike <- despike(MetData_720$maximetTemperature,reference = c("median"), n=2, k=7, replace = c("reference"))
  MetData_720$maximetPressure_despike <- despike(MetData_720$maximetPressure,reference = c("median"), n=2, k=7, replace = c("reference"))
  MetData_720$maximetSolar_despike <- despike(MetData_720$maximetSolar, reference = c("median"), n=2, k=7, replace = c("reference"))
  MetData_720$maximetHumidity_despike <- despike(MetData_720$maximetHumidity, reference = c("median"), n=2, k=7, replace = c("reference"))
  MetData_720$maximetPrecipitation_despike <- despike(MetData_720$maximetPrecipitation, reference = c("median"), n=2, k=7, replace = c("reference"))
  #Maximet Inst Range check
  MetData_720 <- mutate(MetData_720,
                        avgWindSpeed_Inst_Range_QC = ifelse(MetData_720$avgWindSpeed_despike > 60 ,
                                                            "2", ifelse(MetData_720$avgWindSpeed_despike < 0.01,"1","0")))
  MetData_720 <- mutate(MetData_720,
                        gustWindSpeed_Inst_Range_QC = ifelse(MetData_720$gustWindSpeed_despike > 60,"2", ifelse(MetData_720$gustWindSpeed_despike < 0.01,"1","0")))
  
  MetData_720 <- mutate(MetData_720,
                        avgWindDir_Inst_Range_QC = ifelse(MetData_720$avgWindDir > 359 ,"2", ifelse(MetData_720$avgWindDir < 0,"1","0")))
  
  MetData_720 <- mutate(MetData_720,
                        gustWindDir_Inst_Range_QC = ifelse(MetData_720$gustWindDir > 359 ,"2", ifelse(MetData_720$gustWindDir < 0,"1","0")))
  
  MetData_720 <- mutate(MetData_720,
                        maximet_Temperature_Inst_Range_QC = ifelse(MetData_720$maximetTemperature_despike > 70 ,"2", ifelse(MetData_720$maximetTemperature_despike < -40,"1","0")))
  
  MetData_720 <- mutate(MetData_720,
                        maximetPressure_Inst_Range_QC = ifelse(MetData_720$maximetPressure_despike > 1100,"2", ifelse(MetData_720$maximetPressure_despike < 300,"1","0")))
  
  MetData_720 <- mutate(MetData_720,
                        maximetSolar_Inst_Range_QC = ifelse(MetData_720$maximetSolar_despike > 1600,"2", ifelse(MetData_720$maximetSolar_despike < 0,"1","0")))
  
  MetData_720 <- mutate(MetData_720,
                        maximetPrecipitation_Inst_Range_QC = ifelse(MetData_720$maximetPrecipitation_despike > 150,"2", ifelse(MetData_720$maximetPrecipitation_despike < 0,"1","0")))
  
  MetData_720 <- mutate(MetData_720,
                        maximetHumidity_Inst_Range_QC = ifelse(MetData_720$maximetHumidity_despike > 100,"2", ifelse(MetData_720$maximetHumidity_despike < 0,"1","0")))
  #avgWingSpeed_720
  i<-0
  avgWindSpeed_Stuck_Value_QC <- c()
  for (value in MetData_720$avgWindSpeed_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(MetData_720$avgWindSpeed_despike[i+1],MetData_720$avgWindSpeed_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(MetData_720$avgWindSpeed_despike[i+1],MetData_720$avgWindSpeed_despike[i+2],MetData_720$avgWindSpeed_despike[i+3],MetData_720$avgWindSpeed_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((MetData_720$avgWindSpeed_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((MetData_720$avgWindSpeed_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((MetData_720$avgWindSpeed_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((MetData_720$avgWindSpeed_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    avgWindSpeed_Stuck_Value_QC <- c(avgWindSpeed_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  MetData_720$avgWindSpeed_Stuck_Value_QC <-avgWindSpeed_Stuck_Value_QC
  
  
  #avgWindDir_720
  i<-0
  avgWindDir_Stuck_Value_QC <- c()
  for (value in MetData_720$avgWindDir) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(MetData_720$avgWindDir[i+1],MetData_720$avgWindDir[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(MetData_720$avgWindDir[i+1],MetData_720$avgWindDir[i+2],MetData_720$avgWindDir[i+3],MetData_720$avgWindDir[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((MetData_720$avgWindDir[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((MetData_720$avgWindDir[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((MetData_720$avgWindDir[i]), times=2), lead_2,)==FALSE)&(identical(rep((MetData_720$avgWindDir[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    avgWindDir_Stuck_Value_QC <- c(avgWindDir_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  MetData_720$avgWindDir_Stuck_Value_QC <-avgWindDir_Stuck_Value_QC
  
  
  #gustWindSpeed_720
  i<-0
  gustWindSpeed_Stuck_Value_QC <- c()
  for (value in MetData_720$gustWindSpeed_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(MetData_720$gustWindSpeed_despike[i+1],MetData_720$gustWindSpeed_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(MetData_720$gustWindSpeed_despike[i+1],MetData_720$gustWindSpeed_despike[i+2],MetData_720$gustWindSpeed_despike[i+3],MetData_720$gustWindSpeed_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((MetData_720$gustWindSpeed_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((MetData_720$gustWindSpeed_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((MetData_720$gustWindSpeed_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((MetData_720$gustWindSpeed_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    gustWindSpeed_Stuck_Value_QC <- c(gustWindSpeed_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  MetData_720$gustWindSpeed_Stuck_Value_QC <-gustWindSpeed_Stuck_Value_QC
  
  #gustWindDir_720
  i<-0
  gustWindDir_Stuck_Value_QC <- c()
  for (value in MetData_720$gustWindDir) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(MetData_720$gustWindDir[i+1],MetData_720$gustWindDir[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(MetData_720$gustWindDir[i+1],MetData_720$gustWindDir[i+2],MetData_720$gustWindDir[i+3],MetData_720$gustWindDir[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((MetData_720$gustWindDir[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((MetData_720$gustWindDir[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((MetData_720$gustWindDir[i]), times=2), lead_2,)==FALSE)&(identical(rep((MetData_720$gustWindDir[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    gustWindDir_Stuck_Value_QC <- c(gustWindDir_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  MetData_720$gustWindDir_Stuck_Value_QC <-gustWindDir_Stuck_Value_QC
  
  #Maximet_Temp_720
  i<-0
  maximetTemperature_Stuck_Value_QC <- c()
  for (value in MetData_720$maximetTemperature_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(MetData_720$maximetTemperature_despike[i+1],MetData_720$maximetTemperature_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(MetData_720$maximetTemperature_despike[i+1],MetData_720$maximetTemperature_despike[i+2],MetData_720$maximetTemperature_despike[i+3],MetData_720$maximetTemperature_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((MetData_720$maximetTemperature_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((MetData_720$maximetTemperature_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((MetData_720$maximetTemperature_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((MetData_720$maximetTemperature_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    maximetTemperature_Stuck_Value_QC <- c(maximetTemperature_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  MetData_720$maximetTemperature_Stuck_Value_QC <-maximetTemperature_Stuck_Value_QC
  
  #Maximet_Pressure_720
  i<-0
  maximetPressure_Stuck_Value_QC <- c()
  for (value in MetData_720$maximetPressure_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(MetData_720$maximetPressure_despike[i+1],MetData_720$maximetPressure_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(MetData_720$maximetPressure_despike[i+1],MetData_720$maximetPressure_despike[i+2],MetData_720$maximetPressure_despike[i+3],MetData_720$maximetPressure_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((MetData_720$maximetPressure_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((MetData_720$maximetPressure_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((MetData_720$maximetPressure_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((MetData_720$maximetPressure_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    maximetPressure_Stuck_Value_QC <- c(maximetPressure_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  MetData_720$maximetPressure_Stuck_Value_QC <-maximetPressure_Stuck_Value_QC
  
  #Maximet_Solar_720
  i<-0
  maximetSolar_Stuck_Value_QC <- c()
  for (value in MetData_720$maximetSolar_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(MetData_720$maximetSolar_despike[i+1],MetData_720$maximetSolar_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(MetData_720$maximetSolar_despike[i+1],MetData_720$maximetSolar_despike[i+2],MetData_720$maximetSolar_despike[i+3],MetData_720$maximetSolar_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if (MetData_720$maximetSolar_despike[i]==0) {
      Stuck_Value_QC=0
    }
    else if(identical(rep((MetData_720$maximetSolar_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((MetData_720$maximetSolar_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((MetData_720$maximetSolar_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((MetData_720$maximetSolar_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    maximetSolar_Stuck_Value_QC <- c(maximetSolar_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  MetData_720$maximetSolar_Stuck_Value_QC <-maximetSolar_Stuck_Value_QC
  
  #Maximet_Precip_720
  i<-0
  maximetPrecipitation_Stuck_Value_QC <- c()
  for (value in MetData_720$maximetPrecipitation_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(MetData_720$maximetPrecipitation_despike[i+1],MetData_720$maximetPrecipitation_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(MetData_720$maximetPrecipitation_despike[i+1],MetData_720$maximetPrecipitation_despike[i+2],MetData_720$maximetPrecipitation_despike[i+3],MetData_720$maximetPrecipitation_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if (MetData_720$maximetPrecipitation_despike[i]==0) {
      Stuck_Value_QC=0
    }
    else if(identical(rep((MetData_720$maximetPrecipitation_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((MetData_720$maximetPrecipitation_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((MetData_720$maximetPrecipitation_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((MetData_720$maximetPrecipitation_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    maximetPrecipitation_Stuck_Value_QC <- c(maximetPrecipitation_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  MetData_720$maximetPrecipitation_Stuck_Value_QC <-maximetPrecipitation_Stuck_Value_QC
  
  i<-0
  maximetHumidity_Stuck_Value_QC <- c()
  for (value in MetData_720$maximetHumidity_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(MetData_720$maximetHumidity_despike[i+1],MetData_720$maximetHumidity_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(MetData_720$maximetHumidity_despike[i+1],MetData_720$maximetHumidity_despike[i+2],MetData_720$maximetHumidity_despike[i+3],MetData_720$maximetHumidity_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((MetData_720$maximetHumidity_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((MetData_720$maximetHumidity_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((MetData_720$maximetHumidity_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((MetData_720$maximetHumidity_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    maximetHumidity_Stuck_Value_QC <- c(maximetHumidity_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  MetData_720$maximetHumidity_Stuck_Value_QC <-maximetHumidity_Stuck_Value_QC
  MetData_720 <-MetData_720[,c(1,2,3,12,19,28,4,21,29,5,13,20,30,6,22,31,7,14,23,32,8,15,24,33,9,17,27,36,10,18,26,35,11,16,25,34)]
  MetData_720_qc<- sprintf("C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/MetData_720_QC_%s.csv",qc_date_range)
  write.csv(MetData_720, MetData_720_qc)
  dbAppendTable(mysql_db, "QC-Buoy-720_MetData",MetData_720, append=TRUE)
}

if((all(is.na((MetData_620$avgWindSpeed)))==FALSE)){
  MetData_620$`TmStamp` <- parse_date_time(MetData_620$`TmStamp`, c("%y/%m/%d %H:%M:%S"))
  MetData_620 <- filter(MetData_620,TmStamp > start_time)
  MetData_620$avgWindSpeed_despike <- despike(MetData_620$avgWindSpeed,reference = c("median"), n=2, k=7, replace = c("reference"))
  MetData_620$gustWindSpeed_despike <- despike(MetData_620$gustWindSpeed,reference = c("median"), n=2, k=7, replace = c("reference"))
  MetData_620$maximetTemperature_despike <- despike(MetData_620$maximetTemperature,reference = c("median"), n=2, k=7, replace = c("reference"))
  MetData_620$maximetPressure_despike <- despike(MetData_620$maximetPressure,reference = c("median"), n=2, k=7, replace = c("reference"))
  MetData_620$maximetSolar_despike <- despike(MetData_620$maximetSolar, reference = c("median"), n=2, k=7, replace = c("reference"))
  MetData_620$maximetHumidity_despike <- despike(MetData_620$maximetHumidity, reference = c("median"), n=2, k=7, replace = c("reference"))
  MetData_620$maximetPrecipitation_despike <- despike(MetData_620$maximetPrecipitation, reference = c("median"), n=2, k=7, replace = c("reference"))
  #Maximet Inst Range check
  MetData_620 <- mutate(MetData_620,
                        avgWindSpeed_Inst_Range_QC = ifelse(MetData_620$avgWindSpeed_despike > 60 ,
                                                            "2", ifelse(MetData_620$avgWindSpeed_despike < 0.01,"1","0")))
  MetData_620 <- mutate(MetData_620,
                        gustWindSpeed_Inst_Range_QC = ifelse(MetData_620$gustWindSpeed_despike > 60,"2", ifelse(MetData_620$gustWindSpeed_despike < 0.01,"1","0")))
  
  MetData_620 <- mutate(MetData_620,
                        avgWindDir_Inst_Range_QC = ifelse(MetData_620$avgWindDir > 359 ,"2", ifelse(MetData_620$avgWindDir < 0,"1","0")))
  
  MetData_620 <- mutate(MetData_620,
                        gustWindDir_Inst_Range_QC = ifelse(MetData_620$gustWindDir > 359 ,"2", ifelse(MetData_620$gustWindDir < 0,"1","0")))
  
  MetData_620 <- mutate(MetData_620,
                        maximet_Temperature_Inst_Range_QC = ifelse(MetData_620$maximetTemperature_despike > 70 ,"2", ifelse(MetData_620$maximetTemperature_despike < -40,"1","0")))
  
  MetData_620 <- mutate(MetData_620,
                        maximetPressure_Inst_Range_QC = ifelse(MetData_620$maximetPressure_despike > 1100,"2", ifelse(MetData_620$maximetPressure_despike < 300,"1","0")))
  
  MetData_620 <- mutate(MetData_620,
                        maximetSolar_Inst_Range_QC = ifelse(MetData_620$maximetSolar_despike > 1600,"2", ifelse(MetData_620$maximetSolar_despike < 0,"1","0")))
  
  MetData_620 <- mutate(MetData_620,
                        maximetPrecipitation_Inst_Range_QC = ifelse(MetData_620$maximetPrecipitation_despike > 150,"2", ifelse(MetData_620$maximetPrecipitation_despike < 0,"1","0")))
  
  MetData_620 <- mutate(MetData_620,
                        maximetHumidity_Inst_Range_QC = ifelse(MetData_620$maximetHumidity_despike > 100,"2", ifelse(MetData_620$maximetHumidity_despike < 0,"1","0")))
  #avgWingSpeed_620
  i<-0
  avgWindSpeed_Stuck_Value_QC <- c()
  for (value in MetData_620$avgWindSpeed_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(MetData_620$avgWindSpeed_despike[i+1],MetData_620$avgWindSpeed_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(MetData_620$avgWindSpeed_despike[i+1],MetData_620$avgWindSpeed_despike[i+2],MetData_620$avgWindSpeed_despike[i+3],MetData_620$avgWindSpeed_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((MetData_620$avgWindSpeed_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((MetData_620$avgWindSpeed_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((MetData_620$avgWindSpeed_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((MetData_620$avgWindSpeed_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    avgWindSpeed_Stuck_Value_QC <- c(avgWindSpeed_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  MetData_620$avgWindSpeed_Stuck_Value_QC <-avgWindSpeed_Stuck_Value_QC
  
  
  #avgWindDir_620
  i<-0
  avgWindDir_Stuck_Value_QC <- c()
  for (value in MetData_620$avgWindDir) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(MetData_620$avgWindDir[i+1],MetData_620$avgWindDir[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(MetData_620$avgWindDir[i+1],MetData_620$avgWindDir[i+2],MetData_620$avgWindDir[i+3],MetData_620$avgWindDir[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((MetData_620$avgWindDir[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((MetData_620$avgWindDir[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((MetData_620$avgWindDir[i]), times=2), lead_2,)==FALSE)&(identical(rep((MetData_620$avgWindDir[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    avgWindDir_Stuck_Value_QC <- c(avgWindDir_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  MetData_620$avgWindDir_Stuck_Value_QC <-avgWindDir_Stuck_Value_QC
  
  
  #gustWindSpeed_620
  i<-0
  gustWindSpeed_Stuck_Value_QC <- c()
  for (value in MetData_620$gustWindSpeed_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(MetData_620$gustWindSpeed_despike[i+1],MetData_620$gustWindSpeed_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(MetData_620$gustWindSpeed_despike[i+1],MetData_620$gustWindSpeed_despike[i+2],MetData_620$gustWindSpeed_despike[i+3],MetData_620$gustWindSpeed_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((MetData_620$gustWindSpeed_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((MetData_620$gustWindSpeed_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((MetData_620$gustWindSpeed_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((MetData_620$gustWindSpeed_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    gustWindSpeed_Stuck_Value_QC <- c(gustWindSpeed_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  MetData_620$gustWindSpeed_Stuck_Value_QC <-gustWindSpeed_Stuck_Value_QC
  
  #gustWindDir_620
  i<-0
  gustWindDir_Stuck_Value_QC <- c()
  for (value in MetData_620$gustWindDir) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(MetData_620$gustWindDir[i+1],MetData_620$gustWindDir[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(MetData_620$gustWindDir[i+1],MetData_620$gustWindDir[i+2],MetData_620$gustWindDir[i+3],MetData_620$gustWindDir[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((MetData_620$gustWindDir[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((MetData_620$gustWindDir[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((MetData_620$gustWindDir[i]), times=2), lead_2,)==FALSE)&(identical(rep((MetData_620$gustWindDir[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    gustWindDir_Stuck_Value_QC <- c(gustWindDir_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  MetData_620$gustWindDir_Stuck_Value_QC <-gustWindDir_Stuck_Value_QC
  
  #Maximet_Temp_620
  i<-0
  maximetTemperature_Stuck_Value_QC <- c()
  for (value in MetData_620$maximetTemperature_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(MetData_620$maximetTemperature_despike[i+1],MetData_620$maximetTemperature_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(MetData_620$maximetTemperature_despike[i+1],MetData_620$maximetTemperature_despike[i+2],MetData_620$maximetTemperature_despike[i+3],MetData_620$maximetTemperature_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((MetData_620$maximetTemperature_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((MetData_620$maximetTemperature_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((MetData_620$maximetTemperature_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((MetData_620$maximetTemperature_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    maximetTemperature_Stuck_Value_QC <- c(maximetTemperature_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  MetData_620$maximetTemperature_Stuck_Value_QC <-maximetTemperature_Stuck_Value_QC
  
  #Maximet_Pressure_620
  i<-0
  maximetPressure_Stuck_Value_QC <- c()
  for (value in MetData_620$maximetPressure_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(MetData_620$maximetPressure_despike[i+1],MetData_620$maximetPressure_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(MetData_620$maximetPressure_despike[i+1],MetData_620$maximetPressure_despike[i+2],MetData_620$maximetPressure_despike[i+3],MetData_620$maximetPressure_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((MetData_620$maximetPressure_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((MetData_620$maximetPressure_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((MetData_620$maximetPressure_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((MetData_620$maximetPressure_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    maximetPressure_Stuck_Value_QC <- c(maximetPressure_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  MetData_620$maximetPressure_Stuck_Value_QC <-maximetPressure_Stuck_Value_QC
  
  #Maximet_Solar_620
  i<-0
  maximetSolar_Stuck_Value_QC <- c()
  for (value in MetData_620$maximetSolar_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(MetData_620$maximetSolar_despike[i+1],MetData_620$maximetSolar_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(MetData_620$maximetSolar_despike[i+1],MetData_620$maximetSolar_despike[i+2],MetData_620$maximetSolar_despike[i+3],MetData_620$maximetSolar_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if (MetData_620$maximetSolar_despike[i]==0) {
      Stuck_Value_QC=0
    }
    else if(identical(rep((MetData_620$maximetSolar_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((MetData_620$maximetSolar_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((MetData_620$maximetSolar_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((MetData_620$maximetSolar_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    maximetSolar_Stuck_Value_QC <- c(maximetSolar_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  MetData_620$maximetSolar_Stuck_Value_QC <-maximetSolar_Stuck_Value_QC
  
  #Maximet_Precip_620
  i<-0
  maximetPrecipitation_Stuck_Value_QC <- c()
  for (value in MetData_620$maximetPrecipitation_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(MetData_620$maximetPrecipitation_despike[i+1],MetData_620$maximetPrecipitation_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(MetData_620$maximetPrecipitation_despike[i+1],MetData_620$maximetPrecipitation_despike[i+2],MetData_620$maximetPrecipitation_despike[i+3],MetData_620$maximetPrecipitation_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if (MetData_620$maximetPrecipitation_despike[i]==0) {
      Stuck_Value_QC=0
    }
    else if(identical(rep((MetData_620$maximetPrecipitation_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((MetData_620$maximetPrecipitation_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((MetData_620$maximetPrecipitation_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((MetData_620$maximetPrecipitation_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    maximetPrecipitation_Stuck_Value_QC <- c(maximetPrecipitation_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  MetData_620$maximetPrecipitation_Stuck_Value_QC <-maximetPrecipitation_Stuck_Value_QC
  
  i<-0
  maximetHumidity_Stuck_Value_QC <- c()
  for (value in MetData_620$maximetHumidity_despike) {
    i<-i+1 #Keeps track of index position
    lead_2 <- c(MetData_620$maximetHumidity_despike[i+1],MetData_620$maximetHumidity_despike[i+2]) #takes current index position and finds next two values in the vector
    lead_4 <- c(MetData_620$maximetHumidity_despike[i+1],MetData_620$maximetHumidity_despike[i+2],MetData_620$maximetHumidity_despike[i+3],MetData_620$maximetHumidity_despike[i+4]) #takes current index position and finds next four values in the vector
    #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
    if(identical(rep((MetData_620$maximetHumidity_despike[i]),times=4), lead_4)==TRUE){
      Stuck_Value_QC = 2 
    }else if(identical(rep((MetData_620$maximetHumidity_despike[i]),times=2), lead_2)==TRUE){
      Stuck_Value_QC = 1
    }else if((identical(rep((MetData_620$maximetHumidity_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((MetData_620$maximetHumidity_despike[i]),times=4), lead_4)==FALSE)){
      Stuck_Value_QC = 0
    }
    maximetHumidity_Stuck_Value_QC <- c(maximetHumidity_Stuck_Value_QC, Stuck_Value_QC)
    
  } 
  MetData_620$maximetHumidity_Stuck_Value_QC <-maximetHumidity_Stuck_Value_QC
  MetData_620 <-MetData_620[,c(1,2,3,12,19,28,4,21,29,5,13,20,30,6,22,31,7,14,23,32,8,15,24,33,9,17,27,36,10,18,26,35,11,16,25,34)]
  MetData_620_qc<- sprintf("C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/MetData_620_QC_%s.csv",qc_date_range)
  write.csv(MetData_620, MetData_620_qc)
  dbAppendTable(mysql_db, "QC-Buoy-620_MetData",MetData_620, append=TRUE)
}

if((all(is.na((GPS_720$Latitude)))==FALSE)){
  GPS_720$`TmStamp` <- parse_date_time(GPS_720$`TmStamp`, c("%y/%m/%d %H:%M:%S"))
  GPS_720 <- filter(GPS_720,TmStamp >start_time)
  colnames(GPS_720)[colnames(GPS_720) == "TmStamp"] <- "gpsStart" # Rename column
  GPS_720_qc<- sprintf("C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/GPS_720_QC_%s.csv",qc_date_range)
  write.csv(GPS_720,GPS_720_qc)
  dbAppendTable(mysql_db, "QC-Buoy-720_GPSData",GPS_720, append=TRUE)
}

if((all(is.na((GPS_620$Latitude)))==FALSE)){
  GPS_620$`TmStamp` <- parse_date_time(GPS_620$`TmStamp`, c("%y/%m/%d %H:%M:%S"))
  GPS_620 <- filter(GPS_620,TmStamp >start_time)
  colnames(GPS_620)[colnames(GPS_620) == "TmStamp"] <- "gpsStart" # Rename column
  GPS_620_qc<- sprintf("C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/GPS_620_QC_%s.csv",qc_date_range)
  write.csv(GPS_620,GPS_620_qc)
  dbAppendTable(mysql_db, "QC-Buoy-620_GPSData",GPS_620, append=TRUE)
}

dbDisconnect(mysql_db)

Solar_620_max <- max(System_620$SolarVoltage,na.rm = TRUE)
Solar_620_min <- min(System_620$SolarVoltage,na.rm = TRUE)
Battery_620_max <-max(System_620$Battery1Voltage,na.rm = TRUE)
Battery_620_min <-min(System_620$Battery1Voltage,na.rm = TRUE)
Wind_620_max <-max(MetData_620$avgWindSpeed,na.rm = TRUE)
Wind_620_min <-min(MetData_620$avgWindSpeed,na.rm = TRUE)
Gust_620_max <-max(MetData_620$gustWindSpeed,na.rm=TRUE)
Gust_620_min <-min(MetData_620$gustWindSpeed,na.rm=TRUE)
Precip_620_max <- max(MetData_620$maximetPrecipitation,na.rm=TRUE)
Precip_620_min <- min(MetData_620$maximetPrecipitation,na.rm=TRUE)
Atm_Temp_620_max <-max(MetData_620$maximetTemperature,na.rm=TRUE)
Atm_Temp_620_min <-min(MetData_620$maximetTemperature,na.rm=TRUE)
Hydrocat_620_ph_max <- max(Hydrocat_620$ph_despike,na.rm = TRUE)
Hydrocat_620_ph_min <-min(Hydrocat_620$ph_despike,na.rm = TRUE)
Hydrocat_620_oxy_max <- max(Hydrocat_620$Oxygen_despike,na.rm = TRUE)
Hydrocat_620_oxy_min <-min(Hydrocat_620$Oxygen_despike,na.rm = TRUE)
Hydrocat_620_salinity_max <- max(Hydrocat_620$salinity_despike,na.rm = TRUE)
Hydrocat_620_salinity_min <-min(Hydrocat_620$salinity_despike,na.rm = TRUE)
Hydrocat_620_temp_max <- max(Hydrocat_620$Temperature_despike,na.rm = TRUE)
Hydrocat_620_temp_min <-min(Hydrocat_620$Temperature_despike,na.rm = TRUE)
Hydrocat_620_turb_max <- max(Hydrocat_620$turbidity_despike,na.rm = TRUE)
Hydrocat_620_turb_min <- min(Hydrocat_620$turbidity_despike,na.rm = TRUE)
Hydrocat_620_fluor_max <- max(Hydrocat_620$fluorescence_despike,na.rm = TRUE)
Hydrocat_620_fluor_min <- min(Hydrocat_620$fluorescence_despike,na.rm = TRUE)
SUNA_620_nitrate_max <- max(Suna_620$Nitrate_despike,na.rm = TRUE)
SUNA_620_nitrate_min <- min(Suna_620$Nitrate_despike,na.rm = TRUE)
Hydrocycle_620_phosphate_max <- max(Hydrocycle_620$Phosphate_despike,na.rm = TRUE)
Hydrocycle_620_phosphate_min <- min(Hydrocycle_620$Phosphate_despike,na.rm = TRUE)
ECO_620_FDOM_max <- max(ECO_620$FDOM_despike,na.rm = TRUE)
ECO_620_FDOM_min <- min(ECO_620$FDOM_despike,na.rm = TRUE)

Solar_720_max <- max(System_720$SolarVoltage,na.rm = TRUE)
Solar_720_min <- min(System_720$SolarVoltage,na.rm = TRUE)
Battery_720_max <-max(System_720$Battery1Voltage,na.rm = TRUE)
Battery_720_min <-min(System_720$Battery1Voltage,na.rm = TRUE)
Wind_720_max <-max(MetData_720$avgWindSpeed,na.rm = TRUE)
Wind_720_min <-min(MetData_720$avgWindSpeed,na.rm = TRUE)
Gust_720_max <-max(MetData_720$gustWindSpeed,na.rm=TRUE)
Gust_720_min <-min(MetData_720$gustWindSpeed,na.rm=TRUE)
Precip_720_max <- max(MetData_720$maximetPrecipitation,na.rm=TRUE)
Precip_720_min <- min(MetData_720$maximetPrecipitation,na.rm=TRUE)
Atm_Temp_720_max <-max(MetData_720$maximetTemperature,na.rm=TRUE)
Atm_Temp_720_min <-min(MetData_720$maximetTemperature,na.rm=TRUE)
Hydrocat_720_ph_max <- max(Hydrocat_720$ph_despike,na.rm = TRUE)
Hydrocat_720_ph_min <-min(Hydrocat_720$ph_despike,na.rm = TRUE)
Hydrocat_720_oxy_max <- max(Hydrocat_720$Oxygen_despike,na.rm = TRUE)
Hydrocat_720_oxy_min <-min(Hydrocat_720$Oxygen_despike,na.rm = TRUE)
Hydrocat_720_salinity_max <- max(Hydrocat_720$salinity_despike,na.rm = TRUE)
Hydrocat_720_salinity_min <-min(Hydrocat_720$salinity_despike,na.rm = TRUE)
Hydrocat_720_temp_max <- max(Hydrocat_720$Temperature_despike,na.rm = TRUE)
Hydrocat_720_temp_min <-min(Hydrocat_720$Temperature_despike,na.rm = TRUE)
Hydrocat_720_turb_max <- max(Hydrocat_720$turbidity_despike,na.rm = TRUE)
Hydrocat_720_turb_min <- min(Hydrocat_720$turbidity_despike,na.rm = TRUE)
Hydrocat_720_fluor_max <- max(Hydrocat_720$fluorescence_despike,na.rm = TRUE)
Hydrocat_720_fluor_min <- min(Hydrocat_720$fluorescence_despike,na.rm = TRUE)
SUNA_720_nitrate_max <- max(Suna_720$Nitrate_despike,na.rm = TRUE)
SUNA_720_nitrate_min <- min(Suna_720$Nitrate_despike,na.rm = TRUE)
Hydrocycle_720_phosphate_max <- max(Hydrocycle_720$Phosphate_despike,na.rm = TRUE)
Hydrocycle_720_phosphate_min <- min(Hydrocycle_720$Phosphate_despike,na.rm = TRUE)
ECO_720_FDOM_max <- max(ECO_720$FDOM_despike,na.rm = TRUE)
ECO_720_FDOM_min <- min(ECO_720$FDOM_despike,na.rm = TRUE)

Server<-list(smtpServer= "regmail.brown.edu",smtpPort=25 )

qc_subject_text <- "High Resolution QC"
qc_body_text <- sprintf ("12 Hour QC Ranges

          Buoy 620 
          Solar Voltage:%s-%s
          Battery Voltage:%s-%s
          Salinity(PSU):%s-%s
          Temperature(C):%s-%s
          pH:%s-%s
          Turbidity(NTU):%s-%s
          Chlorophyll a(ug/L):%s-%s
          Dissolved Oxygen(mg/L):%s-%s
          Nitrate(uM):%s-%s
          Phosphate(uM):%s-%s
          FDOM(ppb):%s-%s
          Avg. Wind Speed(m/s):%s-%s
          Gust Wind Speed(m/s):%s-%s
          Precipitation(mm/hr):%s-%s
          Air Temperature(C):%s-%s
                         
          Buoy 720
          Solar Voltage:%s-%s
          Battery Voltage:%s-%s
          Salinity(PSU):%s-%s
          Temperature(C):%s-%s
          pH:%s-%s
          Turbidity(NTU):%s-%s
          Chlorophyll a(ug/L):%s-%s
          Dissolved Oxygen(mg/L):%s-%s
          Nitrate(uM):%s-%s
          Phosphate(uM):%s-%s
          FDOM(ppb):%s-%s
          Avg. Wind Speed(m/s):%s-%s
          Gust Wind Speed(m/s):%s-%s
          Precipitation(mm/hr):%s-%s
          Air Temperature(C):%s-%s",
                         Solar_620_min,Solar_620_max,Battery_620_min,Battery_620_max,
                         Hydrocat_620_salinity_min,Hydrocat_620_salinity_max,
                         Hydrocat_620_temp_min,Hydrocat_620_temp_max,
                         Hydrocat_620_ph_min,Hydrocat_620_ph_max,
                         Hydrocat_620_turb_min,Hydrocat_620_turb_max,
                         Hydrocat_620_fluor_min,Hydrocat_620_fluor_max,
                         Hydrocat_620_oxy_min,Hydrocat_620_oxy_max,
                         SUNA_620_nitrate_min,SUNA_620_nitrate_max,
                         Hydrocycle_620_phosphate_min,Hydrocycle_620_phosphate_max,
                         ECO_620_FDOM_min,ECO_620_FDOM_max,
                         Wind_620_min,Wind_620_max,
                         Gust_620_min,Gust_620_max,
                         Precip_620_min,Precip_620_max,
                         Atm_Temp_620_min,Atm_Temp_620_max,
                         Solar_720_min,Solar_720_max,Battery_720_min,Battery_720_max,
                         Hydrocat_720_salinity_min,Hydrocat_720_salinity_max,
                         Hydrocat_720_temp_min,Hydrocat_720_temp_max,
                         Hydrocat_720_ph_min,Hydrocat_720_ph_max,
                         Hydrocat_720_turb_min,Hydrocat_720_turb_max,
                         Hydrocat_720_fluor_min,Hydrocat_720_fluor_max,
                         Hydrocat_720_oxy_min,Hydrocat_720_oxy_max,
                         SUNA_720_nitrate_min,SUNA_720_nitrate_max,
                         Hydrocycle_720_phosphate_min,Hydrocycle_720_phosphate_max,
                         ECO_720_FDOM_min,ECO_720_FDOM_max,
                         Wind_720_min,Wind_720_max,
                         Gust_720_min,Gust_720_max,
                         Precip_720_min,Precip_720_max,
                         Atm_Temp_720_min,Atm_Temp_720_max)

bodyWithAttachment <- list(qc_body_text)
sendmailV <- Vectorize( sendmail , vectorize.args = "to" )
emails <- c( "kristofer_gomes@uri.edu","reporting-ricaim@brown.edu" ) #, 
sendmailV("noreply@brown.edu",emails,qc_subject_text,bodyWithAttachment,control=Server)