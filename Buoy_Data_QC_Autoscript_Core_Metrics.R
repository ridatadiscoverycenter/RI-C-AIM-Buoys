#Load libraries
library("tidyverse")
library("lubridate")
library("oce")
library('ggplot2')
library('ggpubr')
library('plyr')
library('scales')
library('readr')
library('httr')
library('jsonlite')
library('gmailr')
library("httpuv") 
library('sendmailR')

#Create automatic weekly date range
end_date <-parse_date_time(Sys.Date(), c("%y-%m-%d"))#pulls current system date
start_date <-end_date - days(7) #subtracts 7 days to get week range

date_range <-sprintf("%s,%s", start_date, end_date)
qc_date_range<-sprintf("%s_%s", start_date, end_date)


#Pull the last week of data from the Brown API for Buoy-720
System_720 = GET(sprintf("https://api.riddc.brown.edu/telemetry/Buoy-720/System/range?start=%s&end=%s",start_date,end_date))
System_720 = fromJSON(rawToChar(System_720$content))

Core_720 = GET(sprintf("https://api.riddc.brown.edu/telemetry/Buoy-720/CoreMetrics/range?start=%s&end=%s",start_date,end_date))
Core_720 = fromJSON(rawToChar(Core_720$content))

Suna_720 = Core_720$SUNA
Hydrocat_720 = Core_720$Hydrocat
ECO_720 = Core_720$ECO
Hydrocycle_720 = Core_720$Hydrocycle
MetData_720 = Core_720$MetData
PAR_720 = Core_720$PAR


#Parse 720 date/time values using Lubridate 
System_720$`TmStamp` <- parse_date_time(System_720$TmStamp, c("%y/%m/%d %H:%M:%S"))

Hydrocat_720$`TmStamp` <- parse_date_time(Hydrocat_720$TmStamp, c("%y/%m/%d %H:%M:%S"))

ECO_720$`TmStamp` <- parse_date_time(ECO_720$TmStamp, c("%y/%m/%d %H:%M:%S"))

Suna_720$`TmStamp` <- parse_date_time(Suna_720$`TmStamp`, c("%y/%m/%d %H:%M:%S"))

MetData_720$`TmStamp` <- parse_date_time(MetData_720$`TmStamp`, c("%y/%m/%d %H:%M:%S"))

PAR_720$`TmStamp` <- parse_date_time(PAR_720$`TmStamp`, c("%y/%m/%d %H:%M:%S"))

Hydrocycle_720$`TmStamp` <- parse_date_time(Hydrocycle_720$`TmStamp`, c("%y/%m/%d %H:%M:%S"))

month_range <- (range(month(PAR_720$`TmStamp`))) #create month range of collected data to be used for filtering Narragansett Bay historical data by season


#Pull the last week of data from the Brown API for Buoy-620
System_620 = GET(sprintf("https://api.riddc.brown.edu/telemetry/Buoy-620/System/range?start=%s&end=%s",start_date,end_date))
System_620 = fromJSON(rawToChar(System_620$content))

Core_620 = GET(sprintf("https://api.riddc.brown.edu/telemetry/Buoy-620/CoreMetrics/range?start=%s&end=%s",start_date,end_date))
Core_620 = fromJSON(rawToChar(Core_620$content))

Suna_620 = Core_620$SUNA
Hydrocat_620 = Core_620$Hydrocat
ECO_620 = Core_620$ECO
Hydrocycle_620 = Core_620$Hydrocycle
MetData_620 = Core_620$MetData
PAR_620 = Core_620$PAR


#Parse 620 date/time values using Lubridate
System_620$`TmStamp` <- parse_date_time(System_620$TmStamp, c("%y/%m/%d %H:%M:%S"))

Hydrocat_620$`TmStamp` <- parse_date_time(Hydrocat_620$TmStamp, c("%y/%m/%d %H:%M:%S"))

ECO_620$`TmStamp` <- parse_date_time(ECO_620$TmStamp, c("%y/%m/%d %H:%M:%S"))

Suna_620$`TmStamp` <- parse_date_time(Suna_620$`TmStamp`, c("%y/%m/%d %H:%M:%S"))

MetData_620$`TmStamp` <- parse_date_time(MetData_620$`TmStamp`, c("%y/%m/%d %H:%M:%S"))

PAR_620$`TmStamp` <- parse_date_time(PAR_620$`TmStamp`, c("%y/%m/%d %H:%M:%S"))

Hydrocycle_620$`TmStamp` <- parse_date_time(Hydrocycle_620$`TmStamp`, c("%y/%m/%d %H:%M:%S"))

month_range <- (range(month(PAR_620$`TmStamp`))) #create month range of collected data to be used for filtering Narragansett Bay historical data by season


#Despike data through median smoothing approach and write into a new column. 
Hydrocat_720$Oxygen_despike <- despike(Hydrocat_720$`hydrocatDissOxygen`,reference = c("median"),n = 2,k = 7,replace = c("reference"))

Hydrocat_720$ph_despike <- despike(Hydrocat_720$`hydrocatPH`,reference = c("median"),n = 2,k = 7,replace = c("reference"))

Hydrocat_720$Temperature_despike <- despike(Hydrocat_720$`hydrocatTemperature`,reference = c("median"),n = 2,k = 7,replace = c("reference"))

Hydrocat_720$salinity_despike <- despike(Hydrocat_720$`hydrocatSalinity`,reference = c("median"),n = 2,k = 7,replace = c("reference"))

Hydrocat_720$turbidity_despike <- despike(Hydrocat_720$`hydrocatTurbidity`,reference = c("median"),n = 2,k = 7,replace = c("reference"))

Hydrocat_720$fluorescence_despike <- despike(Hydrocat_720$`hydrocatFluorescence`,reference = c("median"),n = 2,k = 7,replace = c("reference"))

ECO_720$FDOM_despike<- despike(ECO_720$`ecoFDOM`,reference = c("median"), n=2, k=7, replace = c("reference"))

Suna_720$Nitrate_despike <- despike(Suna_720$`sunaNitrateMicroMol`,reference = c("median"), n=2, k=7, replace = c("reference"))

Hydrocycle_720$Phosphate_despike <- despike(Hydrocycle_720$'CAPO4',reference = c("median"), n=2, k=7, replace = c("reference"))


Hydrocat_620$Oxygen_despike <- despike(Hydrocat_620$`hydrocatDissOxygen`,reference = c("median"),n = 2,k = 7,replace = c("reference"))

Hydrocat_620$ph_despike <- despike(Hydrocat_620$`hydrocatPH`,reference = c("median"),n = 2,k = 7,replace = c("reference"))

Hydrocat_620$Temperature_despike <- despike(Hydrocat_620$`hydrocatTemperature`,reference = c("median"),n = 2,k = 7,replace = c("reference"))

Hydrocat_620$salinity_despike <- despike(Hydrocat_620$`hydrocatSalinity`,reference = c("median"),n = 2,k = 7,replace = c("reference"))

Hydrocat_620$turbidity_despike <- despike(Hydrocat_620$`hydrocatTurbidity`,reference = c("median"),n = 2,k = 7,replace = c("reference"))

Hydrocat_620$fluorescence_despike <- despike(Hydrocat_620$`hydrocatFluorescence`,reference = c("median"),n = 2,k = 7,replace = c("reference"))

ECO_620$FDOM_despike <- despike(ECO_620$`ecoFDOM`,reference = c("median"), n=2, k=7, replace = c("reference"))

Suna_620$Nitrate_despike <- despike(Suna_620$`sunaNitrateMicroMol`,reference = c("median"), n=2, k=7, replace = c("reference"))

Hydrocycle_620$Phosphate_despike <- despike(Hydrocycle_620$'CAPO4',reference = c("median"), n=2, k=7, replace = c("reference"))


#Perform range check for applicable variables against global historical measurement range, flag with QC Flags (1=low, 2=high, 0=good) 
##global_range_data <- read.csv('C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/data_qc_global_range_values.csv', header=TRUE, sep=",", na.strings=c(""))
global_range_data <- read.csv('C:/Users/1600x/Documents/GitHub/RI-C-AIM-Buoys/data_qc_global_range_values.csv', header=TRUE, sep=",", na.strings=c(""))

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

#Range check against global values, insert global range QC Flag (0=within range, 1=below range, 2=above range)
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

ECO_720 <- mutate(ECO_720,
                  FDOM_Global_Range_QC = ifelse(ECO_720$`FDOM_despike` > global_fluoro_max,
                                                "2", ifelse(ECO_720$`FDOM_despike` < global_fluoro_min,"1","0")))

Suna_720 <- mutate(Suna_720,
                   Nitrate_Global_Range_QC = ifelse(Suna_720$`Nitrate_despike` > global_nitrate_max,
                                                    "2", ifelse(Suna_720$`Nitrate_despike` < global_nitrate_min,"1","0")))
Hydrocyle_720-week <- mutate(Hydrocycle_720,
                    Nitrate_Global_Range_QC = ifelse(Suna_720$`Nitrate_despike` > global_nitrate_max,
                                                 "2", ifelse(Suna_720$`Nitrate_despike` < global_nitrate_min,"1","0")))

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

ECO_620 <- mutate(ECO_620,
                  FDOM_Global_Range_QC = ifelse(ECO_620$`FDOM_despike` > global_fluoro_max,
                                                "2", ifelse(ECO_620$`FDOM_despike` < global_fluoro_min,"1","0")))

Suna_620 <- mutate(Suna_620,
                   Nitrate_Global_Range_QC = ifelse(Suna_620$`Nitrate_despike` > global_nitrate_max,
                                                    "2", ifelse(Suna_620$`Nitrate_despike` < global_nitrate_min,"1","0")))


#Perform range check for applicable variables against historical seasonal Narray Bay measurement range, flag with QC Flags  (0=within range, 1=below range, 2=above range)
##nb_hist_phys_data <- read.csv('C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/NB_PhysicalData_8.10.20201.csv', header=TRUE, sep=",", na.strings=c("","nd")) #Read in historical physical data from GSO LTS measurements
nb_hist_phys_data <- read.csv('C:/Users/1600x/Documents/GitHub/RI-C-AIM-Buoys/NB_PhysicalData_8.10.20201.csv', header=TRUE, sep=",", na.strings=c("","nd"))

nb_hist_phys_data$Date <- parse_date_time(nb_hist_phys_data$Date, c("%d-%m-%y")) #Parse Date column into date objects

nb_hist_phys_seasonal_filter <- nb_hist_phys_data %>% filter(month(nb_hist_phys_data$Date) >= min(month_range) & month(nb_hist_phys_data$Date) <= max(month_range))   #Filter historical data to seasonal time frame of buoy data

seasonal_nb_psu_range <- range(nb_hist_phys_seasonal_filter$Surface_Salinity,na.rm = TRUE)

seasonal_nb_surface_temp_range <- range(nb_hist_phys_seasonal_filter$Surface_Temp,na.rm = TRUE)


##nb_hist_nutr_data <- read.csv('C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/nutrientdata_01.18.2021.csv', header=TRUE, sep=",", na.strings=c("","nd")) #Read in historical nutrient data from GSO LTS measurements
nb_hist_nutr_data <- read.csv('C:/Users/1600x/Documents/GitHub/RI-C-AIM-Buoys/nutrientdata_01.18.2021.csv', header=TRUE, sep=",", na.strings=c("","nd"))

nb_hist_nutr_data$Date <- parse_date_time(nb_hist_nutr_data$Date, c("%m/%d/%y")) #Parse Date column into date objects

nb_hist_nutr_seasonal_filter <- nb_hist_nutr_data %>% filter(month(nb_hist_nutr_data$Date) >= min(month_range) & month(nb_hist_nutr_data$Date) <= max(month_range))   #Filter historical data to seasonal time frame of buoy data

seasonal_nb_nitrate_range <- range(nb_hist_nutr_seasonal_filter$NO3,na.rm = TRUE)

seasonal_nb_DIP_range <- range(nb_hist_nutr_seasonal_filter$DIP,na.rm = TRUE)


#Range check against seasonal Narragansett Bay global values
Hydrocat_720 <-mutate(Hydrocat_720,
                      Salinity_Seasonal_NB_Range_QC =ifelse(Hydrocat_720$`salinity_despike` >  max(seasonal_nb_psu_range),
                                                            "2",ifelse( Hydrocat_720$`salinity_despike` < min(seasonal_nb_psu_range),"1","0"))) 
Hydrocat_720 <-mutate(Hydrocat_720,
                      Temp_Seasonal_NB_Range_QC =ifelse(Hydrocat_720$`Temperature_despike` > max(seasonal_nb_surface_temp_range),
                                                        "2",ifelse( Hydrocat_720$`Temperature_despike` < min(seasonal_nb_surface_temp_range),"1","0")))

Suna_720 <- mutate(Suna_720,
                   Nitrate_Global_Range_QC = ifelse(Suna_720$`Nitrate_despike` > max(seasonal_nb_nitrate_range),
                                                    "2", ifelse(Suna_720$`Nitrate_despike` < min(seasonal_nb_nitrate_range),"1","0")))
Hydrocycle_720 <- mutate(Hydrocycle_720,
                         Phosphate_Seasonal_NB_Range_QC = ifelse(Hydrocycle_720$Phosphate_despike > max(seasonal_nb_DIP_range),
                                                                 "2", ifelse(Hydrocycle_720$Phosphate_despike < min(seasonal_nb_DIP_range),"1","0")))


Hydrocat_620 <-mutate(Hydrocat_620,
                      Salinity_Seasonal_NB_Range_QC =ifelse(Hydrocat_620$`salinity_despike` >  max(seasonal_nb_psu_range),
                                                            "2",ifelse( Hydrocat_620$`salinity_despike` < min(seasonal_nb_psu_range),"1","0"))) 
Hydrocat_620 <-mutate(Hydrocat_620,
                      Temp_Seasonal_NB_Range_QC =ifelse(Hydrocat_620$`Temperature_despike` > max(seasonal_nb_surface_temp_range),
                                                        "2",ifelse( Hydrocat_620$`Temperature_despike` < min(seasonal_nb_surface_temp_range),"1","0")))

Suna_620 <- mutate(Suna_620,
                   Nitrate_Global_Range_QC = ifelse(Suna_620$`Nitrate_despike` > max(seasonal_nb_nitrate_range),
                                                    "2", ifelse(Suna_620$`Nitrate_despike` < min(seasonal_nb_nitrate_range),"1","0")))
Hydrocycle_620 <- mutate(Hydrocycle_620,
                         Phosphate_Seasonal_NB_Range_QC = ifelse(Hydrocycle_620$Phosphate_despike > max(seasonal_nb_DIP_range),
                                                                 "2", ifelse(Hydrocycle_620$Phosphate_despike < min(seasonal_nb_DIP_range),"1","0")))
#Stuck Value Test 

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
  }else if((identical(rep((Hydrocat_720$salinity_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((Hydrocat_720$salinity_despike[i]),times=4), lead_4)==FALSE)){
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


#FDOM_720
i<-0
FDOM_Stuck_Value_QC <- c()
for (value in Suna_720$FDOM_despike) {
  i<-i+1 #Keeps track of index position
  lead_2 <- c(ECO_720$FDOM_despike[i+1],ECO_720$FDOM_despike[i+2]) #takes current index position and finds next two values in the vector
  lead_4 <- c(ECO_720$FDOM_despike[i+1],ECO_720$FDOM_despike[i+2],ECO_720$FDOM_despike[i+3],ECO_720$FDOM_despike[i+4]) #takes current index position and finds next four values in the vector
  #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
  if(identical(rep((ECO_720$FDOM_despike[i]),times=4), lead_4)==TRUE){
    Stuck_Value_QC = 2 
  }else if(identical(rep((ECO_720$FDOM_despike[i]),times=2), lead_2)==TRUE){
    Stuck_Value_QC = 1
  }else if((identical(rep((ECO_720$FDOM_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((ECO_720$FDOM_despike[i]),times=4), lead_4)==FALSE)){
    Stuck_Value_QC = 0
  }
  FDOM_Stuck_Value_QC <- c(FDOM_Stuck_Value_QC, Stuck_Value_QC)
  
} 
ECO_720$FDOM_Stuck_Value_QC <-FDOM_Stuck_Value_QC








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
  }else if((identical(rep((Hydrocat_620$salinity_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((Hydrocat_620$salinity_despike[i]),times=4), lead_4)==FALSE)){
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


#FDOM_620
i<-0
FDOM_Stuck_Value_QC <- c()
for (value in Suna_620$FDOM_despike) {
  i<-i+1 #Keeps track of index position
  lead_2 <- c(ECO_620$FDOM_despike[i+1],ECO_620$FDOM_despike[i+2]) #takes current index position and finds next two values in the vector
  lead_4 <- c(ECO_620$FDOM_despike[i+1],ECO_620$FDOM_despike[i+2],ECO_620$FDOM_despike[i+3],ECO_620$FDOM_despike[i+4]) #takes current index position and finds next four values in the vector
  #compares the list of replicated values and checks if they are identical to the leading values, QC Flags ( 1 = 3 repeated values (potential stuck value), 2 = 5 repeated values (stuck value), 0= No stuck value detected)
  if(identical(rep((ECO_620$FDOM_despike[i]),times=4), lead_4)==TRUE){
    Stuck_Value_QC = 2 
  }else if(identical(rep((ECO_620$FDOM_despike[i]),times=2), lead_2)==TRUE){
    Stuck_Value_QC = 1
  }else if((identical(rep((ECO_620$FDOM_despike[i]), times=2), lead_2,)==FALSE)&(identical(rep((ECO_620$FDOM_despike[i]),times=4), lead_4)==FALSE)){
    Stuck_Value_QC = 0
  }
  FDOM_Stuck_Value_QC <- c(FDOM_Stuck_Value_QC, Stuck_Value_QC)
  
} 
ECO_620$FDOM_Stuck_Value_QC <-FDOM_Stuck_Value_QC



#Pull max and min measurements for instruments from each buoy
#Pull max and min measurements for instruments from each buoy
Solar_620_max <- max(System_620$SolarVoltage,na.rm = TRUE)
Solar_620_min <- min(System_620$SolarVoltage,na.rm = TRUE)
Battery_620_max <-max(System_620$Battery1Voltage,na.rm = TRUE)
Battery_620_min <-min(System_620$Battery1Voltage,na.rm = TRUE)
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

#Write data tables with despiked values and QC flags to CSVs
##Hydrocat_720_qc <- sprintf("C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/Hydrocat_720_QC_%s.csv",qc_date_range)
Hydrocat_720_qc <- sprintf("C:/Users/1600x/Documents/GitHub/RI-C-AIM-Buoys/Hydrocat_720_QC_%s.csv",qc_date_range)
write.csv(Hydrocat_720,Hydrocat_720_qc)

##ECO_720_qc <- sprintf("C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/ECO_720_QC_%s.csv",qc_date_range)
ECO_720_qc <- sprintf("C:/Users/1600x/Documents/GitHub/RI-C-AIM-Buoys/ECO_720_QC_%s.csv",qc_date_range)
write.csv(ECO_720,ECO_720_qc)

Suna_720_qc <- sprintf("C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/SUNA_720_QC_%s.csv",qc_date_range)
Suna_720_qc <- sprintf("C:/Users/1600x/Documents/GitHub/RI-C-AIM-Buoys/SUNA_720_QC_%s.csv",qc_date_range)
write.csv(Suna_720,Suna_720_qc)

##Hydrocycle_720_qc <- sprintf("C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/Hydrocycle_720_QC_%s.csv",qc_date_range)
Hydrocycle_720_qc <- sprintf("C:/Users/1600x/Documents/GitHub/RI-C-AIM-Buoys/Hydrocycle_720_QC_%s.csv",qc_date_range)
write.csv(Hydrocycle_720,Hydrocycle_720_qc)


##Hydrocat_620_qc <- sprintf("C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/Hydrocat_620_QC_%s.csv",qc_date_range)
Hydrocat_620_qc <- sprintf("C:/Users/1600x/Documents/GitHub/RI-C-AIM-Buoys/Hydrocat_620_QC_%s.csv",qc_date_range)
write.csv(Hydrocat_620,Hydrocat_620_qc)

##ECO_620_qc <- sprintf("C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/ECO_620_QC_%s.csv",qc_date_range)
ECO_620_qc <- sprintf("C:/Users/1600x/Documents/GitHub/RI-C-AIM-Buoys/ECO_620_QC_%s.csv",qc_date_range)
write.csv(ECO_620,ECO_620_qc)

##Suna_620_qc <- sprintf("C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/SUNA_620_QC_%s.csv",qc_date_range)
Suna_620_qc <- sprintf("C:/Users/1600x/Documents/GitHub/RI-C-AIM-Buoys/SUNA_620_QC_%s.csv",qc_date_range)
write.csv(Suna_620,Suna_620_qc)

##Hydrocycle_620_qc <- sprintf("C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Data/Hydrocycle_620_QC_%s.csv",qc_date_range)
Hydrocycle_620_qc <- sprintf("C:/Users/1600x/Documents/GitHub/RI-C-AIM-Buoys/Hydrocycle_620_QC_%s.csv",qc_date_range)
write.csv(Hydrocycle_620,Hydrocycle_620_qc)

#Plot desired time series line plots in grid format.Lines are plotted for visual inspection, and colored by QC range flags.
##file_path_720 <- sprintf("C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Plots/720_Weekly_Plots_%s.pdf",qc_date_range)
file_path_720 <- sprintf("C:/Users/1600x/Documents/GitHub/RI-C-AIM-Buoys/720_Weekly_Plots_%s.pdf",qc_date_range)
file_name_720 <-sprintf("720_Weekly_Plots_%s.pdf",qc_date_range)
pdf(file=file_path_720,width=16,height=12,useDingbats=FALSE)#open pdf graphical object to plot as pdf

Solar_720 <- ggplot(System_720,aes(x=`TmStamp`, y=SolarVoltage)) + (scale_x_datetime(breaks = date_breaks("1 day"),
            labels = date_format("%m/%d"))) +  geom_point() +   ylab('Solar Voltage') + xlab('Date')+  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

Battery_720 <- ggplot(System_720,aes(x=`TmStamp`, y=Battery1Voltage)) + (scale_x_datetime(breaks = date_breaks("1 day"),
labels = date_format("%m/%d"))) + geom_point() +  ylab('Battery Voltage') + xlab('Date')+  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


Hydro_Salinity_720 <- ggplot(Hydrocat_720,aes(x=`TmStamp`, y=salinity_despike, color=Salinity_Global_Range_QC)) + (scale_x_datetime(breaks = date_breaks("1 day"),
        labels = date_format("%m/%d")))  +  geom_point() + scale_color_manual(breaks = c("0", "1", "2"),values=c("#009900", "#00CCCC", "#CC0000")) + ylab('Salinity #(psu)') + xlab('Date')+  theme_bw() +
 theme(panel.grid.major.x = element_blank(),
       panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


Hydro_Temp_720 <- ggplot(Hydrocat_720,aes(x=`TmStamp`, y=Temperature_despike, color=Temp_Global_Range_QC )) + (scale_x_datetime(breaks = date_breaks("1 day"),
        labels = date_format("%m/%d")))  +  geom_point() + scale_color_manual(breaks = c("0", "1", "2"),values=c("#009900", "#00CCCC", "#CC0000")) + ylab('Temperature (Celcius)') +   xlab('Date')+  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


Hydro_pH_720 <- ggplot(Hydrocat_720,aes(x=`TmStamp`, y=ph_despike, color=ph_Global_Range_QC )) + (scale_x_datetime(breaks = date_breaks("1 day"),
        labels = date_format("%m/%d")))  +  geom_point() + scale_color_manual(breaks = c("0", "1", "2"),values=c("#009900", "#00CCCC", "#CC0000")) + ylab('pH') + xlab('Date')+       theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


Hydro_oxy_720 <- ggplot(Hydrocat_720,aes(x=`TmStamp`, y=Oxygen_despike, color=oxy_Global_Range_QC )) + (scale_x_datetime(breaks = date_breaks("1 day"),
        labels = date_format("%m/%d")))  +  geom_point() + scale_color_manual(breaks = c("0", "1", "2"),values=c("#009900", "#00CCCC", "#CC0000")) + ylab('Dissolved #Oxygen (mg/L)')   + xlab('Date')+  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

Wind_720_Plot <- ggplot(MetData_720,aes(x=`TmStamp`, y=avgWindSpeed)) + (scale_x_datetime(breaks = date_breaks("1 day"),
                                                                                          labels = date_format("%m/%d")))  +  geom_point() + ylab('Average Wind Speed (m/s)') + xlab('Date')+  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="none")

Temperature_720_Plot <- ggplot(MetData_720,aes(x=`TmStamp`, y=maximetTemperature)) + (scale_x_datetime(breaks = date_breaks("1 day"),
                                                                                                       labels = date_format("%m/%d")))  +  geom_point() + ylab('Temperature (Celcius)') + xlab('Date')+  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="none")

Humidity_720_Plot <- ggplot(MetData_720,aes(x=`TmStamp`, y=maximetHumidity)) + (scale_x_datetime(breaks = date_breaks("1 day"),
                                                                                                 labels = date_format("%m/%d")))  +  geom_point() + ylab('Humidity (%)') + xlab('Date')+  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="none")

Pressure_720_Plot <- ggplot(MetData_720,aes(x=`TmStamp`, y=maximetPressure)) + (scale_x_datetime(breaks = date_breaks("1 day"),
                                                                                                 labels = date_format("%m/%d")))  +  geom_point() + ylab('Pressure (db)') + xlab('Date')+  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="none")

Precipitation_720_Plot <- ggplot(MetData_720,aes(x=`TmStamp`, y=maximetPrecipitation)) + (scale_x_datetime(breaks = date_breaks("1 day"),
                                                                                                           labels = date_format("%m/%d")))  +  geom_point() + ylab('Precipitation (mm/Hr)') + xlab('Date')+  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="none")


Suna_720_plot <- ggplot(Suna_720,aes(x=`TmStamp`, y=Nitrate_despike, color=Nitrate_Global_Range_QC)) + (scale_x_datetime(breaks = date_breaks("1 day"),
                                                                                                                         labels = date_format("%m/%d")))  +  geom_point() + scale_color_manual(breaks = c("0", "1", "2"),values=c("#009900", "#00CCCC", "#CC0000")) + ylab('Nitrate (umol)') + xlab('Date')+  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="none")

ECO_720_plot <- ggplot(ECO_720,aes(x=`TmStamp`,y=ecoReadingRaw, color=FDOM_Global_Range_QC)) + (scale_x_datetime(breaks = date_breaks("1 day"),
                                                                                                                 labels = date_format("%m/%d")))  +  geom_point() + scale_color_manual(breaks = c("0", "1", "2"),values=c("#009900", "#00CCCC", "#CC0000")) + ylab('FDOM (ppb)') + xlab('Date') +  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="none")

Hydrocycle_720_plot <- ggplot(Hydrocycle_720,aes(x=`TmStamp`,y=Phosphate_despike, color=Phosphate_Seasonal_NB_Range_QC)) + (scale_x_datetime(breaks = date_breaks("1 day"),
                                                                                                                                             labels = date_format("%m/%d")))  +  geom_point() + scale_color_manual(breaks = c("0", "1", "2"),values=c("#009900", "#00CCCC", "#CC0000")) + ylab('Phosphate (uM)') + xlab('Date') +  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="none")

qc_arrange <- ggarrange(Solar_720, Battery_720,ECO_720_plot,Suna_720_plot,Precipitation_720_Plot,Pressure_720_Plot,Humidity_720_Plot,Temperature_720_Plot, Wind_720_Plot,Hydrocycle_720_plot,Hydro_oxy_720,Hydro_pH_720,Hydro_Temp_720,Hydro_Salinity_720, nrow = 5)


qc_arrange
dev.off()#close pdf

#Plot desired time series line plots in grid format.Lines are plotted for visual inspection, and colored by QC range flags.
##file_path_620 <- sprintf("C:/Users/kgomes1/Desktop/Buoy_QC_Reports/Plots/620_Weekly_Plots_%s.pdf",qc_date_range)
file_path_620 <- sprintf("C:/Users/1600x/Documents/GitHub/RI-C-AIM-Buoys/620_Weekly_Plots_%s.pdf",qc_date_range)
file_name_620 <-sprintf("620_Weekly_Plots_%s.pdf",qc_date_range)
pdf(file=file_path_620,width=16,height=12,useDingbats=FALSE)#open pdf graphical object to plot as pdf


Solar_620 <- ggplot(System_620,aes(x=`TmStamp`, y=SolarVoltage)) + (scale_x_datetime(breaks = date_breaks("1 day"),
                                                                                     labels = date_format("%m/%d"))) +  geom_point() +  ylab('Solar Voltage') + xlab('Date')+  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

Battery_620 <- ggplot(System_620,aes(x=`TmStamp`, y=Battery1Voltage)) + (scale_x_datetime(breaks = date_breaks("1 day"),
                                                                                          labels = date_format("%m/%d"))) +  geom_point() +  ylab('Battery Voltage') + xlab('Date')+  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

Hydro_Salinity_620 <- ggplot(Hydrocat_620,aes(x=`TmStamp`, y=salinity_despike, color=Salinity_Global_Range_QC)) + (scale_x_datetime(breaks = date_breaks("1 day"),
                                                                                                                                    labels = date_format("%m/%d")))  +  geom_point() + scale_color_manual(breaks = c("0", "1", "2"),values=c("#009900", "#00CCCC", "#CC0000")) + ylab('Salinity (psu)') +         xlab('Date')+  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
       axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="none")


Hydro_Temp_620 <- ggplot(Hydrocat_620,aes(x=`TmStamp`, y=Temperature_despike, color=Temp_Global_Range_QC )) + (scale_x_datetime(breaks = date_breaks("1 day"),
                                                                                                                                labels = date_format("%m/%d")))  +  geom_point() + scale_color_manual(breaks = c("0", "1", "2"),values=c("#009900", "#00CCCC", "#CC0000")) + ylab('Temperature (Celcius)') +   xlab('Date')+  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="none")


Hydro_pH_620 <- ggplot(Hydrocat_620,aes(x=`TmStamp`, y=ph_despike, color=ph_Global_Range_QC )) + (scale_x_datetime(breaks = date_breaks("1 day"),
                                                                                                                   labels = date_format("%m/%d")))  +  geom_point() + scale_color_manual(breaks = c("0", "1", "2"),values=c("#009900", "#00CCCC", "#CC0000")) + ylab('pH') + xlab('Date')+       theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
 theme(legend.position="none")


Hydro_oxy_620 <- ggplot(Hydrocat_620,aes(x=`TmStamp`, y=Oxygen_despike, color=oxy_Global_Range_QC )) + (scale_x_datetime(breaks = date_breaks("1 day"),
                                                                                                                         labels = date_format("%m/%d")))  +  geom_point() + scale_color_manual(breaks = c("0", "1", "2"),values=c("#009900", "#00CCCC", "#CC0000")) + ylab('Dissolved Oxygen (mg/L)')   + xlab('Date')+  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="none")


Wind_620_Plot <- ggplot(MetData_620,aes(x=`TmStamp`, y=avgWindSpeed)) + (scale_x_datetime(breaks = date_breaks("1 day"),
                                                                                          labels = date_format("%m/%d")))  +  geom_point() + ylab('Average Wind Speed (m/s)') + xlab('Date')+  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="none")

Temperature_620_Plot <- ggplot(MetData_620,aes(x=`TmStamp`, y=maximetTemperature)) + (scale_x_datetime(breaks = date_breaks("1 day"),
                                                                                                       labels = date_format("%m/%d")))  +  geom_point() + ylab('Temperature (Celcius)') + xlab('Date')+  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="none")

Humidity_620_Plot <- ggplot(MetData_620,aes(x=`TmStamp`, y=maximetHumidity)) + (scale_x_datetime(breaks = date_breaks("1 day"),
                                                                                                 labels = date_format("%m/%d")))  +  geom_point() + ylab('Humidity (%)') + xlab('Date')+  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="none")

Pressure_620_Plot <- ggplot(MetData_620,aes(x=`TmStamp`, y=maximetPressure)) + (scale_x_datetime(breaks = date_breaks("1 day"),
                                                                                                 labels = date_format("%m/%d")))  +  geom_point() + ylab('Pressure (db)') + xlab('Date')+  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="none")

Precipitation_620_Plot <- ggplot(MetData_620,aes(x=`TmStamp`, y=maximetPrecipitation)) + (scale_x_datetime(breaks = date_breaks("1 day"),
                                                                                                           labels = date_format("%m/%d")))  +  geom_point() + ylab('Precipitation (mm/Hr)') + xlab('Date')+  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="none")


Suna_620_plot <- ggplot(Suna_620,aes(x=`TmStamp`, y=Nitrate_despike, color=Nitrate_Global_Range_QC)) + (scale_x_datetime(breaks = date_breaks("1 day"),
                                                                                                                         labels = date_format("%m/%d")))  +  geom_point() + scale_color_manual(breaks = c("0", "1", "2"),values=c("#009900", "#00CCCC", "#CC0000")) + ylab('Nitrate (umol)') + xlab('Date')+  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="none")

ECO_620_plot <- ggplot(ECO_620,aes(x=`TmStamp`,y=ecoReadingRaw, color=FDOM_Global_Range_QC)) + (scale_x_datetime(breaks = date_breaks("1 day"),
                                                                                                                 labels = date_format("%m/%d")))  +  geom_point() + scale_color_manual(breaks = c("0", "1", "2"),values=c("#009900", "#00CCCC", "#CC0000")) + ylab('FDOM (ppb)') + xlab('Date') +  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="none")

ECO_620_plot <- ggplot(ECO_620,aes(x=`TmStamp`,y=FDOM_despike, color=FDOM_Global_Range_QC)) + (scale_x_datetime(breaks = date_breaks("1 day"),
                                                                                                                labels = date_format("%m/%d")))  +  geom_point() + scale_color_manual(breaks = c("0", "1", "2"),values=c("#009900", "#00CCCC", "#CC0000")) + ylab('FDOM (ppb)') + xlab('Date') +  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="none")

Hydrocycle_620_plot <- ggplot(Hydrocycle_620,aes(x=`TmStamp`,y=Phosphate_despike, color=Phosphate_Seasonal_NB_Range_QC)) + (scale_x_datetime(breaks = date_breaks("1 day"),
                                                                                                                                             labels = date_format("%m/%d")))  +  geom_point() + scale_color_manual(breaks = c("0", "1", "2"),values=c("#009900", "#00CCCC", "#CC0000")) + ylab('Phosphate (uM)') + xlab('Date') +  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(angle=0)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="none")

qc_arrange <- ggarrange(Solar_620, Battery_620,ECO_620_plot,Suna_620_plot,Hydrocycle_620_plot,Precipitation_620_Plot,Pressure_620_Plot,Humidity_620_Plot,Temperature_620_Plot, Wind_620_Plot,Hydro_oxy_620,Hydro_pH_620,Hydro_Temp_620,Hydro_Salinity_620, nrow = 5)

qc_arrange
dev.off()#close pdf

#Setup sendmailR access using Brown emailer
Server<-list(smtpServer= "mail-relay.brown.edu",smtpPort=25 )

qc_subject_text <- sprintf("CAIM Buoy QC Report (Testing Brown Emailer) (%s)",qc_date_range)
qc_body_text <- sprintf ("Automated QC report for CAIM Buoys 720 and 620

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
                         FDOM(ppb):%s-%s",
                         
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
                         Solar_720_min,Solar_720_max,Battery_720_min,Battery_720_max,
                         Hydrocat_720_salinity_min,Hydrocat_720_salinity_max,
                         Hydrocat_720_temp_min,Hydrocat_720_temp_max,
                         Hydrocat_720_ph_min,Hydrocat_720_ph_max,
                         Hydrocat_720_turb_min,Hydrocat_720_turb_max,
                         Hydrocat_720_fluor_min,Hydrocat_720_fluor_max,
                         Hydrocat_720_oxy_min,Hydrocat_720_oxy_max,
                         SUNA_720_nitrate_min,SUNA_720_nitrate_max,
                         Hydrocycle_720_phosphate_min,Hydrocycle_720_phosphate_max,
                         ECO_720_FDOM_min,ECO_720_FDOM_max)

bodyWithAttachment <- list(qc_body_text,mime_part(x=file_path_720,name=file_name_720),mime_part(x=file_path_620,name=file_name_620))
sendmailV <- Vectorize( sendmail , vectorize.args = "to" )
emails <- c( "kristofer_gomes@uri.edu","davies@uri.edu" )
#sendmailV("noreply@brown.edu",emails,qc_subject_text,bodyWithAttachment,control=Server)




