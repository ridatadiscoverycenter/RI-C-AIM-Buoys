library("tidyverse")
library("lubridate")
library("oce")
library('ggplot2')
library('ggpubr')
library('plyr')
library('dplyr')
library('scales')
library('readr')
library('httr')
library('jsonlite')
library('gmailr')
library("httpuv")
library("MatrixGenerics")

https://datacornering.com/how-to-get-previous-or-next-record-in-r/


  for (measurement in Hydrocat_720$ph_despike) {
    measurement_2 <- rep((measurement),times=2)
    measurement_4 <-rep((measurement), times=4)
    lead_2 <- c(measurement[-1],measurement[-2])
    lead_4 <- c(measurement[-1],measurement[-2],measurement[-3],measurement[-4])
    
    
    Hydrocat_720 <- mutate(Hydrocat_720,
                           pH_Stuck_Value_QC = ifelse(identical(measurement_3, lead_3, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE,
                                                                ignore.bytecode = TRUE, ignore.environment = FALSE,
                                                                ignore.srcref = TRUE)==TRUE,
                                                      "1", ifelse(identical(measurement_5, lead_5, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE,
                                                                            ignore.bytecode = TRUE, ignore.environment = FALSE,
                                                                            ignore.srcref = TRUE)==TRUE,"2","0")))
  }   
  
  
  
  
  
  
  
  








for (measurement in Hydrocat_720$ph_despike) {
  measurement_2 <- rep((measurement),times=2)
  measurement_4 <-rep((measurement), times=4)
  lead_2 <- c(lead(measurement, n=2))
  lead_4 <- (lead(measurement, n=4))
  
  
  Hydrocat_720 <- mutate(Hydrocat_720,
         pH_Stuck_Value_QC = ifelse(identical(measurement_3, lead_3, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE,
                                                    ignore.bytecode = TRUE, ignore.environment = FALSE,
                                                    ignore.srcref = TRUE)==TRUE,
                                          "1", ifelse(identical(measurement_5, lead_5, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE,
                                                                ignore.bytecode = TRUE, ignore.environment = FALSE,
                                                                ignore.srcref = TRUE)==TRUE,"2","0")))
} 
#Loop seems to be working mutate but is stuck values are not be detected (everything is getting a 0)

#problem may be with the identical function options, need to mke sure what each one is or it may be that lag and lead are not working as though, look into "shift" functions

#lead and lag are most

shift()


test <- lead(Hydrocat_720$ph_despike,n=2)




identical(measurement_3, lead_3, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE,
          ignore.bytecode = TRUE, ignore.environment = FALSE,
          ignore.srcref = TRUE)==TRUE



mutate(Hydrocycle_720,
       Phosphate_Seasonal_NB_Range_QC = ifelse(Hydrocycle_720$Phosphate_despike > max(seasonal_nb_DIP_range),
                                               "2", ifelse(Hydrocycle_720$Phosphate_despike < min(seasonal_nb_DIP_range),"1","0")))




test <- c(2.2,2.2,2.2,2.2,2.3)
test_2 <- test

test_3 <-  rep(c(28.94),length.out=4,times=3)

print(measurement_3, digits=6)

identical(test, test_2, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE,
          ignore.bytecode = TRUE, ignore.environment = FALSE,
          ignore.srcref = TRUE)






identical(Hydrocat_720$`salinity_despike`, lag(Hydrocat_720$salinity_despike, n=5), num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE,
          ignore.bytecode = TRUE, ignore.environment = FALSE,
          ignore.srcref = TRUE)



#if (colAlls(lead_3, rows = NULL, cols = NULL, value = measurement, na.rm = FALSE)==TRUE) Hydrocat_720$Salinity_Stuck_Value_QC== "1"