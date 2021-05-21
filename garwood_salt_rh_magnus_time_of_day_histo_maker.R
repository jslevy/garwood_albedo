#This is code converts raw Garwood met data to ice cliff boundary layer surface RH

#import from raw csvs

#sets path and object names

setwd("~/Dropbox/r_projects/garwood_salt")
garmet <- read.csv("raw_data/garmet_coded.csv")
garmet$datetime <- strptime(garmet$Date,format='%m/%d/%y %H:%M')  as.Date(garmet$Date, format = "%m/%d/%Y %H:%M")

# By the way, there was an error where some surface classes were coded as "B" and others as "B ". 
#To remove them: 

garmet$surfaceclass <- gsub(' ', '', garmet$surfaceclass)


# recalculates ice cliff RH using magnus formula

garmet$satvappresair <- ifelse(garmet$AirT > 0, 610.94*exp((17.625*garmet$AirT)/(243.04+garmet$AirT)), 611.21*exp((22.587*garmet$AirT)/(273.86+garmet$AirT))) 
garmet$satvapprescliff <- ifelse(garmet$icecliffsurftemp > 0, 610.94*exp((17.625*garmet$icecliffsurftemp)/(243.04+garmet$icecliffsurftemp)), 611.21*exp((22.587*garmet$icecliffsurftemp)/(273.86+garmet$icecliffsurftemp)))

garmet$pvapair <- (garmet$RH/100)*garmet$satvappresair

garmet$icecliffRH <- (garmet$pvapair/garmet$satvapprescliff)*100

garmet$RHdif <- garmet$icecliffRH/garmet$RH

garmet$Tdif <- garmet$icecliffsurftemp - garmet$AirT

library(ggplot2)
library(latticeExtra)

#turn date/time stamps machine readable
garmet$measure_date <- as.POSIXct(garmet$Date, format = "%m/%d/%y %H:%M")
garmet$numdate <- as.numeric(garmet$measure_date)
garmet$datedate <- as.Date(garmet$measure_date)
garmet$hour <- hour(garmet$measure_date)



#make fraction of year histogram



#hour of occurrence of wet conditions. note, y axis needs to be divided by 4

#wet conditions = surface RH > 79.9

wet_conditions <- garmet[garmet$icecliffRH>79.9,]
wet_hours <- table(unlist(wet_conditions$hour))
barplot(wet_hours, space=0, main = "Timing of Potential Deliquesced Conditions", xlab = "Hour of Day", ylab="Hours of Record")

quartz.save(file="figs/samples_with_rh_79_9.tif", type = "tiff", device = dev.cur(), dpi = 600)



