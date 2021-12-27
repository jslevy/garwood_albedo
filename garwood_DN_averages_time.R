### set up working environment ----
# clear working directory
rm (list = ls())

# load required packages 
#install.packages(c("raster", "rgdal")) - use this to install if needed
library(rgdal)
library(Rcpp)
library(raster)
library(RStoolbox)

# set working directory
setwd("Z:/timelapse/2012/")

### process data ----
# create a vector with each file to be processed
f <- list.files(pattern = glob2rx("*.jpg"))

# create a vector of output file names
o <- gsub(".jpg", "_cropped.tif", f)

#initialize log

image_log <- data.frame(image=character(),
                       meanDNnorm=double(),
                       stringsAsFactors=FALSE) 

# process all files in the using a for loop
for(i in 1:length(f))
{
  #makes raster object from file
  r <- stack(f[i])

  #normalize to brightest pixel in scene. This includes stuff outside of the crop, but not the black and white labels
  #for 2012 its rows 0-700, columns 1-767
  cleanview <- crop(r,extent(r,0,700,1,767))
  maxRbands <- cellStats(cleanview,stat='max', na.rm = TRUE)
  maxR <- max(maxRbands[1:3])
  
  #crops the raster. for 2012, rows 300-550, columns 350-550
  r <- crop(r,extent(r,300,550,350,550))
  
  #finds average value of each band in the crop
  meanRbands <- cellStats(r,stat='mean',na.rm=TRUE)
  
  #converts to average DN of R, G, andd B, ignoring alpha channel
  meanR <- mean(meanRbands[1:3])
  
  #logs values
  this_image_log <- data.frame(image=c(f[i]),meanDNnorm=meanR/maxR)
  image_log <- rbind(image_log,this_image_log)
  
  #write cropped image
  #writeRaster(r,filename = o[i],format = "GTiff", overwrite = TRUE)
}

          
#convert image name to date and time

require(lubridate)

image_log$date <- substr(image_log$image,16,25)
image_log$time <- substr(image_log$image,27,34)
image_log$datetime <- substr(image_log$image,16,34)

image_log$measure_date <- as.POSIXct(image_log$datetime, format = "%Y-%m-%d_%H-%M-%S")
image_log$dateonly <- date(image_log$measure_date)
image_log$deci_date <- decimal_date(image_log$dateonly)
image_log$hour <- hour(image_log$measure_date)
image_log$minute <- minute(image_log$measure_date)
image_log$second <- second(image_log$measure_date)

image_log$albedo_decimal_year <- as.double(image_log$deci_date+(image_log$hour/(24*365))+(image_log$minute/(60*24*365)))


#write log to csv
write.csv(image_log,"2012_image_log.csv")

#plot whole record
plot(image_log$meanDNnorm~image_log$measure_date, xlab = "Date", ylab = "Mean Normalized DN", pch = ".", ylim = c(0.3,0.8))

datemin <- "2012-01-11 00:00:00"
datemax <- "2012-01-14 00:00:00"
tmin <- (as.POSIXct(datemin))
tmax <- (as.POSIXct(datemax))
tmindec <- decimal_date(tmin)
tmaxdec <- decimal_date(tmax)

par(mar=c(8,4,2,1))
plot(image_log$meanDNnorm~image_log$albedo_decimal_year, xlim = c(tmindec,tmaxdec),xlab = "Date",ylab = "Mean Normalized DN", pch = ".", ylim = c(0.4,0.8), xaxt='n')
axis(1,xaxp=c(tmindec+(0.5/365), tmaxdec-(0.5/365), 2))
abline(v = seq(tmindec+(0.33/365), tmaxdec+(0.33/365), by=(1/(365))), lty = 2)

#compare brightness to closest RH

#gets data and makes date as well as decimal date-time for comparison to albedo datetime
garmet <- read.csv("garmet_coded.csv")
garmet$measure_date <- as.POSIXct(garmet$date, format = "%m/%d/%Y %H:%M")
garmet$dateonly <- date(garmet$measure_date)
garmet$deci_date <- decimal_date(garmet$dateonly)
garmet$hour <- hour(garmet$measure_date)
garmet$minute <- minute(garmet$measure_date)
garmet$second <- second(garmet$measure_date)

garmet$cliff_decimal_year <- as.double(garmet$deci_date+(garmet$hour/(24*365))+(garmet$minute/(60*24*365)))

#makes soil RH
# recalculates ice cliff RH using magnus formula

garmet$satvappresair <- ifelse(garmet$AirT > 0, 610.94*exp((17.625*garmet$AirT)/(243.04+garmet$AirT)), 611.21*exp((22.587*garmet$AirT)/(273.86+garmet$AirT))) 
garmet$satvapprescliff <- ifelse(garmet$icecliffsurftemp > 0, 610.94*exp((17.625*garmet$icecliffsurftemp)/(243.04+garmet$icecliffsurftemp)), 611.21*exp((22.587*garmet$icecliffsurftemp)/(273.86+garmet$icecliffsurftemp)))

garmet$pvapair <- (garmet$RH/100)*garmet$satvappresair

garmet$icecliffRH <- (garmet$pvapair/garmet$satvapprescliff)*100

#trims off just time and soilRH
garmet_trimmed <- garmet[,c(20,27)]


#  Load package
require( data.table )

#  Make data.frames into data.tables with a key column
ldt <- data.table(image_log)
setkey(ldt,albedo_decimal_year)
dt <- data.table(garmet_trimmed)
setkey(dt,cliff_decimal_year)



#  Join based on the key column of the two tables (image_log$measure_date and garmet_trimmed$cliff_time)
#  roll = "nearest" gives the desired behaviour
df2 <- ldt[dt, roll = "nearest"]


plot(df2$meanDNnorm~df2$icecliffRH)
