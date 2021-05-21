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

#set time range of interest
#dates of interest: 10 Jan 2012 to 28 Jan 2012, 13 dec 2012 to 9 jan 2013, 4 jan 2015 to 3 feb 2015 (14:15 to 14:00)

datemin <- "2012-12-13 00:00:00"
datemax <- "2013-01-09 23:55:00"
tmin <- (as.POSIXct(datemin))
tmax <- (as.POSIXct(datemax))

palette(c("darkorchid1","blue","firebrick1","gray60","black"))

#exports icRH plot

ploticRH <- ggplot(garmet, aes(x=measure_date, y=icecliffRH, color=surfaceclass)) +
  geom_point(size = 2, stroke = 0, shape = 16) +
  ylim(0,100) +
  xlim(tmin, tmax) +
  scale_color_manual(values=c("darkorchid1","blue","firebrick1","gray60","black")) +
  xlab("Time") + ylab("Ice Cliff Boundary Layer RH (%)") +
  theme(panel.background = element_rect("white", "black")) +
  theme(panel.grid.major = element_line("gray"),
    panel.grid.minor = element_line("gray")) +
  labs(color = "Surface Class")
 

#export as 13 inches by 5.18 pdf

#creates T plots
plotTs <- ggplot(garmet, aes(x=measure_date)) +
  geom_point(aes(y=icecliffsurftemp, color=surfaceclass), size = 2, stroke = 0, shape = 16) +
  #geom_point(aes(y=AirT, color=surfaceclass), size = 2, stroke = 0, shape = 17) +
  ylim(-10,10) +
  xlim(tmin, tmax) +
  scale_color_manual(values=c("darkorchid1","blue","firebrick1","gray60","black")) +
  xlab("Time") + ylab("Ice Cliff Surface Temperature (˚C)") +
  theme(panel.background = element_rect("white", "black")) +
  theme(panel.grid.major = element_line("gray"),
        panel.grid.minor = element_line("gray")) +
  labs(color = "Surface Class")

#creates Tair plots
plotTa <- ggplot(garmet, aes(x=measure_date)) +
  #geom_point(aes(y=icecliffsurftemp, color=surfaceclass), size = 2, stroke = 0, shape = 16) +
  geom_point(aes(y=AirT, color=surfaceclass), size = 2, stroke = 0, shape = 17) +
  ylim(-10,10) +
  xlim(tmin, tmax) +
  scale_color_manual(values=c("darkorchid1","blue","firebrick1","gray60","black")) +
  xlab("Time") + ylab("Air Temperature (˚C)") +
  theme(panel.background = element_rect("white", "black")) +
  theme(panel.grid.major = element_line("gray"),
        panel.grid.minor = element_line("gray")) +
  labs(color = "Surface Class")

#creates Tdif plots
plotTdif <- ggplot(garmet, aes(x=measure_date)) +
  #geom_point(aes(y=icecliffsurftemp, color=surfaceclass), size = 2, stroke = 0, shape = 16) +
  geom_point(aes(y=Tdif, color=surfaceclass), size = 2, stroke = 0, shape = 16) +
  ylim(-5,7.5) +
  xlim(tmin, tmax) +
  scale_color_manual(values=c("darkorchid1","blue","firebrick1","gray60","black")) +
  xlab("Time") + ylab("Ice Cliff Surface Temperature - Air Temperature (˚C)") +
  theme(panel.background = element_rect("white", "black")) +
  theme(panel.grid.major = element_line("gray"),
        panel.grid.minor = element_line("gray")) +
  labs(color = "Surface Class")+
  geom_hline(yintercept = 0, size = 1)

ploticRH
plotTs
plotTa
plotTdif

#export as 13 inches by 5.18 pdf
  
  # this in it in lattice: 

xyplot(icecliffRH ~ measure_date, garmet, pch=20, ylim = c(0,100), xlim=c(tmin,tmax), groups = surfaceclass, auto.key = TRUE, xlab = "Time", ylab = "Boundary Layer RH (%)")
plotRH <- xyplot(RH ~ measure_date, garmet, pch=20, ylim = c(0,100), xlim=c(tmin, tmax), groups = surfaceclass, auto.key = TRUE, xlab = "Time", ylab = "2 m Air RH (%)")





# box plots by surface class

boxplot(RH ~ surfaceclass, data=garmet, ylab="Raw RH", xlab="Surface Class")

boxplot(icecliffRH ~ surfaceclass, data=garmet, ylab="IC RH", xlab="Surface Class")

library(ggplot2)
library(ggsignif)
library(ggpubr)

ggplot(garmet, aes(x=surfaceclass, y=RH)) + 
  geom_boxplot() +
  geom_signif(comparisons = list(c("B","D","SC","T", "E")), 
              map_signif_level=TRUE)

ggplot(garmet, aes(x=surfaceclass, y=icecliffRH)) + 
  geom_boxplot() +
  geom_signif(comparisons = list(c("B","D","SC","T", "E")), 
              map_signif_level=TRUE)

#plot box plots using multiple comparisons. From http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/

quartz()
compare_means(icecliffRH ~ surfaceclass,  data = garmet)
my_comparisons <- list( c("B", "D"), c("B", "E"), c("D", "E") )
ggboxplot(garmet, x = "surfaceclass", y = "icecliffRH",
          color = "surfaceclass", palette = "jco")+ 
  stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  labs(color = "Surface Class")+
  xlab("Surface Class") + ylab("Ice Cliff Boundary Layer RH (%)") 

quartz.save(file="figs/surface_RH_box_plots.tif", type = "tiff", device = dev.cur(), dpi = 600)

#plot box plots using multiple comparisons for surface T. From http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/

quartz()
compare_means(icecliffsurftemp ~ surfaceclass,  data = garmet)
my_comparisons <- list( c("B", "D"), c("B", "E"), c("D", "E") )
ggboxplot(garmet, x = "surfaceclass", y = "icecliffsurftemp",
          color = "surfaceclass", palette = "jco")+ 
  stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  labs(color = "Surface Class")+
  xlab("Surface Class") + ylab("Ice Cliff Surface Temperature (˚C)") 

quartz.save(file="figs/surface_temp_box_plots.tif", type = "tiff", device = dev.cur(), dpi = 600)

#values for icecliffrh medians

justEs <- garmet[garmet$surfaceclass=="E",19]
Emedian <- median(justEs)
Esd <- sd(justEs)
justBs <- garmet[garmet$surfaceclass=="B",19]
Bsd <- sd(justBs)
Bmedian <- median(justBs)
justDs <- garmet[garmet$surfaceclass=="D",19]
Dsd <- sd(justDs)
Dmedian <- median(justDs)
justSCs <- garmet[garmet$surfaceclass=="SC",19]
SCsd <- sd(justSCs)
SCmedian <- median(justSCs)
justTs <- garmet[garmet$surfaceclass=="T",19]
Tmedian <- median(justTs)
Tsd <- sd(justTs)

#values for icecliff T medians

justEs_T <- garmet[garmet$surfaceclass=="E",14]
Emedian_T <- median(justEs_T)
Esd_T <- sd(justEs_T)
justBs_T <- garmet[garmet$surfaceclass=="B",14]
Bsd_T <- sd(justBs_T)
Bmedian_T <- median(justBs_T)
justDs_T <- garmet[garmet$surfaceclass=="D",14]
Dsd_T <- sd(justDs_T)
Dmedian_T <- median(justDs_T)
justSCs_T <- garmet[garmet$surfaceclass=="SC",14]
SCsd_T <- sd(justSCs_T)
SCmedian_T <- median(justSCs_T)
justTs_T <- garmet[garmet$surfaceclass=="T",14]
Tmedian_T <- median(justTs_T)
Tsd_T <- sd(justTs_T)


#values for icecliff tdiff medians

justEs_tdif <- garmet[garmet$surfaceclass=="E",21]
Emedian_tdif <- median(justEs_tdif)
Esd_tdif <- sd(justEs_tdif)
justBs_tdif <- garmet[garmet$surfaceclass=="B",21]
Bsd_tdif <- sd(justBs_tdif)
Bmedian_tdif <- median(justBs_tdif)
justDs_tdif <- garmet[garmet$surfaceclass=="D",21]
Dsd_tdif <- sd(justDs_tdif)
Dmedian_tdif <- median(justDs_tdif)
justSCs_tdif <- garmet[garmet$surfaceclass=="SC",21]
SCsd_tdif <- sd(justSCs_tdif)
SCmedian_tdif <- median(justSCs_tdif)
justTs_tdif <- garmet[garmet$surfaceclass=="T",21]
Tmedian_tdif <- median(justTs_tdif)
Tsd_tdif <- sd(justTs_tdif)



#plot histograms of icecliffRH for D and E conditions

mycol <- rgb(0, 0, 255, max = 255, alpha = 125, names = "blue50")
c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

b <- min(c(justEs,justDs))-0.001 # Set the minimum for the breakpoints
e <- max(c(justEs,justDs)) # Set the maximum for the breakpoints
ax <- pretty(b:e, n = 12) # Make a neat vector for the breakpoints


hgA <- hist(justEs, breaks = ax, plot = FALSE) # Save first histogram data
hgB <- hist(justDs, breaks = ax, plot = FALSE) # Save 2nd histogram data

quartz()
plot(hgA, col = c2,main="Efflorescence and Deliquescence Ice Cliff Boundary Layer RH Values",
     xlab="Ice Cliff RH (%)",
     ylab="Frequency") # Plot 1st histogram using a transparent color
plot(hgB, col = c1, add = TRUE) # Add 2nd histogram using different color
legend("topright", c("Deliquescence", "Efflorescence"), fill=c(c2, c1))

quartz.save(file="figs/D_E_histo_magnus.tif", type = "tiff", device = dev.cur(), dpi = 600)

