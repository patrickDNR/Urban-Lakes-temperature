#This script will compare lake surface temperature to air temperature
#reconstructions to see how they match up in time
#Patrick Kelly
#last edit: 7 May 2024

library(tidyverse)
library(lubridate)
library(car)
library(gratia)
library(mgcv)
library(tidymv)
library(grid)
library(gridExtra)
library(daymetr)
library(trend)
library(itsadug)

#load the data
#Chnage to your file path
temp.slope <- read.csv('data/summarized data/tempSlope.csv')
bf.slope <- read.csv('data/summarized data/buoyancyFreq_slope.csv')

#Need to make a dataset of of mean air temps
#can only do for N.American lakes
na.slope <- temp.slope %>%
  filter(Country.of.Lake == 'United States' | Country.of.Lake == 'USA' | 
           Country.of.Lake == 'US' | Country.of.Lake == 'Canada')

#add years
na.slope <- left_join(na.slope, years, by = c('lakeID' = 'lakeName'))

#Make a few empty columns to populate with data from the daymetr output using
#latitude and longitude to get data
na.slope$air.mean..slope <- NA
na.slope$air.max.slope <- NA
na.slope$air.min.slope <- NA
na.slope$summer.mean.slope <- NA
na.slope$winter.mean.slope <- NA
na.slope$srad.slope <- NA
air.temps <- c()
summerTemps <- c() 
winterTemps <- c()
for(i in 1:nrow(na.slope)){
  tryCatch({
    lakei <- download_daymet(site = na.slope$lakeID[i], 
                             lat = na.slope$Latitude[i], 
                             lon = na.slope$Longitude[i], 
                             start = na.slope$year.min[i], 
                             end = na.slope$year.max[i],
                             internal = T) #This is the function that pulls the data
    
    lakei$data <- lakei$data %>%
      mutate(tmean = (tmax..deg.c. + tmin..deg.c.)/2,
             date = as.Date(paste(year, yday, sep = "-"), "%Y-%j"), 
             month = format(ymd(date), '%m')) #reformt for data frame
    
    avg.year.lakei <- aggregate(cbind(lakei$data$tmean, lakei$data$prcp..mm.day., 
                                      lakei$data$srad..W.m.2., lakei$data$tmax..deg.c., 
                                      lakei$data$tmin..deg.c.), 
                                by = list(lakei$data$year), 
                                mean, na.rm = T) %>%
      rename('year' = 'Group.1', 'tmean' = 'V1', 'prcp.mm.day' = 'V2', 'srad.W.m2' = 'V3', 
             'tmax.degC' = 'V4', 'tmin.degC' = 'V5') #average this by year
    
    
    summer.mean.temp <- aggregate(lakei$data$tmean[lakei$data$month == '05' |
                                                     lakei$data$month == '06' |
                                                     lakei$data$month == '07' |
                                                     lakei$data$month == '08' |
                                                     lakei$data$month == '09'],
                                  by = list(lakei$data$year[lakei$data$month == '05' |
                                                              lakei$data$month == '06' |
                                                              lakei$data$month == '07' |
                                                              lakei$data$month == '08' |
                                                              lakei$data$month == '09']), 
                                  mean, na.rm = T)
    summer.mean.temp$lakeID <- na.slope$lakeID[i]
    summerTemps <- rbind(summerTemps, summer.mean.temp) #Also get this for summer
    
    
    winter.mean.temp <- aggregate(lakei$data$tmean[lakei$data$month == '10' |
                                                     lakei$data$month == '11' |
                                                     lakei$data$month == '12' |
                                                     lakei$data$month == '01' |
                                                     lakei$data$month == '02'],
                                  by = list(lakei$data$year[lakei$data$month == '10' |
                                                              lakei$data$month == '11' |
                                                              lakei$data$month == '12' |
                                                              lakei$data$month == '01' |
                                                              lakei$data$month == '02']), 
                                  mean, na.rm = T)
    winter.mean.temp$lakeID <- na.slope$lakeID[i]
    winterTemps <- rbind(winterTemps, winter.mean.temp) #And for winter
    
    avg.year.lakei$lakeID <- na.slope$lakeID[i]
    avg.year.lakei$lat <- na.slope$Latitude[i]
    avg.year.lakei$lon <- na.slope$Longitude[i]
    
    
    
    air.temps <- rbind(avg.year.lakei, air.temps)
    #now need to get slope...
    
    na.slope$air.mean.slope[i] <- sens.slope(avg.year.lakei$tmean)$estimate
    na.slope$air.max.slope[i] <- sens.slope(avg.year.lakei$tmax.degC)$estimate
    na.slope$air.min.slope[i] <- sens.slope(avg.year.lakei$tmin.degC)$estimate
    na.slope$srad.slope[i] <- sens.slope(avg.year.lakei$srad.W.m2)$estimate
    na.slope$winter.mean.slope[i] <- sens.slope(winter.mean.temp$x)$estimate
    na.slope$summer.mean.slope[i] <- sens.slope(summer.mean.temp$x)$estimate
    
    
  }, 
  error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


#remove lakes with no lake area
na.slope <- na.slope %>%
  filter(Lake.Area..m.2. > 0)

#Try GAM analysis with slope
mod.air <- gam(slope ~ s(perag, k = 15) +
                 s(air.mean.slope, k = 15) + 
                 s(log10(Lake.Area..m.2.), k = 15) +
                 s(abs(Latitude), k = 15), 
               data = na.slope, 
               method = 'REML')
summary(mod.air)
plot(mod.air)

