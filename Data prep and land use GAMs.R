#This script assesses the influence of land use on trends in lake water 
#temperatures and buoyancy frequency
#Patrick Kelly
#last edit: 7 May 2024

#start with necessary packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(trend)
library(mgcv)
library(rLakeAnalyzer)
library(mgcViz)
library(tidymv)
library(laketemps)
library(car)
library(daymetr)

#load temperature data from the jane dataset

#####Replace file path with your own!
temp <- read.csv('data/jane_data/interpolated_data.csv')

use <- read.csv('data/jane_data/use_data.csv') #includes land use info

info <- read.csv('data/jane_data/info_data.csv') #includes other info

#get international and US data from other datasets

#####Replace file path with your own
int.landuse <- read.csv('Data/Internationallakes_landuse_amend.csv')
us.landuse <- read.csv('Data/USlakes_landuse_amend.csv')
rand.landuse <- read.csv('Data/randomlakes_wlandcoverfinal.csv')

#get all like columns together
#Need to combine some data for US lakes
int.landuse <- int.landuse %>%
  select(LakeName, Lake.Ab., Country.of.Lake, Latitude, Longitude, Study, 
         Lake.Area..m.2., Trees, Crops, Built.Area, Rangeland)
rand.landuse <- rand.landuse %>%
  select(LakeName, Lake.Ab., Country.of.Lake, Latitude, Longitude, Study, 
         Lake.Area..m.2., Trees, Crops, Built.Area, Rangeland)

#combine different intensities into one category of urban land use
us.landuse$Built.Area <- us.landuse$Developed..High.Intensity + 
  us.landuse$Developed..Low.Intensity + us.landuse$Developed..Medium.Intensity +
  us.landuse$Developed..Open.Space

#combine forest types
us.landuse$Trees <- us.landuse$Deciduous.Forest + us.landuse$Evergreen.Forest +
  us.landuse$Mixed.Forest

us.landuse$Crops <- us.landuse$Cultivated.Crops
us.landuse$Rangeland <- us.landuse$Pasture.Hay

us.landuse <- us.landuse %>%
  select(LakeName, Lake.Ab., Country.of.Lake, Latitude, Longitude, Study, 
         Lake.Area..m.2., Trees, Crops, Built.Area, Rangeland)

#should make it all match the combined use and info data frames
landuse <- rbind(int.landuse, us.landuse, rand.landuse)


#Sharma et al. lakes -- lakes that were not included in the Pilla or Jane
#datasets but that still have summer surface temperature data
sharm <- landuse %>%
  filter(Study == 'Sharma')

#remove lake that doesn't have info
sharm <- sharm[-4,]

#Get accompanying metadata information form each Sharma et al. lake
sharm.data <- c()
for(i in 1:nrow(sharm)){
  lakei <- sharm$LakeName[i]
  metai <- get_metadata(lakei)
  metai <- data.frame(Lake.name = metai$Lake.name, latitude = metai$latitude, 
                      longitude = metai$longitude, 
                      surface.area.km2 = metai$surface.area.km2, 
                      elevation.m = metai$elevation.m, 
                      max.depth.m = metai$max.depth.m)
  tempi <- get_surface_temps(lakei)
  tempi$Lake.name = sharm$LakeName[i]
  x <- left_join(tempi,metai, by = 'Lake.name')
  sharm.data <- rbind(sharm.data,x)
}

#combine with landuse info
sharm.data <- left_join(sharm, sharm.data, by = c('LakeName' = 'Lake.name'))


#now get Pilla et al. lake data
pilla <- landuse %>%
  filter(Study == 'Pilla')

pilla <- pilla$LakeName

#load pilla lake data
pilla.info <- read.csv('data/Pilla_temp data/SiteInformation.csv')
pilla.temp <- read.csv('data/Pilla_temp data/TempProfiles_Interpolated.csv')

#use only pilla lakes
pilla.data <- pilla.temp[pilla.temp$LakeName %in% pilla,]

pilla.data <- left_join(pilla.info, pilla.data, by = 'LakeName')

#add land use
pilla <- left_join(pilla.data, landuse, by = 'LakeName')

#remove data where we don't have landuse categories
pilla <- pilla %>%
  filter(!is.na(Built.Area))

#add month to data
pilla$month <- as.numeric(format(ymd(pilla$Date), '%m'))


#let's only use summer
#first get vector of lakes to loop through
pilla.lakes <- unique(pilla$LakeName)

#Grab summer months for each hemisphere
pilla.summer <- c()
for(i in 1:length(pilla.lakes)){
  lakei <- pilla[pilla$LakeName == pilla.lakes[i],]
  
  lat <- lakei$Latitude.x[1]
  x <- c()
  if(lat > 0){
    x <- lakei[lakei$month>=5 & lakei$month<=9,]
  }
  
  if(lat < 0){
    x <- lakei[lakei$month == 11 | lakei$month == 12 |
                 lakei$month  == 1 | lakei$month == 2,]
  }
  pilla.summer <- rbind(pilla.summer, x)
}

#Now do the same thing for Jane data
#first combine it all into one data frame
jane <- left_join(info, use, by = 'lake_id') %>%
  left_join(., temp, by = 'lake_id')

jane$month <- as.numeric(format(ymd(jane$date), '%m'))

#get rid of NAs
jane <- jane %>%
  filter(!is.na(name))

jane.lakes <- unique(jane$name)
#Just get summer data
jane.summer <- c()
for(i in 1:length(jane.lakes)){
  lakei <- jane[jane$name == jane.lakes[i],]
  
  lat <- lakei$lat[1]
  x <- c()
  if(lat > 0){
    x <- lakei[lakei$month>=5 & lakei$month<=9,]
  }
  
  if(lat < 0){
    x <- lakei[lakei$month == 11 | lakei$month == 12 |
                 lakei$month  == 1 | lakei$month == 2,]
  }
  jane.summer <- rbind(jane.summer, x)
}


#Need to get all of these into the same format
sharm.data$depth <- 0
sharm.data <- sharm.data %>%
  mutate(perag = Crops + Rangeland) %>%
  rename('perfor' = "Trees", 'perdev' = 'Built.Area', 
         'Temperature_degCelsius' = 'Lake.Temp.Summer.InSitu', 
         'MaxDepth_m' = 'max.depth.m') %>%
  select(LakeName, Country.of.Lake, Latitude, Longitude, Lake.Area..m.2., MaxDepth_m,
         perag, perdev, perfor, depth, year, Temperature_degCelsius)

jane.all <- jane %>%
  mutate(Lake.Area..m.2. = surf_area*10000) %>%
  mutate(perag = perag/100, perfor = perfor/100, perdev = perdev/100) %>%
  rename('LakeName' = 'name', 'Country.of.Lake' = 'country', 'Latitude' = 'lat', 
         'Longitude' = 'long',
         'MaxDepth_m' = 'max_depth', 'Temperature_degCelsius' = 'temp', 
         'Date' = 'date') %>%
  select(LakeName, Country.of.Lake, Latitude, Longitude, Lake.Area..m.2.,MaxDepth_m,
         perag, perdev, perfor, depth, Date, Temperature_degCelsius)

#do it for summer data too...
jane.all.summer <- jane.summer %>%
  mutate(Lake.Area..m.2. = surf_area*10000) %>%
  mutate(perag = perag/100, perfor = perfor/100, perdev = perdev/100) %>%
  rename('LakeName' = 'name', 'Country.of.Lake' = 'country', 'Latitude' = 'lat', 
         'Longitude' = 'long',
         'MaxDepth_m' = 'max_depth', 'Temperature_degCelsius' = 'temp', 
         'Date' = 'date') %>%
  select(LakeName, Country.of.Lake, Latitude, Longitude, Lake.Area..m.2.,
         MaxDepth_m, perag, perdev, perfor, depth, Date, Temperature_degCelsius)

#Now for pilla data
pilla <- pilla %>%
  mutate(perag = Crops + Rangeland) %>%
  rename('perfor' = "Trees", 'perdev' = 'Built.Area', 
         'Latitude' = 'Latitude.x', 'Longitude' = 'Longitude.x', 
         'depth' = 'Depth_m') %>%
  select(LakeName, Country.of.Lake, Latitude, Longitude, Lake.Area..m.2.,MaxDepth_m,
         perag, perdev, perfor, depth, Date, Temperature_degCelsius)

pilla.summer <- pilla.summer %>%
  mutate(perag = Crops + Rangeland) %>%
  rename('perfor' = "Trees", 'perdev' = 'Built.Area', 
         'Latitude' = 'Latitude.x', 'Longitude' = 'Longitude.x', 
         'depth' = 'Depth_m') %>%
  select(LakeName, Country.of.Lake, Latitude, Longitude, Lake.Area..m.2.,MaxDepth_m,
         perag, perdev, perfor, depth, Date, Temperature_degCelsius)

#Let's start with surface temps
#Need to aggregate the pilla and jane data by year
jane.all.summer$year <- format(ymd(jane.all.summer$Date), '%Y')

jane.summer.surface <- aggregate(cbind(jane.all.summer$Temperature_degCelsius, 
                                       jane.all.summer$Latitude, 
                                       jane.all.summer$Longitude, 
                                       jane.all.summer$Lake.Area..m.2., 
                                       jane.all.summer$MaxDepth_m,
                                       jane.all.summer$perag, 
                                       jane.all.summer$perdev, 
                                       jane.all.summer$perfor), 
                                 by = list(jane.all.summer$LakeName, 
                                           jane.all.summer$Country.of.Lake, 
                                           jane.all.summer$year), 
                                 mean, na.rm = T)

#use only post 1980
jane.summer.num <- jane.summer.num %>%
  filter(year >=1980)

#rename all columns to make sense
colnames(jane.summer.surface) <- c('LakeName', 'Country.of.Lake','year',
                                   'Temperature_degCelsius','Latitude',
                                   'Longitude','Lake.Area..m.2.','MaxDepth_m',
                                   'perag', 'perdev','perfor')


#remove where we don't have data
jane.summer.surface <- jane.summer.surface %>%
  filter(!is.na(perdev))

#add depth - should all be 0 since it is just from the surface
jane.summer.surface$depth = 0

#do the same with pilla
pilla.summer$year <- format(ymd(pilla.summer$Date), '%Y')

pilla.summer.surface <- aggregate(cbind(pilla.summer$Temperature_degCelsius, 
                                        pilla.summer$Latitude, 
                                        pilla.summer$Longitude, 
                                        pilla.summer$Lake.Area..m.2., 
                                        pilla.summer$MaxDepth_m,
                                        pilla.summer$perag, 
                                        pilla.summer$perdev, 
                                        pilla.summer$perfor), 
                                  by = list(pilla.summer$LakeName, 
                                            pilla.summer$Country.of.Lake, 
                                            pilla.summer$year), 
                                  mean, na.rm = T)

colnames(pilla.summer.surface) <- c('LakeName', 'Country.of.Lake','year',
                                    'Temperature_degCelsius','Latitude',
                                    'Longitude','Lake.Area..m.2.', 'MaxDepth_m',
                                    'perag', 'perdev','perfor')
pilla.summer.surface$depth = 0


#now need to get them in the same order as Sharma
jane.summer.surface <- jane.summer.surface %>%
  select(LakeName, Country.of.Lake, Latitude, Longitude, Lake.Area..m.2.,
         MaxDepth_m, perag, perdev, perfor, depth, year, Temperature_degCelsius)

pilla.summer.surface <- pilla.summer.surface %>%
  select(LakeName, Country.of.Lake, Latitude, Longitude, Lake.Area..m.2., MaxDepth_m,
         perag, perdev, perfor, depth, year, Temperature_degCelsius)


#combine them all together!
surface.data <- rbind(sharm.data, jane.summer.surface, pilla.summer.surface)


#use only data past 1980
surface.data.1980 <- surface.data %>%
  filter(year>=1980)

#make a table with lake and accompanying information
lakes <- unique(surface.data.1980$LakeName)
lake.table <- c()
for(i in 1:length(lakes)){
  lakei <- surface.data[surface.data.1980$LakeName == lakes[i],]
  year.start <- as.numeric(min(lakei$year))
  year.end <- as.numeric(max(lakei$year))
  
  x <- data.frame(lakei[1,1:9], year.start = year.start, year.end = year.end)
  lake.table <- rbind(lake.table, x)
}

#Now find the sens slope for all of the lakes - make sure to include 
#accompanying data...
#using monthly data
lakes <- unique(surface.data.1980$LakeName)

#remove weird outliers
surface.data.1980 <- surface.data.1980[-c(1099,7866),]

#Make data frame with lake slope - include p value from sens slope
lakes.slope <- data.frame(lakeID = lakes, slope = NA, 
                          slope.p = NA)
for(i in 1:length(lakes)){
  lakei <- surface.data.1980[surface.data.1980$LakeName == lakes[i],]
  
  if(nrow(lakei) >= 10){
    lakes.slope$slope[i] <- sens.slope(lakei$Temperature_degCelsius)$estimate
    lakes.slope$slope.p[i] <- round(sens.slope(lakei$Temperature_degCelsius)$p.value, 4)
  }
  if(nrow(lakei) <10){
    i = i +1
  }
}

#match with lake info
surface.all <- c()
for(i in 1:nrow(lakes.slope)){
  lakei <- surface.data.1980[surface.data.1980$LakeName == lakes[i],]
  rowi <- lakei[1,1:9]
  surface.all <- rbind(surface.all, rowi)
}

#combine data
lakes.slope <- left_join(lakes.slope, surface.all, by = c('lakeID' = 'LakeName'))

#######Now get buoyancy frequency#####
prof.data <- rbind(jane.all, pilla) #Use the data where we have depth profiles (not Sharma)


lakes <- unique(prof.data$LakeName)
b.f <- c()
for(i in 1:length(lakes)){
  lakei <- prof.data[prof.data$LakeName == lakes[i],]
  dates <- unique(lakei$Date)
  b.fi <- c()
  for(j in 1:length(dates)){
    lake.datei <- lakei[lakei$Date == dates[j],]
    
    x <- data.frame(lake = lake.datei$LakeName[1], 
                    date = lake.datei$Date[1], 
                    b.f = max(buoyancy.freq(lake.datei$Temperature_degCelsius, 
                                            lake.datei$depth)), 
                    max.depth = max(lake.datei$depth), 
                    deep.temp = lake.datei$Temperature_degCelsius[nrow(lake.datei)])
    b.fi <- rbind(b.fi, x)
  }
  b.f <- rbind(b.f, b.fi)
}

#get prof.summer
north.lakes <- prof.data %>%
  filter(Latitude > 0)
south.lakes <- prof.data %>%
  filter(Latitude <= 0)

south.lakes <- south.lakes %>%
  dplyr::filter(month == 11 | month == 12 | month == 1 | month == 2 |
                  month == 3)

north.lakes <- north.lakes %>%
  dplyr::filter(month == 5 | month == 6 | month == 7 |
                  month == 8 | month == 9)

prof.summer <- rbind(north.lakes, south.lakes)


#now get accompanying data
prof.info <- prof.data %>%
  select(LakeName, Country.of.Lake, Latitude, Longitude, Lake.Area..m.2., 
         MaxDepth_m, perag, perdev, perfor) %>%
  distinct()

#add year and month to dataset
b.f$year <- as.numeric(format(ymd(b.f$date), '%Y'))
b.f$month <- as.numeric(format(ymd(b.f$date), '%m'))

#Now lets add land use data
bf.all <- left_join(b.f, surface.all, by = c('lake' = 'LakeName'))


#Let's look just at "summer" months
north.lakes <- bf.all %>%
  filter(Latitude > 0)
south.lakes <- bf.all %>%
  filter(Latitude <= 0)

south.lakes <- south.lakes %>%
  dplyr::filter(month == 11 | month == 12 | month == 1 | month == 2 |
                  month == 3)

north.lakes <- north.lakes %>%
  dplyr::filter(month == 5 | month == 6 | month == 7 |
                  month == 8 | month == 9)

b.f.summer <- rbind(north.lakes, south.lakes)

#remove NAs
b.f.summer <- b.f.summer %>%
  filter(!is.na(b.f))

#Now let's get sens slope
lakes <- unique(b.f.summer$lake)

bf.slope <- data.frame(lakeID = lakes, slope = NA, 
                       slope.p = NA, deep.temp.slope = NA)
for(i in 1:length(lakes)){
  lakei <- b.f.summer[b.f.summer$lake == lakes[i],]
  
  if(nrow(lakei) >= 10){
    bf.slope$slope[i] <- sens.slope(lakei$b.f)$estimate
    bf.slope$slope.p[i] <- round(sens.slope(lakei$b.f)$p.value, 4)
    bf.slope$deep.temp.slope[i] <- sens.slope(lakei$deep.temp)$estimate
  }
  if(nrow(lakei) <10){
    i = i +1
  }
  
}


#add other info
bf.slope <-  left_join(bf.slope, surface.all, by = c('lakeID' = 'LakeName'))


###GAM analysis###
#For lake surface temperature trends
#some lakes have depth of 0, remove for analysis
lakes.slope <- lakes.slope %>%
  filter(Lake.Area..m.2. > 0 )

#starting with agricultural land use
ag.slope <- gam(slope ~  s(perag, k = 15) + 
                  s(log10(Lake.Area..m.2.), k = 15) +
                  s(abs(Latitude), k = 15),
                data = lakes.slope, 
                method = 'REML')
#Test for concurved slopes
concurvity(ag.slope)

#Check results
summary(ag.slope)

#visualize results
plot(ag.slope)

#now for forested
for.slope <- gam(slope ~  s(perfor, k = 15) + 
                  s(log10(Lake.Area..m.2.), k = 15) +
                  s(abs(Latitude), k = 15),
                data = lakes.slope, 
                method = 'REML')
#Test for concurved slopes
concurvity(for.slope)

#Check results
summary(for.slope)

#visualize results
plot(for.slope)

#Now for urban
dev.slope <- gam(slope ~  s(perdev, k = 15) + 
                  s(log10(Lake.Area..m.2.), k = 15) +
                  s(abs(Latitude), k = 15),
                data = lakes.slope, 
                method = 'REML')
#Test for concurved slopes
concurvity(dev.slope)

#Check results
summary(dev.slope)

#visualize results
plot(dev.slope)

#now analyses for buoyancy frequency
bf.slope <- bf.slope %>%
  filter(Lake.Area..m.2. > 0)

mod.bf.ag <- gam(slope ~  s(perag, k = 15) +
                 s(abs(Latitude), k = 15) +
                 s(log10(Lake.Area..m.2.), k = 15), 
               data = bf.slope, 
               method = 'REML')
summary(mod.bf.ag)

#forested
mod.bf.for <- gam(slope ~  s(perfor, k = 15) +
                   s(abs(Latitude), k = 15) +
                   s(log10(Lake.Area..m.2.), k = 15), 
                 data = bf.slope, 
                 method = 'REML')
summary(mod.bf.for)

#urban
mod.bf.dev <- gam(slope ~  s(perdev, k = 15) +
                   s(abs(Latitude), k = 15) +
                   s(log10(Lake.Area..m.2.), k = 15), 
                 data = bf.slope, 
                 method = 'REML')
summary(mod.bf.dev)


