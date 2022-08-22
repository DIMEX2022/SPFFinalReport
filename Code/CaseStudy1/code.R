
# CONTENTS                BEGINS AT LINE

# population data:            26   
# pollutant data:             190 
# temperature data:           394
# model buidlding:            589
# sensitivity analysis:       1258
# application:                1481
# results:                    1507


######################### PART 1: USED PACKAGES ######################### 

# import libraries
library(tidyverse)
library(raster)
library(sf)
library(maptools)
library(lubridate)    # to work with dates
library(EnvStats)     # for triangular distribution
library(truncnorm)    # to truncate Normal distributions from Burke et al. (2001)
library(lhs)          # for Latin Hypercube Sampling


######################### PART 2: PREPARATION OF POPULATION DATA ######################### 

##################################################
############ import and wrangle data #############
##################################################

# read population data
devon_data <- read.csv("~/Dropbox/Github/SPFFinalReport/Data/CaseStudy1/Devon_simulated_TU_keyworker_health.csv")

# add id column to Devon dataset and order columns
devon_data$id <- 1:length(devon_data$area)
devon_data <- dplyr::select(devon_data, id, everything())

# subset with relevant columns
population_data <- dplyr::select(devon_data, id, area, hid, pid, Sex, Age1, pwkstat, 
                                 smoke, punknown:ptransport, pid_tus)

# add rounded hours spent in each ME
population_data <- population_data %>% 
  mutate(rhome = ceiling(phome * 24 + pworkhome * 24),
         rindoor = ceiling(pwork * 24 + pschool * 24 + pshop * 24 + pservices * 24 +
                             pescort * 24),
         rleisure = ceiling(pleisure * 24),
         rtransport = ceiling(ptransport * 24))

# add total of rounded hours
population_data <- population_data %>% 
  mutate(rtotal = rowSums(.[20:23]))

# get all time use patterns
TUS <- population_data %>% 
  group_by(pid_tus, Sex, Age1, pwkstat, punknown, phome, pworkhome, pwork, pschool,
           pshop, pservices, pleisure, pescort,
           ptransport, rhome, rindoor, rtransport, rleisure, rtotal) %>% 
  count()

# subset of al complete time use patterns
TUS_complete <- subset(TUS, rtotal >= 24 & rhome >= 8)

# subset of all broken time use patterns
TUS_broken <- subset(TUS, !(rtotal >= 24 & rhome >= 8))


##### find out if all broken time use patterns can be resampled from same strata
# this is possible if there are more timeuse patterns in strata than are broken
# for the strata

# find number of all unique patterns for each strata
overall_timeuse <- population_data %>% 
  group_by(Sex, Age1, pwkstat) %>% 
  mutate(unique_types = n_distinct(pid_tus))

overall_timeuse <- overall_timeuse %>% 
  group_by(Sex, Age1, pwkstat) %>% 
  summarise(total_unique = mean(unique_types)) 

# find number of broken unique patterns for each strata
broken_timeuse <- subset(population_data, !(rtotal >= 24 & rhome >= 8)) %>% 
  group_by(Sex, Age1, pwkstat) %>% 
  mutate(unique_types = n_distinct(pid_tus))

broken_timeuse <- broken_timeuse %>% 
  group_by(Sex, Age1, pwkstat) %>% 
  summarise(broken_unique = mean(unique_types))

# compare number of all patterns to number of broken and see which strata cannot be
# resampled
compare_timeuse <- left_join(overall_timeuse, broken_timeuse) %>% 
  mutate(remaining_unique = total_unique - broken_unique)

# there are five individuals for which the time use pattern is broken and cannot be
# resampled

# save ids of those individuals
badid <- c(291063, 186008, 444492, 660272, 184296)

# exlude bad ids from dataset
pop_data <- subset(population_data, !(id %in% badid))

# split in two datasets: 1: subset with broken patterns, 2: subset with complete
# patterns
pop_data1 <- subset(pop_data, pid_tus %in% TUS_broken$pid_tus)
pop_data2 <- subset(pop_data, !(pid_tus %in% TUS_broken$pid_tus))

# sample new time use patterns for bad time use patterns
for (i in 1:nrow(pop_data1)){
  
  # subset of possible complete time use patterns of strata
  temp <- as.data.frame(subset(TUS_complete, Sex == pop_data1$Sex[i] & 
                                 Age1 == pop_data1$Age1[i] & 
                                 pwkstat == pop_data1$pwkstat[i]))
  
  # select random time use pattern
  pattern <- sample_n(temp, 1)
  
  # fill in new pattern
  pop_data1[i, 9:24] <- c(pattern$punknown, pattern$phome, pattern$pworkhome,
                          pattern$pwork, pattern$pschool, pattern$pshop,
                          pattern$pservices, pattern$pleisure, pattern$pescort,
                          pattern$ptransport, pattern$pid_tus,
                          pattern$rhome, pattern$rindoor, pattern$rtransport,
                          pattern$rleisure, pattern$rtotal)
}


# merge the two sets and organise, and duplicate pwkstat column for next steps
population_data <- bind_rows(pop_data1, pop_data2) %>% 
  mutate(work = pwkstat) %>% 
  dplyr::select(id, area, hid, pid, Sex, Age1, pwkstat, work, everything()) %>% 
  arrange(id)

##### now a column specifying if an individual works is created

# change levels of work column to 1 if works and 0 if not
levels(population_data$work) <- c(0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0)

# change column class to factor and adjust values so that 1 if works and 0 if not
population_data$work <- as.numeric(population_data$work)
population_data <- mutate(population_data, work = work-1)

# check if dataset tidy
summary(population_data)

# fill missing smoking status with 0
population_data[is.na(population_data)] <- 0

##################################################
############ add coordinates of area #############
##################################################

# import shapefile, converte to longitude/latitude coordinates and organise data
shape <- rgdal::readOGR(dsn = "Data/Middle_Layer_Super_Output_Areas__December_2011_
                        _Population_Weighted_Centroids-shp", 
                        layer = "Middle_Layer_Super_Output_Areas__December_2011_
                        _Population_Weighted_Centroids")
proj4string(shape)
shapeLL = spTransform(shape, CRS("+init=epsg:4326"))
converted_coordinates <- as.data.frame(shapeLL)
colnames(converted_coordinates) <- c("id", "msoa", "name", "long", "lat")

# get unique values for MSOAs from population dataset
devon_msoa <- unique(population_data$area)

# get corresponding coordinates in Devon
devon_msoa_coordinates <- subset(converted_coordinates, msoa %in% devon_msoa)

# create longitude and latitude column in population dataset and organise
population_data$Longitude <- NA
population_data$Latitude <- NA
population_data <- dplyr::select(population_data, id, area, Longitude, Latitude, hid,
                                 pid, Sex, Age1, pwkstat, work, everything())

# add coordinates to population_data
for (i in 1:nrow(population_data)) {
  population_data$Longitude[i] <- subset(devon_msoa_coordinates, msoa ==
                                           as.character(population_data$area[i]))$long
  population_data$Latitude[i] <- subset(devon_msoa_coordinates, msoa ==
                                          as.character(population_data$area[i]))$lat
}

# save complete dataset
save(population_data, file = "~/Dropbox/Github/SPFFinalReport/Data/CaseStudy1/population_data_location.RData")


######################### PART 3: PREPARATION OF POLLUTANT DATA ######################### 

##################################################
############ import and wrangle data #############
##################################################

# read in data
pm_full_brick <- brick("Data/final/pm2.5_UK_2019.grib")

# get coords for region just bigger than Devon and crop data
newextent <- c(-7, -1, 48, 53)
pm_brick <- crop(pm_full_brick, newextent)
pm_data <- as.data.frame(pm_brick, xy = TRUE)

### create tidy df

# collapse and name columns
pm_tidy <- pivot_longer(pm_data, starts_with("pm"), names_to = "tmp", 
                        values_to = "PM2.5")
colnames(pm_tidy) <- c("Longitude", "Latitude", "Tmp", "PM2.5")

# add dates
Dates <- seq(as.Date("2018/12/25"), as.Date("2019/12/31"), by = "day")
pm_tidy$Date <- rep(Dates, each = 8)

# add time column
TimeSteps <- c(0, 3, 6, 9, 12, 15, 18, 21)
pm_tidy$Time <- TimeSteps

# transform unit
pm_tidy <- mutate(pm_tidy, PM2.5 = PM2.5 * 1000000000)

# select and order relevant columns
pm_tidy <- dplyr::select(pm_tidy, Longitude, Latitude, Date, Time, PM2.5)


##################################################
############### space interpolation ##############
##################################################

# define function to interpolate space
# long_int and lat_int specify how fine interpolation
interpolate_space <- function(PM_data, long_int, lat_int) {
  
  # create vectors of unique date and time values to loop through
  dates <- unique(PM_data$Date)
  times <- unique(PM_data$Time)
  
  # initilaise final dataframe
  df <- data.frame()
  
  # looping through days
  for (i in 1:length(dates)) {
    
    # select rows that correspond to date
    df_by_date <- filter(PM_data, Date == dates[i])
    
    # looping through times
    for (j in 1:length(times)) {
      
      # select rows that correspond to time
      df_by_date_and_time <- filter(df_by_date, Time == times[j])
      
      # interpolate
      int <- akima::interp(x = df_by_date_and_time$Longitude, 
                           y = df_by_date_and_time$Latitude, 
                           z = df_by_date_and_time$PM2.5, 
                           nx = long_int, ny = lat_int)
      
      ### create dataframe
      
      # extract longitude and latitude from interpolated list object
      lon <- int[[1]]
      lat <- int[[2]]
      
      # start dataframe with interpolated PM values
      PM <- as.data.frame(int[[3]])
      
      # add longitude to dataframe
      PM$Longitude <- lon
      
      # transform columns into rows and add latitude
      PM <- pivot_longer(PM, starts_with("V"), names_to = "Latitude", 
                         values_to = "PM2.5")
      PM$Latitude <- lat
      
      # add date and time
      PM$Date <- dates[i]
      PM$Time <- times[j]
      
      # sort columns
      PM <- dplyr::select(PM, Longitude, Latitude, Date, Time, PM2.5)
      
      # merge to final dataframe
      df <- bind_rows(df, PM)
    }
  }
  # return final dataframe
  return(df)
}


##################################################
############### time interpolation ###############
##################################################

# define function to interpolate between 3 hours
interpolate_time <- function(PM_data) {
  
  ### create dataframe with missing hours
  
  # define vectors
  times_missing <- c(1, 2, 4, 5, 7, 8, 10, 11, 13, 14, 16, 17, 19, 20, 22, 23)
  dates <- unique(PM_data$Date)
  lons <- unique(PM_data$Longitude)
  lats <- unique(PM_data$Latitude)
  
  # create rows from all combinations of the above and rename columns
  missing_df <- expand.grid(lons, lats, dates, times_missing)
  colnames(missing_df) <- c("Longitude", "Latitude", "Date", "Time")
  
  # merge dataframe with PM measurements and dataframe with missing values 
  # and arrange
  total_df <- bind_rows(PM_data, missing_df)
  total_df <- arrange(total_df, Longitude, Latitude, Date, Time)
  
  # add id column to interpolate missing values
  total_df$id <- 1:length(total_df$Longitude)
  
  # transform into zoo object and interpolate
  total_zoo <- zoo::zoo(total_df$PM2.5, total_df$id)
  total_zoo <- imputeTS::na_interpolation(total_zoo, option = "linear")
  
  # create final dataframe
  tmp <- zoo::fortify.zoo(total_zoo)
  total_df$PM2.5 <- tmp$total_zoo
  total_df[, 1:5]
}


##################################################
################ interpolate data ################
##################################################

space_int_df <- interpolate_space(pm_tidy, 52, 37)
space_time_int_df <- interpolate_time(space_int_df)

# round values and select only for Devon
PM_data <- space_time_int_df %>% 
  mutate(Longitude = round(Longitude, 5), Latitude = round(Latitude, 5)) %>% 
  filter(Longitude < -2 & Longitude > -6 & Latitude < 51.5 & Latitude > 49.5)

# save data
save(PM_data, file = "~/Dropbox/Github/SPFFinalReport/Data/CaseStudy1/PM_2019_fine.RData")


##################################################
################### create plot ##################
##################################################

# This is a data set from the maptools package
data(wrld_simpl)

# Create a data.frame object for ggplot. ggplot requires a data frame.
mymap <- fortify(wrld_simpl)

ggplot() +
  geom_tile(data = PM_data[ which( PM_data$Date == as.Date("2019/05/01") &
                                     PM_data$Time == 12) , ], 
            aes(x = Longitude, y = Latitude, fill = PM2.5)) +
  geom_point(data = PM_data[ which( PM_data$Date == as.Date("2019/05/01") &
                                      PM_data$Time == 12) , ], 
             aes(x = Longitude, y = Latitude), pch = 20) + 
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id), 
           alpha = 0, fill = "white", color = grey(0)) +
  scale_fill_gradientn(colours = c("#9CFF9C", "#9CFF9C", "#31FF00", "#31CF00",
                                   "#FFFF00", "#FFCF00", "#FF9A00", "#FF6464",
                                   "#FF0000", "#990000"), 
                       values = c(0, 11/70, 23/70, 35/70, 41/70, 47/70, 53/70, 
                                  58/70, 64/70, 1), 
                       breaks = c(0, 11, 23, 35, 41, 47, 53, 58, 64), 
                       limits = c(0.0, 70), oob = scales::squish, 
                       name = "?g/m3") +
  scale_x_continuous(limits = c(-6, -1.95), expand = c(0, 0)) +
  scale_y_continuous(limits = c( 49.515, 51.5265), expand = c(0, 0)) +
  coord_equal() +
  guides(fill = guide_colorbar(barheight = 9)) +
  labs(title = "Ambient pollution field of PM2.5", subtitle = "01/05/2019, 12:00",  
       x = "Longitude", y = "Latitude") +
  theme(panel.grid = element_blank())


##################################################
############## create rollback data ##############
##################################################

# decrease pollution by 20%
PM_rollback_data <- PM_data %>% 
  mutate(PM2.5 = PM2.5 * 0.8)

# save data
save(PM_rollback_data, file = "~/Dropbox/Github/SPFFinalReport/Data/CaseStudy1/PM_rollback_2019_fine.RDATA")


######################### PART 4: PREPARATION OF TEMPERATURE DATA ######################### 

##################################################
############ import and wrangle data #############
##################################################

# read in data
temp_full_brick <- brick("Data/final/temperature_UK_2019.grib")

# get coords for region just bigger than Devon and crop data
newextent <- c(-7, -1, 48, 53)
temp_brick <- crop(temp_full_brick, newextent)
temp_data <- as.data.frame(temp_brick, xy = TRUE)

### create tidy df

# collapse and name columns
temp_tidy <- pivot_longer(temp_data, starts_with("temp"), names_to = "Tmp", 
                          values_to = "Temp_Kelvin")
colnames(temp_tidy) <- c("Longitude", "Latitude", "Tmp", "Temp_Kelvin")

# add dates
Dates <- seq(as.Date("2018/12/25"), as.Date("2019/12/31"), by = "day")
temp_tidy$Date <- rep(Dates, each = 8)

# add time column
TimeSteps <- c(0, 3, 6, 9, 12, 15, 18, 21)
temp_tidy$Time <- TimeSteps

# transform unit
temp_tidy <- mutate(temp_tidy, Temp_Celsius = Temp_Kelvin - 273.15)

# select and order relevant columns
temp_tidy <- dplyr::select(temp_tidy, Longitude, Latitude, Date, Time, Temp_Celsius)


##################################################
############### space interpolation ##############
##################################################

# define function to interpolate space
# long_int and lat_int specify how fine interpolation
interpolate_space <- function(temp_data, long_int, lat_int) {
  
  # create vectors of unique date and time values to loop through
  dates <- unique(temp_data$Date)
  times <- unique(temp_data$Time)
  
  # initilaise final dataframe
  df <- data.frame()
  
  # looping through days
  for (i in 1:length(dates)) {
    
    # select rows that correspond to date
    df_by_date <- filter(temp_data, Date == dates[i])
    
    # looping through times
    for (j in 1:length(times)) {
      
      # select rows that correspond to time
      df_by_date_and_time <- filter(df_by_date, Time == times[j])
      
      # interpolate
      int <- akima::interp(x = df_by_date_and_time$Longitude, 
                           y = df_by_date_and_time$Latitude, 
                           z = df_by_date_and_time$Temp_Celsius,
                           nx = long_int, ny = lat_int)
      
      ### create dataframe
      
      # extract longitude and latitude from interpolated list object
      lon <- int[[1]]
      lat <- int[[2]]
      
      # start dataframe with interpolated temp values
      temp <- as.data.frame(int[[3]])
      
      # add longitude to dataframe
      temp$Longitude <- lon
      
      # transform columns into rows
      temp <- pivot_longer(temp, starts_with("V"), names_to = "Latitude", 
                           values_to = "Temp_Celsius")
      temp$Latitude <- lat
      
      # add date and time
      temp$Date <- dates[i]
      temp$Time <- times[j]
      
      # sort columns
      temp <- dplyr::select(temp, Longitude, Latitude, Date, Time, Temp_Celsius)
      
      # merge to final dataframe
      df <- bind_rows(df, temp)
    }
  }
  # return final dataframe
  return(df)
}


##################################################
############### time interpolation ###############
##################################################

# define function to interpolate between 3 hours
interpolate_time <- function(temp_data) {
  
  ### create dataframe with missing hours
  
  # define vectors
  times_missing <- c(1, 2, 4, 5, 7, 8, 10, 11, 13, 14, 16, 17, 19, 20, 22, 23)
  dates <- unique(temp_data$Date)
  lons <- unique(temp_data$Longitude)
  lats <- unique(temp_data$Latitude)
  
  # create rows from all combinations of the above and rename columns
  missing_df <- expand.grid(lons, lats, dates, times_missing)
  colnames(missing_df) <- c("Longitude", "Latitude", "Date", "Time")
  
  # merge dataframe with temp measurements and dataframe with missing values 
  # and arrange
  total_df <- bind_rows(temp_data, missing_df)
  total_df <- arrange(total_df, Longitude, Latitude, Date, Time)
  
  # add id column to interpolate missing values
  total_df$id <- 1:length(total_df$Longitude)
  
  # transform into zoo object and interpolate
  total_zoo <- zoo::zoo(total_df$Temp_Celsius, total_df$id)
  total_zoo <- imputeTS::na_interpolation(total_zoo, option = "linear")
  
  # create final dataframe
  tmp <- zoo::fortify.zoo(total_zoo)
  total_df$Temp_Celsius <- tmp$total_zoo
  total_df[, 1:5]
}


##################################################
################ interpolate data ################
##################################################

space_int_df <- interpolate_space(temp_tidy, 52, 37)
space_time_int_df <- interpolate_time(space_int_df)

# round values and select only for Devon
temp_data <- space_time_int_df %>% 
  mutate(Longitude = round(Longitude, 5), Latitude = round(Latitude, 5)) %>% 
  filter(Longitude < -2 & Longitude > -6 & Latitude < 51.5 & Latitude > 49.5)

# find daily averages
temp_avg_Devon_data <- temp_data %>% 
  group_by(Longitude, Latitude, Date) %>% 
  summarise(Average = mean(Temp_Celsius))

# save to data file
save(temp_avg_Devon_data, file = "~/Dropbox/Github/SPFFinalReport/Data/CaseStudy1/temp_avg_2019_Devon_fine.RData")


##################################################
################## create plot ###################
##################################################

# This is a data set from the maptools package
data(wrld_simpl)

# Create a data.frame object for ggplot. ggplot requires a data frame.
mymap <- fortify(wrld_simpl)

# plot
ggplot() +
  geom_tile(data = temp_avg_Devon_data[ which( temp_avg_Devon_data$Date == 
                                                 as.Date("2019/02/03")) , ], 
            aes(x = Longitude, y = Latitude, fill = Average)) +
  geom_point(data = PM_data[ which( PM_data$Date == as.Date("2019/02/03") &
                                      PM_data$Time == 12) , ], 
             aes(x = Longitude, y = Latitude), pch = 20) + 
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id), 
           alpha = 0, fill = "white", color = grey(0)) +
  scale_fill_gradientn(colours = c("blue", "green", "yellow", "orange", "red"),
                       breaks = c(0, 5, 10, 15, 20), limits = c(0.0, 25), 
                       oob = scales::squish, name = "?C") +
  scale_x_continuous(limits = c(-6, -1.95), expand = c(0, 0)) +
  scale_y_continuous(limits = c( 49.5, 51.53), expand = c(0, 0)) +
  coord_equal() +
  guides(fill = guide_colorbar(barheight = 9)) +
  labs(title = "Spatial field of average daily temperature", 
       subtitle = "03/02/2019",
       x = "Longitude", y = "Latitude") +
  theme(panel.grid = element_blank())



######################### PART 5: MODEL BUILDING ######################### 

##################################################
################## import data ###################
##################################################

# read population data
load("~/Dropbox/Github/SPFFinalReport/Data/CaseStudy1/population_data_location.RDATA")

# load particulate matter data
load("~/Dropbox/Github/SPFFinalReport/Data/CaseStudy1/PM_2019_fine.RDATA")
load("~/Dropbox/Github/SPFFinalReport/Data/CaseStudy1/PM_rollback_2019_fine.RDATA")

# load temperature data
load("~/Dropbox/Github/SPFFinalReport/Data/CaseStudy1/temp_avg_2019_Devon_fine.RDATA")

##################################################
############### match coordinates ################
##################################################

##### match coordinates to of population data to PM data

# get coordinates of PM data
long_pm <- unique(PM_data$Longitude)
lat_pm <- unique(PM_data$Latitude)

# get coordinates of population dataa
long_pop <- unique(population_data$Longitude)
lat_pop <- unique(population_data$Latitude)

# create dataframe to match coords
match_long <- data.frame(long_pop)
match_lat <- data.frame(lat_pop)

# match longitude (closest distance) and fill in dataframe
for (i in 1:nrow(match_long)) {
  
  dist <- sqrt((match_long[i, 1] - long_pm)^2)
  
  match_long[i, 2] <- long_pm[match(min(dist), dist)]
}

# match latitude (closest distance) and fill in dataframe
for (i in 1:nrow(match_lat)) {
  
  dist <- sqrt((match_lat[i, 1] - lat_pm)^2)
  
  match_lat[i, 2] <- lat_pm[match(min(dist), dist)]
}

# change column names
colnames(match_long)[2] <- "long_match"
colnames(match_lat)[2] <- "lat_match"

# add column in population data for coords match
population_data$Longitude_match <- NA
population_data$Latitude_match <- NA

# match coordinates of PM data to population_data
for (i in 1:nrow(population_data)) {
  population_data$Longitude_match[i] <- 
    subset(match_long, long_pop == population_data$Longitude[i])$long_match
  population_data$Latitude_match[i] <- 
    subset(match_lat, lat_pop == population_data$Latitude[i])$lat_match
}

# arrange columns and save data
population_data <- dplyr::select(population_data, id, area, Longitude, Latitude,
                                 Longitude_match, Latitude_match, hid, pid, Sex,
                                 Age1, pwkstat, work, everything())
save(population_data, file = "~/Dropbox/Github/SPFFinalReport/Data/CaseStudy1/population_data_location_matched.RData")


##################################################
############# open microenvironments #############
##################################################

# define function to calculate local concentration of microenvironment outdoor
calculate_outdoor <- function(lon, lat, date, time) {
  
  # parameters (Zidek et al. (2003))
  a <- 0
  b <- 1
  
  # extract ambient concentration
  tmp <- subset(PM_data, Longitude == lon & Latitude == lat & 
                  Date == as.Date(date) & Time == time)
  AmbCon <- tmp$PM2.5
  
  # calculate local concentration
  a + b * AmbCon
}


##################################################


# define function to calculate local concentration of microenvironment indoor
calculate_indoor <- function(lon, lat, date, time) {
  
  # parameters (Normal from Burke et al. (2001) truncated)
  a <- rtruncnorm(n = 1, a = 0, mean = 6.467, sd = 2.1)
  b <- rtruncnorm(n = 1, a = 0, mean = 0.507, sd = 0.11)
  
  # extract ambient concentration
  tmp <- subset(PM_data, Longitude == lon & Latitude == lat & 
                  Date == as.Date(date) & Time == time)
  AmbCon <- tmp$PM2.5
  
  # calculate local concentration (Normal from Burke et al. (2001) truncated)
  rtruncnorm(n = 1, a = 0, mean = a + b * AmbCon, sd = 3.467)
}


##################################################

# define function to calculate local concentration of microenvironment car
calculate_transport <- function(lon, lat, date, time) {
  
  # parameters (Normal from Burke et al. (2001) truncated)
  a <- rtruncnorm(n = 1, a = 0, mean = 33, sd = 7.2)
  b <- rtruncnorm(n = 1, a = 0, mean = 0.26, sd = 0.14)
  
  # extract ambient concentration
  tmp <- subset(PM_data, Longitude == lon & Latitude == lat & 
                  Date == as.Date(date) & Time == time)
  AmbCon <- tmp$PM2.5
  
  # calculate local concentration (Normal from Burke et al. (2001) truncated)
  rtruncnorm(n = 1, a = 0, mean = a + b * AmbCon, sd = 12)
}


##################################################
############ closed microenvironments ############
##################################################

##### find region for air exchange rate (Murray and Burmaster (1995))

# calculate annual heating degree days for Devon
temp <- temp_avg_Devon_data %>% 
  mutate(Average = (Average*9/5)+32) %>% 
  group_by(Date) %>% 
  summarise(Average = mean(Average)) %>% 
  filter(Date >= as.Date("2019-01-01")) %>% 
  filter(Average < 65) %>% 
  mutate(Diff = 65 - Average)

sum(temp$Diff) # 4170 -> use region 3

# calculate annual heating degree days for Exeter
temp <- temp_avg_Devon_data %>% 
  mutate(Average = (Average*9/5)+32) %>% 
  group_by(Date) %>% 
  filter(Latitude == 50.72917 & Longitude == -3.55882) %>% 
  filter(Date >= as.Date("2019-01-01")) %>% 
  filter(Average < 65) %>% 
  mutate(Diff = 65 - Average)

sum(temp$Diff) # 4618 -> use region 3


##### calculate average consumption of cigarettes per day
# (ONS, Adult Smoking Habits in GB (2019))

### MALE
male_age_1 <- 4.0   # age 16-24
male_age_2 <- 5.1   # age 25-34
male_age_3 <- 10.9  # age 35-49
male_age_4 <- 12.4  # age 50-59
male_age_5 <- 12.9  # age 60+

# adjust to model age groups

# under 19
male_age_1

# 19 - 29
(5 * male_age_1 + 5 * male_age_2)/10

# 30 - 44
(5 * male_age_2 + 10 * male_age_3)/15

# 45 - 59
(5 * male_age_3 + 10 * male_age_4)/15

# 60 - 74
male_age_5

# over 74
male_age_5


### FEMALE
female_age_1 <- 5     # age 16-24
female_age_2 <- 8.8   # age 25-34
female_age_3 <- 8.1   # age 35-49 
female_age_4 <- 11.7  # age 50-59
female_age_5 <- 10.8  # age 60+

# adjust to model age groups

# under 19
female_age_1

# 19 - 29
(5 * female_age_1 + 5 * female_age_2)/10

# 30 - 44
(5 * female_age_2 + 10 * female_age_3)/15

# 45 - 59
(5 * female_age_3 + 10 * female_age_4)/15

# 60 - 74
female_age_5

# over 74
female_age_5


##### define function to calculate local concentration of microenvironment home
calculate_home <- function(lon, lat, date, time, housetype, smoker, sex, 
                           age, Cbefore) {
  
  # penetration factor (?zkaynak et al. (1996))
  Fp <- rnorm(n = 1, mean = 1, sd = 0.055)
  
  # deposition rate (?zkaynak et al. (1996))
  Fd <- rnorm(n = 1, mean = 0.39, sd = 0.0825)
  
  # emission generating source (?zkaynak et al. (1996))
  SCigarette <- rnorm(n = 1, mean = 13.8, sd = 1.775)
  SCooking <- rnorm(n = 1, mean = 1.7, sd = 0.325)
  SOther <- rnorm(n = 1, mean = 1.1, sd = 0.525)
  
  # parameter for cooking AND emitting PM2.5
  Cook_and_pm <- 0.1
  
  # define variable if individual is cooking AND cooking emits PM2.5 
  cooking <- sample(0:1, 1, prob = c(1 - Cook_and_pm, Cook_and_pm))
  
  # calculate total emissions by source (ONS, Adult Smoking Habits in GB (2019))
  # assumption: no second hand smoke
  # for smokers
  S <- if (smoker == 1) {
    # for males
    if (sex == 1) {
      # aged 16 to 24
      if (age == 1) {
        SSmoking <- rpois(1, 4/15) * SCigarette
        S <- SSmoking + cooking * SCooking + SOther
        # aged 25 to 34
      } else if (age == 2) {
        SSmoking <- rpois(1, 4.55/15) * SCigarette
        S <- SSmoking + cooking * SCooking + SOther
        # aged 35 to 49
      } else if (age == 3) {
        SSmoking <- rpois(1, 8.97/15) * SCigarette
        S <- SSmoking + cooking * SCooking + SOther
        # aged 50 to 59 
      } else if (age == 4) {
        SSmoking <- rpois(1, 11.9/15) * SCigarette
        S <- SSmoking + cooking * SCooking + SOther
        # aged 60 or older
      } else if (age == 5 | age == 6) {
        SSmoking <- rpois(1, 12.9/15) * SCigarette
        S <- SSmoking + cooking * SCooking + SOther
      } 
      # for females
    } else if (sex == 0) {
      # aged 16 to 24
      if (age == 1) {
        SSmoking <- rpois(1, 5/15) * SCigarette
        S <- SSmoking + cooking * SCooking + SOther
        # aged 25 to 34
      } else if (age == 2) {
        SSmoking <- rpois(1, 6.9/15) * SCigarette
        S <- SSmoking + cooking * SCooking + SOther
        # aged 35 to 49
      } else if (age == 3) {
        SSmoking <- rpois(1, 8.33/15) * SCigarette
        S <- SSmoking + cooking * SCooking + SOther
        # aged 50 to 59 
      } else if (age == 4) {
        SSmoking <- rpois(1, 10.5/15) * SCigarette
        S <- SSmoking + cooking * SCooking + SOther
        # aged 60 or older
      } else if (age == 5 | age == 6) {
        SSmoking <- rpois(1, 10.8/15) * SCigarette
        S <- SSmoking + cooking * SCooking + SOther
      }  
    }
    # for non-smokers
  } else if (smoker == 0) {
    S <- cooking * SCooking + SOther
  }
  
  # seasons
  Winter <- c(12, 1, 2)
  Spring <- c(3, 4, 5)
  Summer <- c(6, 7, 8)
  Autumn <- c(9, 10, 11)
  
  # time
  Night <- c(22:23, 0:5)
  Day <- c(6:21)
  
  # calculate air exchange rate (Murray and Burmaster (1995) Region 3)
  v <- if (month(as.Date(date)) %in% Winter) {
    rlnorm(n = 1, meanlog = -0.958, sdlog = 0.589)
  } else if (month(as.Date(date)) %in% Spring) { 
    rlnorm(n = 1, meanlog = -0.802, sdlog = 0.782)
  } else if (month(as.Date(date)) %in% Summer) {
    rlnorm(n = 1, meanlog = -0.588, sdlog = 0.612)
  } else if (month(as.Date(date)) %in% Autumn) {
    rlnorm(n = 1, meanlog = -0.787, sdlog = 0.453)
  }
  
  # calculate volume of home (zoopla + onaverage.co.uk)
  V <- if (housetype == "detached") {
    rtri(1, min = 81, max = 214, mode = 159) * runif(1, min = 2.1, max = 2.6)
  } else if (housetype == "semi-detached") { 
    rtri(1, min = 56, max = 204, mode = 84)  * runif(1, min = 2.1, max = 2.6)
  } else if (housetype == "terrace") {
    rtri(1, min = 33, max = 155, mode = 59)  * runif(1, min = 2.1, max = 2.6)
  } else if (housetype == "flat") {
    rtri(1, min = 34, max = 106, mode = 41)  * runif(1, min = 2.1, max = 2.6)
  }
  
  # extract ambient concentration
  tmp <- subset(PM_data, Longitude == lon & Latitude == lat & 
                  Date == as.Date(date) & Time == time)
  Cout <- tmp$PM2.5
  
  # concentration added
  Cadd <-  S / V + v * Fp * Cout
  
  # calculate indoor concentration (Zidek et al. (2007))
  # Cbefore * (1 - v - Fd) + Cadd
  (Cadd / (v + Fd)) + (Cbefore - (Cadd / (v + Fd))) * exp(-(v + Fd) * 1)
}


##################################################
############### calibrate outdoors ###############
##################################################

# on average individuals spend 85-90% of their time indoors 
# (European Comission, 2003)

# define threshold for cold and warm days
median(temp_avg_Devon_data$Average)
hist(temp_avg_Devon_data$Average, main = "Median average temperature in Devon", 
     xlab = "Temperature (in ?C)")
abline(v = 14, col = "red", lwd = 3)

# find quantile of 14?C
quantile(temp_avg_Devon_data$Average, probs = 0.7)

# balance time of leisure to MEs indoor not home and outdoors
total_hours <- sum(population_data$rtotal)
indoor_hours <- sum(population_data$rhome, population_data$rindoor)
transport_hours <- sum(population_data$rtransport)
leisure_hours <- sum(population_data$rleisure)

indoor_hours/total_hours     # 85.4%
transport_hours/total_hours  #  9.0%
leisure_hours/total_hours    #  5.6%

# proportion of leisure time spent indoors on cold day
a <- 0.7

# proportion of leisure time spent indoors on warm day
b <- 0.2

85.4 + 5.6 * a * 0.7 + 5.6 * b * (1-0.7) # 88.5%


##################################################
############ activity pattern sampler ############
##################################################

# define function to sample activity pattern
sample_activity <- function(df, i, date) {
  
  ### divide leisure into outdoor and indoor depending on temperature
  
  # get daily average temperature
  temp <- subset(temp_avg_Devon_data, Longitude == df$Longitude_match[i] & 
                   Latitude == df$Latitude_match[i] & Date == as.Date(date))$Average
  
  # define if warm or cold day
  if (temp > 14) {
    warm_day <- 1
  } else {
    warm_day <- 0
  }
  
  # split leisure into indoor and outdoor (assumptions that overall 
  # time spent outdoors of popultion 85-90% (European Comission (2003)))
  
  # parameter: proportion of leisure time spent indoors on warm day
  prop_leis_warm <- 0.2
  
  # parameter: proportion of leisure time spent indoors on cold day
  prop_leis_cold <- 0.7
  
  # CASE 1: it is a warm day
  if (warm_day == 1) {
    rleisure_outdoor <- df$rleisure[i] * (1 - prop_leis_warm)
    rleisure_indoor <- df$rleisure[i] * prop_leis_warm
    
    # CASE 2: it is a cold day
  } else {
    rleisure_outdoor <- df$rleisure[i] * (1 - prop_leis_cold)
    rleisure_indoor <- df$rleisure[i] * prop_leis_cold
  }
  
  
  # set up vector with MEs to sample from 
  MEs <- c(rep("home", ceiling(df$rhome[i] - 8)), # subtract 8 as we allocate 8
           # hours to home independent from
           # sampling
           rep("indoor", ceiling(df$rindoor[i] + rleisure_indoor)),
           rep("outdoor", ceiling(rleisure_outdoor)),
           rep("transport", ceiling(df$rtransport[i])))
  
  # get hours spent at work and total hours without work
  work <- (df$pwork[i] + df$pworkhome[i]) * 24
  total <- df$rhome[i] + df$rindoor[i] - work + df$rleisure[i] + df$rtransport[i]
  
  # set up vector with MEs to sample from for weekend 
  # (hours spent at work are distributed to other MEs)
  MEs_weekend <- c(rep("home", ceiling(df$rhome[i] - 8 + df$rhome[i]/total * work)),
                   # subtract 8 as we allocate 8 hours to home independent from
                   # sampling
                   rep("indoor", ceiling(df$rindoor[i] + rleisure_indoor - work +
                                           (df$rindoor[i] + rleisure_indoor)/
                                           total * work)),
                   rep("outdoor", ceiling(rleisure_outdoor + rleisure_outdoor/
                                            total * work)),
                   rep("transport", ceiling(df$rtransport[i] +
                                              df$rtransport[i]/total * work)))
  
  
  # indicator variable to decide if work on specific day 
  # (Labour Force Survey (2020))
  # don't work == 0, work == 1
  work_day <- 
    
    # CASE 1: individual employed
    if (df$work[i] == 1) {
      if ((weekdays(as.Date(date), TRUE) == "Mon")) {
        sample(0:1, 1, prob = c(0.131, 0.869))
      } else if ((weekdays(as.Date(date), TRUE) == "Tue")) {
        sample(0:1, 1, prob = c(0.106, 0.894))
      } else if ((weekdays(as.Date(date), TRUE) == "Wed")) {
        sample(0:1, 1, prob = c(0.109, 0.891))
      } else if ((weekdays(as.Date(date), TRUE) == "Thu")) {
        sample(0:1, 1, prob = c(0.116, 0.884))
      } else if ((weekdays(as.Date(date), TRUE) == "Fri")) {
        sample(0:1, 1, prob = c(0.157, 0.843))
      } else if ((weekdays(as.Date(date), TRUE) == "Sat")) {
        sample(0:1, 1, prob = c(0.758, 0.242))
      } else if ((weekdays(as.Date(date), TRUE) == "Sun")) {
        sample(0:1, 1, prob = c(0.823, 0.177))
      }
      
      # CASE 2: individual unemployed
    } else {
      0
    }
  
  # indicator variable to decide if work at day (1) or night (0) 
  # (Labour Force Survey (2020))
  nightshift <- sample(0:1, 1, prob = c(0.895, 0.105))
  
  # CASE 1: individual works during the day
  if (work_day == 1 && nightshift == 0) {
    pattern <- rep("home", 6)
    pattern <- append(pattern, sample(MEs, size = 16), 6)
    pattern <- append(pattern, rep("home", 2), 22)
    pattern
    
    # CASE 2: individual works during the night
  } else if (work_day == 1 && nightshift == 1) {
    pattern <- sample(MEs, size = 6)
    pattern <- append(pattern, rep("home", 8), 6)
    pattern <- append(pattern, sample(MEs, size = 10), 14)
    pattern
    
    # CASE 3: individual does not work    
  } else if (work_day == 0) {
    pattern <- rep("home", 6)
    pattern <- append(pattern, sample(MEs_weekend, size = 16), 6)
    pattern <- append(pattern, rep("home", 2), 22)
    pattern
  }
}


##################################################
################# stratification #################
##################################################

# define function for stratification
stratify <- function(Data, age, sex, Work, Smoke) {
  subset(Data, Age1 == age & Sex == sex & work == Work & smoke == Smoke)
}


##################################################
############### assign housingtype ###############
##################################################

assign_housingtype <- function(data){
  
  # combine area and house id to create unique id 
  # (o/w same household could get assigned different housing types)
  tmp_data <- data %>% 
    mutate(areahid = paste(area, hid, sep = "")) 
  
  # create housetype column
  housetype_df <- data.frame(areahid = unique(tmp_data$areahid),
                             housetype = NA)
  
  # initialise vector to fill with loop
  housetype <- NULL
  
  # (Office of National Statistics, Census data (2011))
  for (i in 1:length(housetype_df$areahid)) {
    housetype[i] <- sample(c("detached", "semi-detached", "terrace", "flat"), 
                           prob = c(0.3000, 0.2746, 0.2340, 0.1914), size = 1)
  }
  
  housetype_df$housetype <- housetype
  
  # add housetype to tmp_data
  tmp_data <- left_join(tmp_data, housetype_df, by = "areahid")
  
  # order columns
  tmp_data <- dplyr::select(tmp_data, id, area, Longitude, Latitude,
                            Longitude_match, Latitude_match, hid, pid, Sex, Age1,
                            pwkstat, work, housetype, everything())
  tmp_data <- dplyr::select(tmp_data, -areahid)
}


##################################################
############### calculate exposure ###############
##################################################

calculate_exposure <- function(Pop_data, age, sex, Work, Smoke, Pol_data, rep,
                               Start, End) {
  
  ##### create dataframe with individuals
  strata <- stratify(Pop_data, age, sex, Work, Smoke)
  individuals <- sample_n(strata, rep)
  
  
  ##### create dataframe with exposure
  days <- seq(as.Date(Start), as.Date(End), by = "days")
  
  exposure <- expand.grid(Date = days, Time = 0:23) %>% 
    arrange(Date, Time)
  
  ##### create data frame for home concentrations
  # begins 3 days earlier than timeframe to let concentration indoor get settled
  home_concentration <- expand.grid(
    Date = unique(subset(Pol_data, Date <= as.Date(End) & 
                           Date > (as.Date(Start) - 4))$Date), 
    Time = unique(Pol_data$Time)
  ) %>% 
    arrange(Date, Time)
  
  
  ##### fill in exposure for individuals
  
  # loop over individuals
  for (k in 1:rep) {
    print(paste("Calculating individual", k, sep = " "))
    # initialise / empty vector
    test_activitypattern <- NULL
    
    # simulate activity pattern
    for (h in 1:length(days)) {
      test_activitypattern <- append(test_activitypattern,
                                     sample_activity(individuals, k,
                                                     as.Date(days[h])))
    }
    
    # number of columns for indices
    j <- ncol(exposure)
    l <- ncol(home_concentration)
    
    print("Activity done")
    # Calculate indoor concentrations for the entire time frame of the pollution
    # dataset. Has to be done in one piece as it is a mass balance equation and
    # concentrations from the hour before are needed to calculate current
    # concentrations. Startpoint for the the first hour is median indoor 
    # concentration from Wallace et al. (1993).
    for (i in 1:length(home_concentration$Date)) {
      
      # find concentration of hour before
      Cbefore <- if (i == 1) {
        # concentration starting point (Wallace et al. (1993))
        rnorm(n = 1, mean = 26, sd = 2)
      } else { 
        home_concentration[i-1, l+1]
      }
      
      # calculate concentration of current hour
      home_concentration[i, l+1] <- calculate_home(individuals$Longitude_match[k],
                                                   individuals$Latitude_match[k],
                                                   home_concentration$Date[i],
                                                   home_concentration$Time[i],
                                                   individuals$housetype[k],
                                                   individuals$smoke[k],
                                                   individuals$Sex[k],
                                                   individuals$Age1[k], Cbefore)
    }
    
    # rename added column
    colnames(home_concentration)[l+1] <- paste("Home", k, sep = "")
    
    print("Home concentration done")
    
    # loop over hours
    # calculate exposure for MEs transport, indoor, and outdoor and take
    #concentration for home from home_concentration dataframe
    for (i in 1:length(test_activitypattern)) {
      
      if (test_activitypattern[i] == "transport") {
        exposure[i, j+1] <- test_activitypattern[i]
        exposure[i, j+2] <- calculate_transport(individuals$Longitude_match[k],
                                                individuals$Latitude_match[k],
                                                exposure$Date[i], exposure$Time[i])
      } else if (test_activitypattern[i] == "home") {
        exposure[i, j+1] <- test_activitypattern[i]
        exposure[i, j+2] <- subset(home_concentration, Date == exposure$Date[i] &
                                     Time == exposure$Time[i])[, l+1]
      } else if (test_activitypattern[i] == "indoor") {
        exposure[i, j+1] <- test_activitypattern[i]
        exposure[i, j+2] <- calculate_indoor(individuals$Longitude_match[k],
                                             individuals$Latitude_match[k],
                                             exposure$Date[i], exposure$Time[i])
      } else if (test_activitypattern[i] == "outdoor") {
        exposure[i, j+1] <- test_activitypattern[i]
        exposure[i, j+2] <- calculate_outdoor(individuals$Longitude_match[k],
                                              individuals$Latitude_match[k],
                                              exposure$Date[i], exposure$Time[i])
      }
      
      
    }
    # rename added column
    colnames(exposure)[j+1] <- paste("Activity", k, sep = "")
    colnames(exposure)[j+2] <- paste("Exposure", k, sep = "")
    
    print("Exposure done")
  }
  
  # return dataframe
  returnlist <- list(exposure, individuals, home_concentration)
  return(returnlist)
}


######################### PART 6: SENSITIVITY ANALYSIS ######################### 

##################################################
################ create hypercube ################
##################################################

########## space filling design: maximin latin hypercube

# create maximin latin hypercube
HypCube <- maximinLHS(5, 3)

# plot hypercube
plot(HypCube[, 1:2], pch = 21, xlim = c(0,1), ylim = c(0,1), col = "#3B3B3BFF",
     bg = "#CD534CFF", main = "Latin hypercube sampling", xlab = "Variable 1", 
     ylab = "Variable 2")
abline(v = seq(0.2, 0.8, by = 0.2), col = "gray50", lty = 3)
abline(h = seq(0.2, 0.8, by = 0.2), col = "gray50", lty = 3)


##################################################
################### run model ####################
##################################################

# assign housting type
Model_data <- assign_housingtype(population_data)

# pick individual for sensitivity analysis
individual <- subset(Model_data, id == 16602)

# run model and save with all five sets of parameters
# change parameters of function in between model runs
exposure1_df <- calculate_exposure(individual, 3, 0, 0, 0, PM_data, 1, 
                                   "2019-01-01", "2019-12-31")
exposure2_df <- calculate_exposure(individual, 3, 0, 0, 0, PM_data, 1, 
                                   "2019-01-01", "2019-12-31")
exposure3_df <- calculate_exposure(individual, 3, 0, 0, 0, PM_data, 1, 
                                   "2019-01-01", "2019-12-31")
exposure4_df <- calculate_exposure(individual, 3, 0, 0, 0, PM_data, 1, 
                                   "2019-01-01", "2019-12-31")
exposure5_df <- calculate_exposure(individual, 3, 0, 0, 0, PM_data, 1, 
                                   "2019-01-01", "2019-12-31")

exposure_list <- list(exposure1_df, exposure2_df, exposure3_df, exposure4_df,
                      exposure5_df, HypCube)

# extract exposure from each run
exp1 <- exposure1_df[[1]]
exp2 <- exposure2_df[[1]]
exp3 <- exposure3_df[[1]]
exp4 <- exposure4_df[[1]]
exp5 <- exposure5_df[[1]]


##################################################
############# format for regression ##############
##################################################

# run 1
exp1a <- dplyr::select(exp1, Date, Time, contains("Act"))
exp1a <- pivot_longer(exp1a, cols = starts_with("Act"), names_to = "Individual",
                      values_to = "Exposure") 
exp1b <- dplyr::select(exp1, Date, Time, contains("Exp"))
exp1b <- pivot_longer(exp1b, cols = starts_with("Exp"), names_to = "Individual",
                      values_to = "Exposure")
exp1full <- data.frame(exp1a$Date, exp1a$Time, exp1a$Individual, exp1a$Exposure,
                       exp1b$Exposure)
colnames(exp1full) <- c("Date", "Time", "Individual", "ME", "Exposure")
exp1full$id <- 1:8760
exp1full$Cook <- HypCube[1, 1]
exp1full$Warm <- HypCube[1, 2]
exp1full$Cold <- HypCube[1, 3]
exp1full$Run <- as.character(1)

# run 2
exp2a <- dplyr::select(exp2, Date, Time, contains("Act"))
exp2a <- pivot_longer(exp2a, cols = starts_with("Act"), names_to = "Individual",
                      values_to = "Exposure") 
exp2b <- dplyr::select(exp2, Date, Time, contains("Exp"))
exp2b <- pivot_longer(exp2b, cols = starts_with("Exp"), names_to = "Individual",
                      values_to = "Exposure")
exp2full <- data.frame(exp2a$Date, exp2a$Time, exp2a$Individual, exp2a$Exposure,
                       exp2b$Exposure)
colnames(exp2full) <- c("Date", "Time", "Individual", "ME", "Exposure")
exp2full$id <- 1:8760
exp2full$Cook <- HypCube[2, 1]
exp2full$Warm <- HypCube[2, 2]
exp2full$Cold <- HypCube[2, 3]
exp2full$Run <- as.character(2)

# run 3
exp3a <- dplyr::select(exp3, Date, Time, contains("Act"))
exp3a <- pivot_longer(exp3a, cols = starts_with("Act"), names_to = "Individual",
                      values_to = "Exposure") 
exp3b <- dplyr::select(exp3, Date, Time, contains("Exp"))
exp3b <- pivot_longer(exp3b, cols = starts_with("Exp"), names_to = "Individual",
                      values_to = "Exposure")
exp3full <- data.frame(exp3a$Date, exp3a$Time, exp3a$Individual, exp3a$Exposure,
                       exp3b$Exposure)
colnames(exp3full) <- c("Date", "Time", "Individual", "ME", "Exposure")
exp3full$id <- 1:8760
exp3full$Cook <- HypCube[3, 1]
exp3full$Warm <- HypCube[3, 2]
exp3full$Cold <- HypCube[3, 3]
exp3full$Run <- as.character(3)

# run 4
exp4a <- dplyr::select(exp4, Date, Time, contains("Act"))
exp4a <- pivot_longer(exp4a, cols = starts_with("Act"), names_to = "Individual",
                      values_to = "Exposure") 
exp4b <- dplyr::select(exp4, Date, Time, contains("Exp"))
exp4b <- pivot_longer(exp4b, cols = starts_with("Exp"), names_to = "Individual",
                      values_to = "Exposure")
exp4full <- data.frame(exp4a$Date, exp4a$Time, exp4a$Individual, exp4a$Exposure,
                       exp4b$Exposure)
colnames(exp4full) <- c("Date", "Time", "Individual", "ME", "Exposure")
exp4full$id <- 1:8760
exp4full$Cook <- HypCube[4, 1]
exp4full$Warm <- HypCube[4, 2]
exp4full$Cold <- HypCube[4, 3]
exp4full$Run <- as.character(4)

# run 5
exp5a <- dplyr::select(exp5, Date, Time, contains("Act"))
exp5a <- pivot_longer(exp5a, cols = starts_with("Act"), names_to = "Individual",
                      values_to = "Exposure") 
exp5b <- dplyr::select(exp5, Date, Time, contains("Exp"))
exp5b <- pivot_longer(exp5b, cols = starts_with("Exp"), names_to = "Individual",
                      values_to = "Exposure")
exp5full <- data.frame(exp5a$Date, exp5a$Time, exp5a$Individual, exp5a$Exposure,
                       exp5b$Exposure)
colnames(exp5full) <- c("Date", "Time", "Individual", "ME", "Exposure")
exp5full$id <- 1:8760
exp5full$Cook <- HypCube[5, 1]
exp5full$Warm <- HypCube[5, 2]
exp5full$Cold <- HypCube[5, 3]
exp5full$Run <- as.character(5)

# combine all dataframes
exp <- bind_rows(exp1full, exp2full, exp3full, exp4full, exp5full)


##################################################
################# model fitting ##################
##################################################

##### linear model

### linear model with all variables
lm <- lm(Exposure ~ Date + Time + ME + Cook + Warm + Cold + Run, exp)

# summary
summary(lm)

# residual plots
par(mfrow = c(2, 2))
plot(lm)


### linear model with selected variables
lm1 <- lm(Exposure ~ ME + Cook + Warm + Cold, exp)

# summary
summary(lm1)

# residual plots
par(mfrow = c(2, 2))
plot(lm1)


##### Gamma GLM

# add small noise value to 0s
exp_gamma <- exp
exp_gamma[exp_gamma == 0] <- 0.000001

# fit model with all variables
glm <- glm(Exposure ~ ME + Cook + Warm + Cold, family = Gamma(link = "log"), 
           data = exp_gamma)

# summary
summary(glm)

# residual plots
par(mfrow = c(2, 2))
plot(glm)

# repeatedly using drop1() yields the reduced model with only ME as a predictor
# this is also true if quadratic and interaction terms are included in the model
drop1(glm, test = "LRT")

# fit model with reduced parameters
glm1 <- glm(Exposure ~ ME, family = Gamma(link = "log"), data = exp_gamma)

# summary
summary(glm1)

# residual plots
par(mfrow = c(2, 2))
plot(glm1)


######### plots ##########

# calculate daily average exposure
exp_day <- exp %>% 
  group_by(Date, Cook, Warm, Cold, Run) %>% 
  summarise(Average = mean(Exposure))

# plot daily average exposure for full year
ggplot(exp_day) +
  geom_line(aes(x = Date, y = Average, colour = Run)) +
  labs(title = "Daily averages of exposure to PM2.5 for all runs", 
       y  = "Exposure (in ?g/m?)") +
  theme_bw()

# comparison plot of residuals
par(mfrow=c(2, 2), mar = c(4,4,2,1) + 0.1)
plot(lm, which = 1, sub = "")
plot(glm1, which = 1, sub = "")
plot(lm, which = 2, sub = "")
plot(glm1, which = 2, sub = "")


######################### PART 7: APPLICATION ######################### 

# assign housting type
Model_data <- assign_housingtype(population_data)

# normal run for april
modelrun_april <- calculate_exposure(Model_data, 2, 0, 1, 0, PM_data, 30,
                                     "2019-04-01", "2019-04-30")
save(modelrun_april, file = "~/Dropbox/Github/SPFFinalReport/Output/CaseStudy1/modelrun_april.RDATA")

# rollback run for april
modelrun_april_rb <- calculate_exposure(Model_data, 2, 0, 1, 0, PM_rollback_data,
                                        30, "2019-04-01", "2019-04-30")
save(modelrun_april_rb, file = "~/Dropbox/Github/SPFFinalReport/Output/CaseStudy1/modelrun_rb_april.RDATA")

# normal run for july
modelrun_july <- calculate_exposure(Model_data, 2, 0, 1, 0, PM_data, 30,
                                    "2019-07-01", "2019-07-31")
save(modelrun_july, file = "~/Dropbox/Github/SPFFinalReport/Output/CaseStudy1/modelrun_july.RDATA")

# rollback run for july
modelrun_july_rb <- calculate_exposure(Model_data, 2, 0, 1, 0, PM_rollback_data, 30,
                                       "2019-07-01", "2019-07-31")
save(modelrun_july_rb, file = "~/Dropbox/Github/SPFFinalReport/Output/CaseStudy1/modelrun_rb_july.RDATA")


######################### PART 8: RESULTS ######################### 

###################################################################
################## calculating days exceeding x0 ##################
###################################################################

########## APRIL ##########

##### 1: personal exposure

# combine runs
modelrun <- modelrun_april[[1]]

# change column names
for (i in 1:30) {
  colnames(modelrun)[i*2+1] <- paste("Ind_act_", i, sep = "")
  colnames(modelrun)[i*2+2] <- paste("Ind_exp_", i, sep = "")
}

# predictive distribution over hours
exposure <- dplyr::select(modelrun, Date, contains("Exp"))

exposure_april <- pivot_longer(exposure, cols = starts_with("Ind"), 
                               names_to = "Individual", values_to = "Exposure") %>% 
  group_by(Date, Individual) %>% 
  summarise(Exposure = mean(Exposure))


nrow(subset(exposure_april, Exposure > 11))/30 # 17.2 days
nrow(subset(exposure_april, Exposure > 23))/30 #  2.6 days
nrow(subset(exposure_april, Exposure > 35))/30 #  0.9 days
nrow(subset(exposure_april, Exposure > 41))/30 #  0.2 days
nrow(subset(exposure_april, Exposure > 47))/30 #  0.03 days
nrow(subset(exposure_april, Exposure > 53))/30 #  0.0 days 
nrow(subset(exposure_april, Exposure > 58))/30 #  0.0 days
nrow(subset(exposure_april, Exposure > 64))/30 #  0.0 days
nrow(subset(exposure_april, Exposure > 70))/30 #  0.0 days


##### 2: ambient concentration

individuals_april <- modelrun_april[[2]]

long <- unique(individuals_april$Longitude_match)
lat <- unique(individuals_april$Latitude_match)

ambient <- subset(PM_data, Date >= as.Date("2019-04-01") & 
                    Date <= as.Date("2019-04-30"))
ambient <- subset(ambient, Longitude %in% long)
ambient <- subset(ambient, Latitude %in% lat)

ambient_Avg_april <- ambient %>% 
  group_by(Date) %>% 
  summarise(Average = mean(PM2.5))


nrow(subset(ambient_Avg_april, Average > 11)) # 21.0 days
nrow(subset(ambient_Avg_april, Average > 23)) #  6.0 days
nrow(subset(ambient_Avg_april, Average > 35)) #  2.0 days
nrow(subset(ambient_Avg_april, Average > 41)) #  2.0 days
nrow(subset(ambient_Avg_april, Average > 47)) #  1.0 days
nrow(subset(ambient_Avg_april, Average > 53)) #  1.0 days
nrow(subset(ambient_Avg_april, Average > 58)) #  1.0 days
nrow(subset(ambient_Avg_april, Average > 64)) #  1.0 days
nrow(subset(ambient_Avg_april, Average > 70)) #  0.0 days


########## July ##########

##### 2: personal exposure

# combine runs
modelrun <- modelrun_july[[1]]

# change column names
for (i in 1:30) {
  colnames(modelrun)[i*2+1] <- paste("Ind_act_", i, sep = "")
  colnames(modelrun)[i*2+2] <- paste("Ind_exp_", i, sep = "")
}

# predictive distribution over hours
exposure <- dplyr::select(modelrun, Date, contains("Exp"))

exposure_july <- pivot_longer(exposure, cols = starts_with("Ind"), 
                              names_to = "Individual", values_to = "Exposure") %>% 
  group_by(Date, Individual) %>% 
  summarise(Exposure = mean(Exposure))


nrow(subset(exposure_july, Exposure > 11))/30 #  6.9 days
nrow(subset(exposure_july, Exposure > 23))/30 #  0.0 days
nrow(subset(exposure_july, Exposure > 35))/30 #  0.0 days 
nrow(subset(exposure_july, Exposure > 41))/30 #  0.0 days
nrow(subset(exposure_july, Exposure > 47))/30 #  0.0 days
nrow(subset(exposure_july, Exposure > 53))/30 #  0.0 days 
nrow(subset(exposure_july, Exposure > 58))/30 #  0.0 days
nrow(subset(exposure_july, Exposure > 64))/30 #  0.0 days
nrow(subset(exposure_july, Exposure > 70))/30 #  0.0 days


##### 2: ambient concentration

individuals_july <- modelrun_july[[2]]

long <- unique(individuals_july$Longitude_match)
lat <- unique(individuals_july$Latitude_match)

ambient <- subset(PM_data, Date >= as.Date("2019-07-01") & 
                    Date <= as.Date("2019-07-31"))
ambient <- subset(ambient, Longitude %in% long)
ambient <- subset(ambient, Latitude %in% lat)

ambient_Avg_july <- ambient %>% 
  group_by(Date) %>% 
  summarise(Average = mean(PM2.5))


nrow(subset(ambient_Avg_july, Average > 11)) #   8.0 days
nrow(subset(ambient_Avg_july, Average > 23)) #   0.0 days
nrow(subset(ambient_Avg_july, Average > 35)) #   0.0 days
nrow(subset(ambient_Avg_july, Average > 41)) #   0.0 days
nrow(subset(ambient_Avg_july, Average > 47)) #   0.0 days
nrow(subset(ambient_Avg_july, Average > 53)) #   0.0 days
nrow(subset(ambient_Avg_july, Average > 58)) #   0.0 days
nrow(subset(ambient_Avg_july, Average > 64)) #   0.0 days
nrow(subset(ambient_Avg_july, Average > 70)) #   0.0 days


##############################################################
############# calculating measure of uncertainty #############
##############################################################

# define function for p_i
calculate_pi <- function (N, exp, x0, day) {
  1/N * sum(subset(exp, Date == as.Date(day))$Exposure > x0)
}

# definfe functon for p_{j|i}
calculate_pji <- function(N, exp, x0, day_j, day_i) {
  pi <- calculate_pi(N, exp, x0, day_i)
  pj <- calculate_pi(N, exp, x0, day_j)
  
  pji <- 1/(N^2 * pi) * sum((subset(exp, Date == as.Date(day_i))$Exposure > x0) %o%
                              (subset(exp, Date == as.Date(day_j))$Exposure > x0))
  
  # check if pji is a number (if there are no exposures > x0 it returns NAN: not a number)
  if(is.nan(pji)) {
    0
  } else {
    pji
  }
}

# set threshold
x <- 11       # 11, 23, 35, 41, 47, 53, 58, 64, 70

# adjust for month
days <- unique(exposure_april$Date) # exposure_july$Date or exposur_april$Date
exposure_data <- exposure_april     # exposure_july or exposur_april

###### calculate standard error term for full month

# initialise variable that store term total
se <- 0

# loop over all days
for (i in 1:30) {
  
  #calculate pi for day
  pi <- calculate_pi(30, exposure_data, x, days[i])
  
  # and add to total
  se <- se + pi*(1 - pi)
}


##### calculate correlation term for full month

# initialise variable to store term total
dependance <- 0

# loop over all days
for(i in 1:30) {
  
  # find all days apart from current day for all possible combinations
  js <- 1:30
  js <- js[-i]
  
  # for each day calculate all combinations with other days excluding where i = j
  for(j in js) {
    pi <- calculate_pi(30, exposure_data, x, days[i])
    pj <- calculate_pi(30, exposure_data, x, days[j])
    pji <- calculate_pji(30, exposure_data, x, days[j], days[i])
    
    # add to total
    dependance <- dependance + pi*(pji - pj)
  }
}

# both terms included in measure of uncertainty
se
dependance

# calculate full measure of uncertainty
dependance + se

##### APRIL
# for 11: 3.683333
# for 23: 0.2455556
# for 35: 0.2755556
# for 41: 0.1477778
# for 47: 0.03222222
# for 53: 0

##### JULY
# for 11: 3.146667
# for 23: 0


##################################################
################## result plots ##################
##################################################

########## JULY ##########

# extract results
results_july <- modelrun_july[[1]]

# change column names
for (i in 1:30) {
  colnames(results_july)[i*2+1] <- paste("Ind_act_", i, sep = "")
  colnames(results_july)[i*2+2] <- paste("Ind_exp_", i, sep = "")
}

##### PLOT 1: predictive distribution by hours

# prepare data
hour_data_july <- dplyr::select(results_july, Date, Time, contains("Exp")) %>% 
  pivot_longer(cols = starts_with("Ind"), names_to = "Individual", 
               values_to = "Exposure") %>% 
  group_by(Time, Individual) %>% 
  summarise(Exposure = mean(Exposure))

# plot
hour_plot_july <- ggplot(hour_data_july) +
  geom_boxplot(aes(x = as.factor(Time), y = Exposure)) +
  labs(subtitle = "July",
       x = "Hour", y = "Exposure (in ?g/m?)") + 
  scale_x_discrete(breaks = seq(0, 23, by = 2)) +
  theme(plot.subtitle = element_text(hjust = 0.5))


##### PLOT 2: predictive distribution by MEs

# prepare data
exposure_july <- dplyr::select(results_july, Date, Time, contains("Exp")) %>% 
  pivot_longer(cols = starts_with("Ind"), names_to = "Individual", 
               values_to = "Exposure")

activity_july <- dplyr::select(results_july, Date, Time, contains("act")) %>% 
  pivot_longer(cols = starts_with("Ind"), names_to = "Individual", 
               values_to = "Activity")

ME_data_july <- data.frame(activity_july, Exposure = exposure_july$Exposure) %>% 
  group_by(Individual, Activity) %>% 
  summarise(Exposure = mean(Exposure))

# plot
ME_plot_july <- ggplot(ME_data_july) +
  geom_boxplot(aes(x = Activity, y = Exposure)) +
  labs(subtitle = "July", x = "Microenvironment", y = "Exposure (in ?g/m?)") +
  theme(plot.subtitle = element_text(hjust = 0.5))


##### PLOT 3: exposure vs ambient by days

# prepare data
daily_exposure_july <- dplyr::select(results_july, Date, Time, contains("Exp")) %>% 
  pivot_longer(cols = starts_with("Ind"), names_to = "Individual", 
               values_to = "Exposure") %>%
  group_by(Individual, Date) %>% 
  summarise(Exposure = mean(Exposure))

# calculate average ambient concentrations in areas of simulated individuals
longs_july <- unique(modelrun_july[[2]]$Longitude_match)
lats_july <- unique(modelrun_july[[2]]$Latitude_match)

ambient_july <- subset(PM_data, Date >= as.Date("2019-07-01") & 
                         Date <= as.Date("2019-07-31")) %>% 
  subset(Longitude %in% longs_july & Latitude %in% lats_july) %>% 
  group_by(Date) %>% 
  summarise(Average = mean(PM2.5))

# plot
ambient_plot_july <- ggplot() +
  geom_boxplot(aes(x = Date, y = Exposure, group = Date), 
               data = daily_exposure_july) +
  geom_line(aes(x = Date, y = Average), data = ambient_july, colour = "red") +
  labs(subtitle = "July", x = "Date", y = "Exposure (in ?g/m?)") +
  theme(plot.subtitle = element_text(hjust = 0.5))

##### PLOT 4: rollback

# extract results
results_july_rb <- modelrun_july_rb[[1]]

# change column names
for (i in 1:30) {
  colnames(results_july_rb)[i*2+1] <- paste("Ind_act_", i, sep = "")
  colnames(results_july_rb)[i*2+2] <- paste("Ind_exp_", i, sep = "")
}

# prepare data
monthly_exposure_july <- dplyr::select(results_july, Date, Time, 
                                       contains("Exp")) %>% 
  pivot_longer(cols = starts_with("Ind"), names_to = "Individual", 
               values_to = "Exposure") %>%
  group_by(Individual) %>% 
  summarise(Exposure = mean(Exposure))

monthly_exposure_july_rb <- dplyr::select(results_july_rb, Date, Time,
                                          contains("Exp")) %>% 
  pivot_longer(cols = starts_with("Ind"), names_to = "Individual", 
               values_to = "Exposure") %>%
  group_by(Individual) %>% 
  summarise(Exposure = mean(Exposure))

# plot
rollback_plot_july <- ggplot() +
  geom_boxplot(aes(x = "No rollback", y = Exposure), data = monthly_exposure_july) +
  geom_boxplot(aes(x = "Rollback", y = Exposure), data = monthly_exposure_july_rb) +
  labs(subtitle = "July", x = "", y = "Exposure (in ?g/m?)") +
  theme(plot.subtitle = element_text(hjust = 0.5))


########## APRIL #########

# extract results
results_april <- modelrun_april[[1]]

# change column names
for (i in 1:30) {
  colnames(results_april)[i*2+1] <- paste("Ind_act_", i, sep = "")
  colnames(results_april)[i*2+2] <- paste("Ind_exp_", i, sep = "")
}

##### PLOT 1: predictive distribution by hours

# prepare data
hour_data_april <- dplyr::select(results_april, Date, Time, contains("Exp")) %>% 
  pivot_longer(cols = starts_with("Ind"), names_to = "Individual", 
               values_to = "Exposure") %>% 
  group_by(Time, Individual) %>% 
  summarise(Exposure = mean(Exposure))

# plot
hour_plot_april <- ggplot(hour_data_april) +
  geom_boxplot(aes(x = as.factor(Time), y = Exposure)) +
  labs(subtitle = "April", x = "Hour", y = "Exposure (in ?g/m?)") +
  scale_x_discrete(breaks = seq(0, 23, by = 2)) +
  theme(plot.subtitle = element_text(hjust = 0.5))


##### PLOT 2: predictive distribution by MEs

# prepare data
exposure_april <- dplyr::select(results_april, Date, Time, contains("Exp")) %>% 
  pivot_longer(cols = starts_with("Ind"), names_to = "Individual", 
               values_to = "Exposure")

activity_april <- dplyr::select(results_april, Date, Time, contains("act")) %>% 
  pivot_longer(cols = starts_with("Ind"), names_to = "Individual", 
               values_to = "Activity")

ME_data_april <- data.frame(activity_april, Exposure = exposure_april$Exposure) %>% 
  group_by(Individual, Activity) %>% 
  summarise(Exposure = mean(Exposure))

# plot
ME_plot_april <- ggplot(ME_data_april) +
  geom_boxplot(aes(x = Activity, y = Exposure)) +
  labs(subtitle = "April", x = "Microenvironment", y = "Exposure (in ?g/m?)") +
  theme(plot.subtitle = element_text(hjust = 0.5))


##### PLOT 3: exposure vs ambient by days

# prepare data
daily_exposure_april <- dplyr::select(results_april, Date, Time, 
                                      contains("Exp")) %>% 
  pivot_longer(cols = starts_with("Ind"), names_to = "Individual", 
               values_to = "Exposure") %>%
  group_by(Individual, Date) %>% 
  summarise(Exposure = mean(Exposure))

# calculate average ambient concentrations in areas of simulated individuals
longs_april <- unique(modelrun_april[[2]]$Longitude_match)
lats_april <- unique(modelrun_april[[2]]$Latitude_match)

ambient_april <- subset(PM_data, Date >= as.Date("2019-04-01") & 
                          Date <= as.Date("2019-04-30")) %>% 
  subset(Longitude %in% longs_april & Latitude %in% lats_april) %>% 
  group_by(Date) %>% 
  summarise(Average = mean(PM2.5))

# plot
ambient_plot_april <- ggplot() +
  geom_boxplot(aes(x = Date, y = Exposure, group = Date), 
               data = daily_exposure_april) +
  geom_line(aes(x = Date, y = Average), data = ambient_april, colour = "red") +
  labs(subtitle = "April", x = "Date", y = "Exposure (in ?g/m?)") +
  theme(plot.subtitle = element_text(hjust = 0.5))


##### PLOT 4: rollback

# extract results
results_april_rb <- modelrun_april_rb[[1]]

# change column names
for (i in 1:30) {
  colnames(results_april_rb)[i*2+1] <- paste("Ind_act_", i, sep = "")
  colnames(results_april_rb)[i*2+2] <- paste("Ind_exp_", i, sep = "")
}

# prepare data
monthly_exposure_april <- dplyr::select(results_april, Date, Time, 
                                        contains("Exp")) %>% 
  pivot_longer(cols = starts_with("Ind"), names_to = "Individual", 
               values_to = "Exposure") %>%
  group_by(Individual) %>% 
  summarise(Exposure = mean(Exposure))

monthly_exposure_april_rb <- dplyr::select(results_april_rb, Date, Time,
                                           contains("Exp")) %>% 
  pivot_longer(cols = starts_with("Ind"), names_to = "Individual", 
               values_to = "Exposure") %>%
  group_by(Individual) %>% 
  summarise(Exposure = mean(Exposure))

# plot
rollback_plot_april <- ggplot() +
  geom_boxplot(aes(x = "No rollback", y = Exposure), 
               data = monthly_exposure_april) +
  geom_boxplot(aes(x = "Rollback", y = Exposure), 
               data = monthly_exposure_april_rb) +
  labs(subtitle = "April", x = "", y = "Exposure (in ?g/m?)") +
  theme(plot.subtitle = element_text(hjust = 0.5))

########## combine April and July in one plot #########

##### PLOT 1: predictive distribution by hours

hour_plot <- ggpubr::ggarrange(hour_plot_april, hour_plot_july)
ggpubr::annotate_figure(hour_plot, top = ggpubr::text_grob("Predictive distributions
                                                           of exposure by hour",
                                                           size = 14))


##### PLOT 2: predictive distribution by MEs

ME_plot <- ggpubr::ggarrange(ME_plot_april, ME_plot_july)
ggpubr::annotate_figure(ME_plot, top = ggpubr::text_grob("Predictive distributions
                                                         of exposure by
                                                         microenvironment", 
                                                         size = 14))


##### PLOT 3: exposure vs ambient by days

ambient_plot <- ggpubr::ggarrange(ambient_plot_april, ambient_plot_july)
ggpubr::annotate_figure(ambient_plot, top = ggpubr::text_grob("Exposure vs ambient
                                                              concentrations", 
                                                              size = 14))


##### PLOT 4: rollback

rollback_plot <- ggpubr::ggarrange(rollback_plot_april, rollback_plot_july)
ggpubr::annotate_figure(rollback_plot, top = ggpubr::text_grob("Effects of a policy
                                                               change", 
                                                               size = 14))

