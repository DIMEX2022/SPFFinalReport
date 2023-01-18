#####################
### Preliminaries ###
#####################
# Loading source code
library(here)
source(here("Code", "CaseStudy2", "0_Source.R"))

# Loading shapefiles 
load("Data/CaseStudy2/Processed/Shapefiles/shapefiles.RData")

##########################################
### Preparing PM data from CAMS-Europe ###
##########################################
################### The following code takes a while to run, so population estimates  ###################
################### were run separately and saved to a raster (found underneath)      ###################
# # Empty raster
# r <- raster(xmn = -10,
#             xmx = 3,
#             ymn = 49,
#             ymx = 62,
#             res = 0.1)
# 
# # Adding unique  ID
# r[] <- 1:(dim(r)[1]*dim(r)[2])
# 
# # Empty dataset to append to
# weights <- NULL
# 
# # Extracting values
# a1 <- raster::extract(r, # Grid unique IDs
#                       uk_full, # Shapefiles
#                       weight = TRUE, # Give us Weights of the cells so we can do a weighted average of the cells we overlap
#                       small = TRUE) # Small areas in comparison to the raster
# 
# # Setting to missing if not in UK
# r[!(r[] %in% a1[[1]])] <- NA
# 
# # file names
# files <- c('Data/CaseStudy2/Raw/PM25/CAMS-Europe/CAMSEurope_20201201-20210531.nc',
#            'Data/CaseStudy2/Raw/PM25/CAMS-Europe/CAMSEurope_20210601-20211130.nc',
#            'Data/CaseStudy2/Raw/PM25/CAMS-Europe/CAMSEurope_20211201-20220430.nc')
# 
# # files start dates
# start_date <- c(as.Date('2020-12-01'),
#                 as.Date('2021-06-01'),
#                 as.Date('2021-12-01'))
# 
# # Loop for each file
# for (i in 1:length(files)){
#   # Opening raster
#   ncin <- raster(files[i],
#                  band = 1,
#                  verbose = FALSE,
#                  stopIfNotEqualSpaced = FALSE)
#   # Getting the number of  days
#   N_days <- floor(nbands(ncin)/24)
#   # Dates
#   Dates <- start_date[i] + (1:N_days) - 1
#   # Loop for each day in the year
#   for (j in 1:N_days){
#     # Getting date
#     date <- Dates[j]
#     # Looping for each hour in the day
#     for (k in (24*(j-1)+1):(24*j)){
#       # Opening raster
#       ncin <- raster(files[i],
#                      band = k,
#                      verbose = FALSE,
#                      stopIfNotEqualSpaced = FALSE)
#       # # Cropping for the UK
#       # ncin <- crop(ncin, extent(-10, 3, 49, 62))
#       extent(ncin) <- c(-10, 3, 49, 62)
#       # Setting to missing if not in  UK
#       ncin[is.na(r[])] <- NA
#       # Saving raster
#       writeRaster(ncin,
#                   filename = paste('Data/CaseStudy2/Processed/PM25/CAMS-Europe/PM25_', date, '-', sprintf("%02d", k %% 24), "00.tif", sep = ''),
#                   overwrite = TRUE)
#       # else {keep <- keep + ncin}
#       print(paste(date, '-', sprintf("%02d", (k - 1) %% 24), "00", sep = ''))
#     }
#   }
# }
#########################################################################################################
#########################################################################################################

# Empty raster
r0 <- raster(xmn = -2.8,
             xmx = -1.8,
             ymn = 53.2,
             ymx = 53.7,
             res = 0.1)

# Adding unique  ID
r0[] <- 1:(dim(r0)[1]*dim(r0)[2])

# Subsetting Greater Manchester shapefiles
mcr_msoa <- subset(ew_msoa, parent_area_name %in% c('Bolton', 'Bury', 'Manchester', 'Oldham', 'Rochdale',
                                                    'Salford', 'Stockport', 'Tameside', 'Trafford', 'Wigan'))

# Converting to long lat
mcr_msoa <- spTransform(mcr_msoa, CRS("+proj=longlat +datum=WGS84 +no_defs"))

# Extracting values
a1 <- raster::extract(r0, # Grid unique IDs
                      mcr_msoa, # Shapefiles
                      weight = TRUE, # Give us Weights of the cells so we can do a weighted average of the cells we overlap
                      small = TRUE) # Small areas in comparison to the raster

# Empty dataset to append to
Weights_msoa <- NULL

# Loop for each area
for (i in 1:length(a1)){
  # Converting Weights_msoa to dataframes
  tmp <- as.data.frame(a1[[i]])
  # Removing NAs
  tmp <- subset(tmp, !is.na(value))
  # Reweighting the Weights_msoa after zeroes removed
  tmp$weight <- tmp$weight/sum(tmp$weight)
  # New Region Name
  tmp$area_id <- mcr_msoa@data$area_id[i]
  # Creating new dataset
  if (i == 1) {Weights_msoa <- tmp}
  else {Weights_msoa <- rbind(Weights_msoa, tmp)}
  # Removing unecessary data
  rm(tmp)
}

# Altering column names
names(Weights_msoa)[1] <- c('IDGRID')

# Merging to skeleton dataset
pm25_cams <- NULL

# Loop for each date
for (i in as.character(seq(as.Date('2020-12-01'), as.Date('2021-12-31'), by = 1))){
  # Loop for each time
  for (j in 0:23){
    # Reading in PM25 from CAMS
    r <- raster(paste('Data/CaseStudy2/Processed/PM25/CAMS-Europe/PM25_', i, '-', sprintf("%02d", j), "00.tif", sep = ''))
    # Renaming raster
    names(r) <- 'pm25'
    # Creating aggregated estimates of PM25 by MSOA
    tmp1 <- r %>%
      # Converting raster to dataframe
      crop(r0) %>%
      stack(r0) %>%
      rasterToPoints() %>%
      as.data.frame()%>%
      # Renaming columns
      dplyr::select(IDGRID = layer, pm25) %>%
      # Merging on weights to aggregate
      right_join(Weights_msoa,
                 by = 'IDGRID') %>%
      # Aggregating grid to
      ddply(.(area_id),
            summarize,
            pm25_cams_agg = weighted.mean(pm25, weight))
    # Extracting PM2.5 values at centroids
    tmp1$pm25_cams_cent<- raster::extract(r, mcr_msoa@data[,c('cent_long', 'cent_lat')])
    # Adding on date and hour
    tmp1 <- tmp1 %>%
      mutate(hour = j,
             date = i) %>%
      # Outputting datasets
      dplyr::select(area_id, date, hour, pm25_cams_cent, pm25_cams_agg)
    # Appending together
    pm25_cams <- rbind(pm25_cams, tmp1)
    # Removing uncessary datasets
    rm(tmp1, tmp2)    
    # Printing index
    print(paste('PM25_', i, '-', sprintf("%02d", j), "00.tif", sep = ''))
  }
}

# Save cams
save(pm25_cams, file = "Data/CaseStudy2/Processed/PM25/pm25_cams.RData")

##############################################
### Preparing PM data from ground monitors ###
##############################################
# Subsetting Greater Manchester shapefiles
mcr_msoa <- subset(ew_msoa, parent_area_name %in% c('Bolton', 'Bury', 'Manchester', 'Oldham', 'Rochdale',
                                                    'Salford', 'Stockport', 'Tameside', 'Trafford', 'Wigan'))

# Converting to long lat
mcr_msoa <- spTransform(mcr_msoa, CRS("+proj=longlat +datum=WGS84 +no_defs"))

# Reading in LTN network data
tmp1 <- read.csv('Data/CaseStudy2/Raw/PM25/GroundMonitors/BroomLane_AQ_hourly.csv')
tmp2 <- read.csv('Data/CaseStudy2/Raw/PM25/GroundMonitors/CromwellAQ_hourly.csv')
tmp3 <- read.csv('Data/CaseStudy2/Raw/PM25/GroundMonitors/DelamereRoad_AQ_hourly.csv')
tmp4 <- read.csv('Data/CaseStudy2/Raw/PM25/GroundMonitors/GrangethorpeAQ_2b_hourly.csv')
tmp5 <- read.csv('Data/CaseStudy2/Raw/PM25/GroundMonitors/ManorRad_AQ_hourly.csv')
tmp6 <- read.csv('Data/CaseStudy2/Raw/PM25/GroundMonitors/SladeLane_AQ_hourly.csv')

# Adding Longitude
tmp1$latitude <- 53.441161
tmp2$latitude <- 53.445053
tmp3$latitude <- 53.442702
tmp4$latitude <- 53.436213
tmp5$latitude <- 53.447703
tmp6$latitude <- 53.448551

# Adding Latitude
tmp1$longitude <- -2.185467
tmp2$longitude <- -2.188442
tmp3$longitude <- -2.188183
tmp4$longitude <- -2.203991
tmp5$longitude <- -2.183646
tmp6$longitude <- -2.197556

# Getting site label
tmp1$site <- 'BroomLane'
tmp2$site <- 'Cromwell'
tmp3$site <- 'DelamereRoad'
tmp4$site <- 'Grangethorpe'
tmp5$site <- 'ManorRoad'
tmp6$site <- 'SladeLane'

# Adding site label
tmp1$code <- 'LTN-BLN'
tmp2$code <- 'LTN-CRM'
tmp3$code <- 'LTN-DRD'
tmp4$code <- 'LTN-GRA'
tmp5$code <- 'LTN-MRD'
tmp6$code <- 'LTN-SLN'

# Adding site label
tmp1$date <- as.Date(substr(tmp1$ds, 1, 10))
tmp2$date <- as.Date(substr(tmp2$ds, 1, 10))
tmp3$date <- as.Date(substr(tmp3$ds, 1, 10))
tmp4$date <- as.Date(substr(tmp4$ds, 1, 10))
tmp5$date <- as.Date(substr(tmp5$ds, 1, 10))
tmp6$date <- as.Date(substr(tmp6$ds, 1, 10))

# Adding site label
tmp1$hour <- as.numeric(substr(tmp1$ds, 12, 13))
tmp2$hour <- as.numeric(substr(tmp2$ds, 12, 13))
tmp3$hour <- as.numeric(substr(tmp3$ds, 12, 13))
tmp4$hour <- as.numeric(substr(tmp4$ds, 12, 13))
tmp5$hour <- as.numeric(substr(tmp5$ds, 12, 13))
tmp6$hour <- as.numeric(substr(tmp6$ds, 12, 13))

# Loading in stations data
stations_ltn_dat <- rbind(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6) %>%
  dplyr::select(site, code, longitude, latitude) %>%
  unique()

# Pulling all data together
ltn_dat <- rbind(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6) %>%
  dplyr::select(date, pm2.5 = PM2.5 , site, code, hour)%>%
  dplyr::mutate(pm2.5 = ifelse(pm2.5 <= 0, NA, pm2.5))

# Removing unecessary datasets
rm(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6)

# List of Stations
stations_aurn_dat <- importMeta(source = "aurn", all = TRUE) %>%
  filter(variable == 'PM2.5' &
           (as.Date(end_date, format = '%Y-%m-%d') >= as.Date('2020-12-01') |
              end_date == 'ongoing') &
           longitude > (mcr_msoa@bbox[1,1] - 0.5) &
           longitude < (mcr_msoa@bbox[1,2] + 0.5) &
           latitude > (mcr_msoa@bbox[2,1] - 0.5) &
           latitude < (mcr_msoa@bbox[2,2] + 0.5)) %>%
  dplyr::select(code, site, latitude, longitude) %>%
  unique() %>%
  filter(!(site %in% c('Glazebury', 'Liverpool Speke')))

# Downloading pollutant/weather data
aurn_dat <- importAURN(site = stations_aurn_dat$code,
                       year = 2020:2021,
                       pollutant = c('pm2.5'),
                       meta = FALSE,
                       data_type = 'hourly',
                       verbose = FALSE) %>%
  dplyr::mutate(pm2.5 = ifelse(pm2.5 <= 0, NA, pm2.5))

# Getting date and hour
aurn_dat$hour <- as.numeric(substr(aurn_dat$date, 12, 13))
aurn_dat$date <- as.Date(substr(aurn_dat$date, 1, 10))

# Appenidng both together
gm_dat <- rbind(aurn_dat, ltn_dat)
stations_gm_dat <- rbind(stations_aurn_dat, stations_ltn_dat)

# Getting closest station for each MSOA
A1 <- apply(proxy::dist(mcr_msoa@data[,c('cent_long', 'cent_lat')],
                       stations_aurn_dat[,c('longitude', 'latitude')]), 1, which.min)
A2 <- apply(proxy::dist(mcr_msoa@data[,c('cent_long', 'cent_lat')],
                        stations_ltn_dat[,c('longitude', 'latitude')]), 1, which.min)
A3 <- apply(proxy::dist(mcr_msoa@data[,c('cent_long', 'cent_lat')],
                        stations_gm_dat[,c('longitude', 'latitude')]), 1, which.min)

# Getting data from nearest station (AURN)
tmp1 <- data.frame(area_id = mcr_msoa$area_id,
                   code_aurn = stations_aurn_dat$code[A1]) %>%
  left_join(aurn_dat %>%
              dplyr::select(code_aurn = code, date, hour, pm25_aurn = pm2.5),
            by = 'code_aurn') %>%
  dplyr::select(area_id, date, hour, pm25_aurn, code_aurn)

# Getting data from nearest station (LTN)
tmp2 <- data.frame(area_id = mcr_msoa$area_id,
                   code_ltn = stations_ltn_dat$code[A2]) %>%
  left_join(ltn_dat %>%
              dplyr::select(code_ltn = code, date, hour, pm25_ltn = pm2.5),
            by = 'code_ltn') %>%
  dplyr::select(area_id, date, hour, pm25_ltn, code_ltn)

# Getting data from nearest station (Both)
tmp3 <- data.frame(area_id = mcr_msoa$area_id,
                   code_gm = stations_gm_dat$code[A3]) %>%
  left_join(gm_dat %>%
              dplyr::select(code_gm = code, date, hour, pm25_gm = pm2.5),
            by = 'code_gm') %>%
  dplyr::select(area_id, date, hour, pm25_gm, code_gm)

# Merging together
pm25_gm <- expand.grid(area_id = mcr_msoa$area_id,
                       date =seq(as.Date('2020-12-01'), as.Date('2021-12-31'), by = 1),
                       hour = 0:23)%>%
  left_join(tmp1,
            by = c('area_id', 'date', 'hour')) %>%
  left_join(tmp2,
            by = c('area_id', 'date', 'hour')) %>%
  left_join(tmp3,
            by = c('area_id', 'date', 'hour'))

# Removing unecessary datasets
rm(tmp1, tmp2, tmp3)

# Getting stations with non-missing data
aurn_dat <- aurn_dat %>%
  left_join(stations_aurn_dat %>%
              dplyr::select(code, longitude, latitude),
            by = "code")
# Getting stations with non-missing data
ltn_dat <- ltn_dat %>%
  left_join(stations_ltn_dat %>%
              dplyr::select(code, longitude, latitude),
            by = "code")
# Getting stations with non-missing data
gm_dat <- gm_dat %>%
  left_join(stations_gm_dat %>%
              dplyr::select(code, longitude, latitude),
            by = "code")

# Empty dataset for data from nearest *non-missing* station
tmp1 <- NULL
tmp2 <- NULL
tmp3 <- NULL

# Loop for each date
for (i in as.character(seq(as.Date('2020-12-01'), as.Date('2021-12-31'), by = 1))){
  # Loop for each time
  for (j in 0:23){
    # Getting stations with non-missing data
    test1 <- aurn_dat %>%
      dplyr::filter(date == i & hour == j & !is.na(pm2.5))
    # Getting stations with non-missing data
    test2 <- ltn_dat %>%
      dplyr::filter(date == i & hour == j & !is.na(pm2.5))
    # Getting stations with non-missing data
    test3 <- gm_dat %>%
      dplyr::filter(date == i & hour == j & !is.na(pm2.5))
    # Getting closest non-missing station for each MSOA
    A1 <- apply(proxy::dist(mcr_msoa@data[,c('cent_long', 'cent_lat')],
                            test1[,c('longitude', 'latitude')]),
                1, which.min)
    # Getting closest non-missing station for each MSOA
    A2 <- apply(proxy::dist(mcr_msoa@data[,c('cent_long', 'cent_lat')],
                            test2[,c('longitude', 'latitude')]),
                1, which.min)
    # Getting closest non-missing station for each MSOA
    A3 <- apply(proxy::dist(mcr_msoa@data[,c('cent_long', 'cent_lat')],
                            test3[,c('longitude', 'latitude')]),
                1, which.min)
    # Getting data from nearest station (AURN)
    if (length(A1) > 0){
      test1 <- data.frame(area_id = mcr_msoa$area_id,
                          code_aurn_near = test1$code[A1]) %>%
        left_join(test1 %>%
                    dplyr::select(code_aurn_near = code, date, hour, pm25_aurn_near = pm2.5),
                  by = 'code_aurn_near') %>%
        dplyr::select(area_id, date, hour, pm25_aurn_near, code_aurn_near)
      # Appending
      tmp1 <- rbind(tmp1, test1)
    }
    # Getting data from nearest station (LTN)
    if (length(A2) > 0){
      test2 <- data.frame(area_id = mcr_msoa$area_id,
                          code_ltn_near = test2$code[A2]) %>%
        left_join(test2 %>%
                    dplyr::select(code_ltn_near = code, date, hour, pm25_ltn_near = pm2.5),
                  by = 'code_ltn_near') %>%
        dplyr::select(area_id, date, hour, pm25_ltn_near, code_ltn_near)
      # Appending
      tmp2 <- rbind(tmp2, test2)
    }
    if (length(A3) > 0){
      # Getting data from nearest station (Both)
      test3 <- data.frame(area_id = mcr_msoa$area_id,
                          code_gm_near = test3$code[A3]) %>%
        left_join(test3 %>%
                    dplyr::select(code_gm_near = code, date, hour, pm25_gm_near = pm2.5),
                  by = 'code_gm_near') %>%
        dplyr::select(area_id, date, hour, pm25_gm_near, code_gm_near)
      # Appending
      tmp3 <- rbind(tmp3, test3)
    }
    # Removing uncessary datasets
    rm(test1, test2, test3)
    # Printing index
    print(paste('PM25_', i, '-', sprintf("%02d", j), "00.tif", sep = ''))
  }
}

# Merging together
pm25_gm <- pm25_gm %>%
  left_join(tmp1,
            by = c('area_id', 'date', 'hour')) %>%
  left_join(tmp2,
            by = c('area_id', 'date', 'hour')) %>%
  left_join(tmp3,
            by = c('area_id', 'date', 'hour'))

# Removing unecessary files
rm(tmp1, tmp2, tmp3, aurn_dat, gm_dat, ltn_dat, mcr_msoa,
   stations_aurn_dat, stations_gm_dat, stations_ltn_dat)

# Save aurn data
save(pm25_gm, file = "Data/CaseStudy2/Processed/PM25/pm25_gm.RData")

###################################
### Preparing PM data from EMEP ###
###################################
################### The following code takes a while to run, so population estimates  ###################
################### were run separately and saved to a raster (found underneath)      ###################
# # file names
# files <- c("Data/CaseStudy2/Raw/PM25/EMEP/pm25_janfeb2021_regrid.nc",
#            "Data/CaseStudy2/Raw/PM25/EMEP/pm25_marapr2021_regrid.nc")
# 
# # files start dates
# start_date <- c(as.Date('2021-01-01'),
#                 as.Date('2021-03-01'))
# 
# # Loop for each file
# for (i in 1:length(files)){
#   # Opening raster to get number of days
#   ncin <- raster(files[i],
#                  band = 1,
#                  verbose = FALSE,
#                  stopIfNotEqualSpaced = FALSE)
#   # Getting the number of  days
#   N_days <- floor(nbands(ncin)/24)
#   # Dates
#   Dates <- start_date[i] + (1:N_days) - 1
#   # Loop for each day in the year
#   for (j in 1:N_days){
#     # Getting date
#     date <- Dates[j]
#     # Looping for each hour in the day
#     for (k in (24*(j-1)+1):(24*j)){
#       # Opening raster
#       ncin <- raster(files[i],
#                      band = k,
#                      verbose = FALSE,
#                      stopIfNotEqualSpaced = FALSE)
#       # Cropping for the UK
#       ncin <- crop(ncin, extent(-2.755, -1.895, 53.325, 53.695))
#       # Saving raster
#       writeRaster(ncin,
#                   filename = paste('Data/CaseStudy2/Processed/PM25/EMEP/PM25_', date, '-', sprintf("%02d", k %% 24), "00.tif", sep = ''),
#                   overwrite = TRUE)
#       # else {keep <- keep + ncin}
#       print(paste(date, '-', sprintf("%02d", (k - 1) %% 24), "00", sep = ''))
#     }
#   }
# }
#########################################################################################################
#########################################################################################################

# Empty raster
r0 <- raster(xmn = -2.755,
             xmx = -1.895,
             ymn = 53.325,
             ymx = 53.695,
             res = 0.01)

# Adding unique  ID
r0[] <- 1:(dim(r0)[1]*dim(r0)[2])

# Subsetting Greater Manchester shapefiles
mcr_msoa <- subset(ew_msoa, parent_area_name %in% c('Bolton', 'Bury', 'Manchester', 'Oldham', 'Rochdale',
                                                    'Salford', 'Stockport', 'Tameside', 'Trafford', 'Wigan'))

# Converting to long lat
mcr_msoa <- spTransform(mcr_msoa, CRS("+proj=longlat +datum=WGS84 +no_defs"))

# Extracting values
a1 <- raster::extract(r0, # Grid unique IDs
                      mcr_msoa, # Shapefiles
                      weight = TRUE, # Give us Weights of the cells so we can do a weighted average of the cells we overlap
                      small = TRUE) # Small areas in comparison to the raster

# Empty dataset to append to
Weights_msoa <- NULL

# Loop for each area
for (i in 1:length(a1)){
  # Converting Weights_msoa to dataframes
  tmp <- as.data.frame(a1[[i]])
  # Removing NAs
  tmp <- subset(tmp, !is.na(value))
  # Reweighting the Weights_msoa after zeroes removed
  tmp$weight <- tmp$weight/sum(tmp$weight)
  # New Region Name
  tmp$area_id <- mcr_msoa@data$area_id[i]
  # Creating new dataset
  if (i == 1) {Weights_msoa <- tmp}
  else {Weights_msoa <- rbind(Weights_msoa, tmp)}
  # Removing unecessary data
  rm(tmp)
}

# Altering column names
names(Weights_msoa)[1] <- c('IDGRID')

# Merging to skeleton dataset
pm25_emep <- NULL

# Loop for each date
for (i in as.character(seq(as.Date('2021-01-01'), as.Date('2021-04-30'), by = 1))){
  # Loop for each time
  for (j in 0:23){
    # Reading in PM25 from CAMS
    r <- raster(paste('Data/CaseStudy2/Processed/PM25/EMEP/PM25_', i, '-', sprintf("%02d", j), "00.tif", sep = ''))
    # Renaming raster
    names(r) <- 'pm25'
    # Creating aggregated estimates of PM25 by MSOA
    tmp1 <- r %>%
      # Converting raster to dataframe
      crop(r0) %>%
      stack(r0) %>%
      rasterToPoints() %>%
      as.data.frame()%>%
      # Renaming columns
      dplyr::select(IDGRID = layer, pm25) %>%
      # Merging on weights to aggregate
      right_join(Weights_msoa,
                 by = 'IDGRID') %>%
      # Aggregating grid to
      ddply(.(area_id),
            summarize,
            pm25_cams_agg = weighted.mean(pm25, weight))
    # Extracting PM2.5 values at centroids
    tmp1$pm25_cams_cent<- raster::extract(r, mcr_msoa@data[,c('cent_long', 'cent_lat')])
    # Adding on date and hour
    tmp1 <- tmp1 %>%
      mutate(hour = j,
             date = i) %>%
      # Outputting datasets
      dplyr::select(area_id, date, hour, pm25_cams_cent, pm25_cams_agg)
    # Appending together
    pm25_emep <- rbind(pm25_emep, tmp1)
    # Removing uncessary datasets
    rm(tmp1)
    # Printing index
    print(paste('PM25_', i, '-', sprintf("%02d", j), "00.tif", sep = ''))
  }
}

# Save aurn data
save(pm25_emep, file = "Data/CaseStudy2/Processed/PM25/pm25_emep.RData")







