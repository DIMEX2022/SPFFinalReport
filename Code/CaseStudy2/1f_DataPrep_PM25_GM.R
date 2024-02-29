#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

# Setting working directory
setwd('~/Dropbox/Github/SPFFinalReport/')

# Loading source code
source('~/Dropbox/Github/SPFFinalReport/Code/CaseStudy2/0_Source.R')

# Loading shapefiles 
load("Data/Processed/Shapefiles/shapefiles.RData")

##############################################
### Preparing PM data from ground monitors ###
##############################################
# Subsetting Greater Manchester shapefiles
mcr_msoa <- subset(ew_msoa, parent_area_name %in% c('Bolton', 'Bury', 'Manchester', 'Oldham', 'Rochdale',
                                                    'Salford', 'Stockport', 'Tameside', 'Trafford', 'Wigan'))

# Converting to long lat
mcr_msoa <- mcr_msoa %>%
  st_transform(CRS("+proj=longlat +datum=WGS84 +no_defs"))

# Reading in LTN network data
tmp1 <- read.csv('Data/Raw/PM25/GroundMonitors/BroomLane_AQ_hourly.csv')
tmp2 <- read.csv('Data/Raw/PM25/GroundMonitors/CromwellAQ_hourly.csv')
tmp3 <- read.csv('Data/Raw/PM25/GroundMonitors/DelamereRoad_AQ_hourly.csv')
tmp4 <- read.csv('Data/Raw/PM25/GroundMonitors/GrangethorpeAQ_2b_hourly.csv')
tmp5 <- read.csv('Data/Raw/PM25/GroundMonitors/ManorRad_AQ_hourly.csv')
tmp6 <- read.csv('Data/Raw/PM25/GroundMonitors/SladeLane_AQ_hourly.csv')

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
  dplyr::select(date, pm2.5 = PM2.5 , site, code, hour, datetime = ds) %>%
  dplyr::mutate(pm2.5 = ifelse(pm2.5 <= 0, NA, pm2.5),
                source = 'ltn')

# Removing unecessary datasets
rm(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6)

# List of Stations
stations_aurn_dat <- importMeta(source = "aurn", all = TRUE) %>%
  filter(variable == 'PM2.5' &
           (as.Date(start_date, format = '%Y-%m-%d') <= as.Date('2021-12-31')) &
           (as.Date(end_date, format = '%Y-%m-%d') >= as.Date('2020-01-01') |
              end_date == 'ongoing') &
           longitude > (st_bbox(mcr_msoa)[1] - 0.5) &
           longitude < (st_bbox(mcr_msoa)[3] + 0.5) &
           latitude > (st_bbox(mcr_msoa)[2] - 0.5) &
           latitude < (st_bbox(mcr_msoa)[4] + 0.5)) %>%
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
  # Altering columns 
  dplyr::mutate(
    # Removing negative values
    pm2.5 = ifelse(pm2.5 <= 0, NA, pm2.5),
    # Adding hour
    hour = if_else(is.na(as.numeric(substr(date, 12, 13))),
                   0, as.numeric(substr(date, 12, 13))),
    # Keeping datetime
    datetime = date,
    # Adding just the date
    date = as.Date(substr(date, 1, 10))) %>%
  # Dropping unecessary columns
  dplyr::select(-c(ws, wd, air_temp))

# Appenidng both together
gm_dat <- rbind(aurn_dat, ltn_dat)
stations_gm_dat <- rbind(stations_aurn_dat, stations_ltn_dat)


#################################################
### Aggregating to MSOA and bringing together ###
#################################################
# Getting closest station for each MSOA
A1 <- apply(proxy::dist(mcr_msoa[,c('cent_long', 'cent_lat')] %>% st_drop_geometry(),
                        stations_aurn_dat[,c('longitude', 'latitude')]), 1, which.min)
A2 <- apply(proxy::dist(mcr_msoa[,c('cent_long', 'cent_lat')] %>% st_drop_geometry(),
                        stations_ltn_dat[,c('longitude', 'latitude')]), 1, which.min)
A3 <- apply(proxy::dist(mcr_msoa[,c('cent_long', 'cent_lat')] %>% st_drop_geometry(),
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
for (i in as.character(seq(as.Date('2020-12-01'), as.Date('2021-07-31'), by = 1))){
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
    A1 <- apply(proxy::dist(mcr_msoa[,c('cent_long', 'cent_lat')] %>% st_drop_geometry(),
                            test1[,c('longitude', 'latitude')]),
                1, which.min)
    # Getting closest non-missing station for each MSOA
    A2 <- apply(proxy::dist(mcr_msoa[,c('cent_long', 'cent_lat')] %>% st_drop_geometry(),
                            test2[,c('longitude', 'latitude')]),
                1, which.min)
    # Getting closest non-missing station for each MSOA
    A3 <- apply(proxy::dist(mcr_msoa[,c('cent_long', 'cent_lat')] %>% st_drop_geometry(),
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
rm(tmp1, tmp2, tmp3, aurn_dat, ltn_dat, mcr_msoa,
   stations_aurn_dat, stations_ltn_dat)

######################
### Saving outputs ###
######################
# Save aurn data on monitor level
save(gm_dat, file = "Data/Processed/PM25/GroundMonitoring/pm25_aurn.RData")
save(stations_gm_dat, file = "Data/Processed/PM25/GroundMonitoring/pm25_aurn_metadata.RData")

# Save aurn data on MSOA level
save(pm25_gm, file = "Data/Processed/PM25/pm25_gm.RData")








