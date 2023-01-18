#####################
### Preliminaries ###
#####################
# Loading source code
library(here)
source(here("Code", "CaseStudy2", "0_Source.R"))

# Read population data
load("Data/CaseStudy2/Processed/Population/pop_dat.RData")
load("Data/CaseStudy2/Processed/PM25/pm25_gm.RData")

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# Setting seed
# set.seed(1409)

############################
### Estimating exposures ###
############################
# Loop for each MSOA
for (k in unique(pop_dat$area_id)){
  t1 <- Sys.time()
  # Saving datasets 
  load(paste('Output/CaseStudy2/Activities/activities_', k, '_2021.RData', sep = ''))
  
  # Parparing data for exposure modelling
  activities_complete <- activities_complete %>%
    # Only keeping specific period
    filter(as.numeric(date) >= 18779 & 
             as.numeric(date) <= 18839) %>%
    # Adding on demographic variables
    left_join(pop_dat %>%
                dplyr::select(pop_id, area_id, sex, sex_label, agegr4, agegr4_label, nssec5, nssec5_label),
              by = 'pop_id') %>%
    # Merging on pm data
    left_join(pm25_gm %>%
                # Only keeping specific period
                filter(date >= as.Date('2021-06-01') & 
                         date <= as.Date('2021-07-31')) %>%
                dplyr::select(area_id, date, hour, pm25_aurn_near, pm25_gm_near, pm25_ltn_near),
              by = c('area_id', 'date', 'hour')) %>% 
    as.data.frame()
  
  # Transportation exposures
  activities_complete <- calculate_transport(activities_complete, ambient = "pm25_aurn_near", outvar = "pm25_aurn_near_tns")
  activities_complete <- calculate_transport(activities_complete, ambient = "pm25_ltn_near", outvar = "pm25_ltn_near_tns")
  activities_complete <- calculate_transport(activities_complete, ambient = "pm25_gm_near", outvar = "pm25_gm_near_tns")
  
  # Indoor-not-home exposures
  activities_complete <- calculate_indoor(activities_complete, ambient = "pm25_aurn_near", outvar = "pm25_aurn_near_inh")
  activities_complete <- calculate_indoor(activities_complete, ambient = "pm25_ltn_near", outvar = "pm25_ltn_near_inh")
  activities_complete <- calculate_indoor(activities_complete, ambient = "pm25_gm_near", outvar = "pm25_gm_near_inh")
  
  # Household exposures
  activities_complete <- calculate_household(act_dat = activities_complete, pop_dat = pop_dat, 
                                             ambient = "pm25_aurn_near", outvar = "pm25_aurn_near_hhd")
  activities_complete <- calculate_household(act_dat = activities_complete, pop_dat = pop_dat, 
                                             ambient = "pm25_ltn_near", outvar = "pm25_ltn_near_hhd")
  activities_complete <- calculate_household(act_dat = activities_complete, pop_dat = pop_dat, 
                                             ambient = "pm25_gm_near", outvar = "pm25_gm_near_hhd")
  # Saving datasets 
  save(activities_complete, file = paste('~/Desktop/exposures_', k, '_July_2021.RData', sep = ''))
  
  t2 <- Sys.time()
  # Printing index
  print(k)
  print(t2-t1)
}

# Clearing Workspace
rm(list = ls())




