#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

# Setting working directory
setwd('~/Dropbox/Github/SPFFinalReport/')

# Loading source code
source('~/Dropbox/Github/SPFFinalReport/Code/CaseStudy2/0_Source.R')

# Read population data
load("Data/Processed/Population/pop_dat.RData")
load("Data/Processed/PM25/pm25_gm.RData")

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# Setting seed
set.seed(1409)

############################
### Estimating exposures ###
############################
### Changes in the below from the main code to get small set of test data: 
###    * I removed the loop, which previously repeated the exposure 
###      estimation through the different MSOAs 
###    * Change the reference to the test dataset

# Saving datasets 
load('Code/CaseStudy2/RTests/test_activities.RData')

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
save(activities_complete, file = 'Code/CaseStudy2/RTests/test_exposures_July_2021.RData')

# Clearing Workspace
rm(list = ls())




