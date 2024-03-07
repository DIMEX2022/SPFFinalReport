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
load("Data/Processed/PM25/pm25_emep.RData")
load("Data/Processed/PM25/pm25_cams.RData")

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# Setting seed
set.seed(1409)

# Merging pm2.5 data together
pm25_ctm <- pm25_cams %>%
  dplyr::select(area_id, date, hour, pm25_cams_agg)%>%
  left_join(pm25_emep %>%
              dplyr::select(area_id, date, hour, pm25_emep_agg = pm25_cams_agg),
            by = c("area_id", "date", "hour")) %>%
  filter(as.Date(date) >= as.Date("2020-12-20") & 
           as.Date(date) <= as.Date("2021-03-31")) %>%
  mutate(pm25_emep_agg = ifelse(is.na(pm25_emep_agg), pm25_cams_agg, pm25_emep_agg),
         pm25_five = 5,
         date = as.Date(date))

# Removing uncessary datasets
rm(pm25_cams, pm25_emep)

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
  filter(as.numeric(date) >= 18616 & 
           as.numeric(date) <= 18717) %>%
  # Adding on demographic variables
  left_join(pop_dat %>%
              dplyr::select(pop_id, area_id, sex, sex_label, agegr4, agegr4_label, nssec5, nssec5_label),
            by = 'pop_id') %>%
  # Merging on pm data
  left_join(pm25_ctm %>%
              dplyr::select(area_id, date, hour, pm25_cams_agg, pm25_five, pm25_emep_agg),
            by = c('area_id', 'date', 'hour')) %>% 
  as.data.frame()

# Transportation exposures
activities_complete <- calculate_transport(activities_complete, ambient = "pm25_cams_agg", outvar = "pm25_cams_agg_tns")
activities_complete <- calculate_transport(activities_complete, ambient = "pm25_emep_agg", outvar = "pm25_emep_agg_tns")
activities_complete <- calculate_transport(activities_complete, ambient = "pm25_five", outvar = "pm25_five_tns")

# Indoor-not-home exposures
activities_complete <- calculate_indoor(activities_complete, ambient = "pm25_cams_agg", outvar = "pm25_cams_agg_inh")
activities_complete <- calculate_indoor(activities_complete, ambient = "pm25_emep_agg", outvar = "pm25_emep_agg_inh")
activities_complete <- calculate_indoor(activities_complete, ambient = "pm25_five", outvar = "pm25_five_inh")

# Household exposures
activities_complete <- calculate_household(act_dat = activities_complete, pop_dat = pop_dat, 
                                           ambient = "pm25_cams_agg", outvar = "pm25_cams_agg_hhd")
activities_complete <- calculate_household(act_dat = activities_complete, pop_dat = pop_dat, 
                                           ambient = "pm25_emep_agg", outvar = "pm25_emep_agg_hhd")
activities_complete <- calculate_household(act_dat = activities_complete, pop_dat = pop_dat, 
                                           ambient = "pm25_five", outvar = "pm25_five_hhd")

# Saving datasets 
save(activities_complete, file = 'Code/CaseStudy2/RTests/test_exposures_Q1_2021.RData')

# Clearing Workspace
rm(list = ls())




