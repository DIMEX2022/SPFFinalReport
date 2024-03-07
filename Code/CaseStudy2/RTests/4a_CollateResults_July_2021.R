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

############################
### Estimating exposures ###
############################
### Changes in the below from the main code to get small set of test data: 
###    * I removed the loop, which previously repeated the collation and 
###      summary through the different MSOAs 

# Saving datasets 
load('Code/CaseStudy2/RTests/test_exposures_July_2021.RData')

# Preparing case study 2
out_july2021 <- activities_complete %>%
  # Only keepting July
  dplyr::filter(date >= as.Date("2021-07-01")) %>%
  # Getting exposures 
  dplyr::mutate(exposure_aurn = ifelse(micro_group == "outdoor", pm25_aurn_near,
                                       ifelse(micro_group == "indoor", pm25_aurn_near_inh,
                                              ifelse(micro_group == "transport", pm25_aurn_near_tns,
                                                     ifelse(micro_group == "home", pm25_aurn_near_hhd, NA)))),
                exposure_ltn = ifelse(micro_group == "outdoor", pm25_ltn_near,
                                      ifelse(micro_group == "indoor", pm25_ltn_near_inh,
                                             ifelse(micro_group == "transport", pm25_ltn_near_tns,
                                                    ifelse(micro_group == "home", pm25_ltn_near_hhd, NA)))),
                exposure_gm = ifelse(micro_group == "outdoor", pm25_gm_near,
                                     ifelse(micro_group == "indoor", pm25_gm_near_inh,
                                            ifelse(micro_group == "transport", pm25_gm_near_tns,
                                                   ifelse(micro_group == "home", pm25_gm_near_hhd, NA)))))%>%
  # Averaging by day
  ddply(.(area_id, pop_id, date, daytype, daytype_label, season, season_label,
          sex, sex_label, agegr4, agegr4_label, nssec5, nssec5_label),
        summarize,
        exposure_aurn = mean(exposure_aurn),
        exposure_ltn = mean(exposure_ltn),
        exposure_gm = mean(exposure_gm),
        pm25_aurn_near = mean(pm25_aurn_near),
        pm25_ltn_near = mean(pm25_ltn_near),
        pm25_gm_near = mean(pm25_gm_near))

# Saving datasets 
save(out_july2021, file = 'Code/CaseStudy2/RTests/test_summary_July_2021.RData')

# Clearing Workspace
rm(list = ls())


