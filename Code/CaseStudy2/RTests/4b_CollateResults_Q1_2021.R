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
load('Code/CaseStudy2/RTests/test_exposures_Q1_2021.RData')

# Preparing case study 2
out_q12021 <- activities_complete %>%
  # Only keeping Q1 2021
  dplyr::filter(date >= as.Date("2021-01-01")) %>%
  # Getting exposures 
  dplyr::mutate(exposure_cams = ifelse(micro_group == "outdoor", pm25_cams_agg,
                                       ifelse(micro_group == "indoor", pm25_cams_agg_inh,
                                              ifelse(micro_group == "transport", pm25_cams_agg_tns,
                                                     ifelse(micro_group == "home", pm25_cams_agg_hhd, NA)))),
                exposure_emep = ifelse(micro_group == "outdoor", pm25_emep_agg,
                                       ifelse(micro_group == "indoor", pm25_emep_agg_inh,
                                              ifelse(micro_group == "transport", pm25_emep_agg_tns,
                                                     ifelse(micro_group == "home", pm25_emep_agg_hhd, NA)))),
                exposure_five = ifelse(micro_group == "outdoor", pm25_five,
                                       ifelse(micro_group == "indoor", pm25_five_inh,
                                              ifelse(micro_group == "transport", pm25_five_tns,
                                                     ifelse(micro_group == "home", pm25_five_hhd, NA)))))%>%
  # Averaging by day
  ddply(.(area_id, pop_id, date, daytype, daytype_label, season, season_label,
          sex, sex_label, agegr4, agegr4_label, nssec5, nssec5_label),
        summarize,
        exposure_cams = mean(exposure_cams),
        exposure_emep = mean(exposure_emep),
        exposure_five = mean(exposure_five),
        pm25_cams_agg = mean(pm25_cams_agg),
        pm25_emep_agg = mean(pm25_emep_agg),
        pm25_five = mean(pm25_five))

# Saving outputs
save(out_q12021, file = 'Code/CaseStudy2/RTests/test_summary_Q1_2021.RData')

# Clearing Workspace
rm(list = ls())


