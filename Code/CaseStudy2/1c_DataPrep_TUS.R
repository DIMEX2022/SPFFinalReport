#####################
### Preliminaries ###
#####################
# Loading source code
library(here)
source(here("Code", "CaseStudy2", "0_Source.R"))

###################################
### Reading in time  use survey ###
###################################
# Reading in individual data
uktus15_individual <- read_dta("Data/CaseStudy2/Raw/TimeUseSurvey/uktus15_individual.dta")

# Preparing individual dataset
uktus15_individual <- uktus15_individual %>%
  # Removing individuals without any samples
  filter(psu != -2)   %>%
  # Altering columns needed for analysis
  mutate(pid_tus = as.numeric(paste(serial, pnum, sep = '')),
         sex_label = case_when(DMSex == 2 ~ 'Female',
                               DMSex == 1 ~ 'Male'),
         sex = case_when(DMSex == 2 ~ 0,
                         DMSex == 1 ~ 1),
         agegr1_label = cut(DVAge,
                            breaks = c(0, 19, 29, 44, 59, 74, 100),
                            labels = c('<20', '20-29', '30-44', '45-59', '60-74', '75+'),
                            include.lowest = TRUE),
         agegr2_label = cut(DVAge,
                            breaks = c(0, seq(4, 64, by = 5), 100),
                            labels = c('0-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34',
                                       '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65+')),
         agegr3_label = cut(DVAge,
                            breaks = c(0, 17, 29, 44, 59, 74, 100),
                            labels = c('<18','18-29', '30-44', '45-59', '60-74', '75+'),
                            include.lowest = TRUE),
         agegr4_label = cut(DVAge,
                            breaks = c(0, 15, 29, 44, 59, 74, 100),
                            labels = c('<16','16-29', '30-44', '45-59', '60-74', '75+'),
                            include.lowest = TRUE),
         agegr1 = as.numeric(agegr1_label),
         agegr2 = as.numeric(agegr2_label),
         agegr3 = as.numeric(agegr3_label),
         agegr4 = as.numeric(agegr4_label),
         pwkstat_label = case_when(WorkSta %in% c(-9, -8, -7, -2) ~ "Unknown",
                                   WorkSta == 2 & FtPtWk == 1 ~ "Employee FT",
                                   WorkSta == 2 & FtPtWk == 2 ~ "Employee PT",
                                   WorkSta == 2 & FtPtWk == -1 ~ "Employee unspec.",
                                   WorkSta == 1 ~ "Self-Employed",
                                   WorkSta == 3 ~ "Unemployed",
                                   WorkSta == 4 ~ "Retired",
                                   WorkSta %in% c(5, 6) ~ "Homemaker/Mat.Leave",
                                   WorkSta == 7 ~ "Student FT",
                                   WorkSta == -1 ~ "Under 16",
                                   WorkSta == 8 ~ "Long-term Sick/Dis",
                                   WorkSta %in% c(9, 10, 97) ~ "Other"),
         pwkstat = case_when(WorkSta %in% c(-9, -8, -7, -2) ~ 99,
                             WorkSta == 2 & FtPtWk == 1 ~ 1,
                             WorkSta == 2 & FtPtWk == 2 ~ 2,
                             WorkSta == 2 & FtPtWk == -1 ~ 3,
                             WorkSta == 1 ~ 4,
                             WorkSta == 3 ~ 5,
                             WorkSta == 4 ~ 6,
                             WorkSta %in% c(5, 6) ~ 7,
                             WorkSta == 7 ~ 8,
                             WorkSta == -1 ~ 9,
                             WorkSta == 8 ~ 10,
                             WorkSta %in% c(9, 10, 97) ~ 11)) %>%
  # Selecting relevant columns
  dplyr::select(pid_tus, age = DVAge, agegr1, agegr1_label, agegr2, agegr2_label, agegr3, agegr3_label,
                agegr4, agegr4_label, sex, sex_label, psu, pwkstat, pwkstat_label, sic2 = SIC2007, 
                sicgroup = dsic, nssec, soc2010 = XSOC2010, soc2010group = dsoc)

# Adding on occupation and socioeconomic status information
uktus15_individual <- uktus15_individual %>%
  # Adding the Standard Occupational Classification (SOC2010 labels and codes)
  left_join(read_csv("Data/CaseStudy2/Raw/Misc/soc2010_classification.csv") %>%
              dplyr::select(soc2010, soc2010_label),
            by = 'soc2010') %>%
  # Tidying SOC2010 labels and setting missings as not applicable
  mutate(soc2010_label = if_else(is.na(soc2010_label), 'Not applicable', soc2010_label),
         soc2010group_label = case_when(soc2010group == -1 ~ "Not applicable",
                                        soc2010group == 1 ~ "Managers",
                                        soc2010group == 2 ~ "Professionals",
                                        soc2010group == 3 ~ "Assoc. professionals",
                                        soc2010group == 4 ~ "Administrative",
                                        soc2010group == 5 ~ "Skilled Trade",
                                        soc2010group == 6 ~ "Caring qnd Leisure",
                                        soc2010group == 7 ~ "Sales and cust services",
                                        soc2010group == 8 ~ "Machine Operatives",
                                        soc2010group == 9 ~ "Elementary occupations"),
         soc2010group = as.numeric(soc2010group)) %>%
  # Adding the National Statistics Socio-economic classification (NS-SEC labels and codes)
  left_join(read_csv("Data/CaseStudy2/Raw/Misc/nssec_classification.csv") %>%
              dplyr::select(nssec, nssec_label, nssec5, nssec5_label),
            by = 'nssec') %>%
  # Tidying labels and setting missings as non employed
  mutate(nssec_label = if_else(nssec == -1, "Not applicable", nssec_label),
         nssec5 = if_else(nssec == -1, -1, nssec5),
         nssec5_label = if_else(nssec == -1, "Not applicable", nssec5_label))%>%
  # Adding the Standard Industrial Classification (SIC2007 labels and codes)
  left_join(read_csv("Data/CaseStudy2/Raw/Misc/sic2007_classification.csv") %>%
              dplyr::select(-c('sic3', 'sic3_label')) %>%
              unique(),
            by = 'sic2') %>%
  # Tidying labels and setting missings as non employed
  mutate(sic1 = if_else(is.na(sic1), 'ZZZ', sic1),
         sic1_label = if_else(is.na(sic1_label), 'Not applicable', sic1_label),
         sic2_label = if_else(is.na(sic2_label), 'Not applicable', sic2_label),
         sicgroup_label = case_when(sicgroup == 1 ~ "Agriculture, forestry and fishing",
                                    sicgroup == 2 ~ "Manufacturing",
                                    sicgroup == 3 ~ "Energy and water supply",
                                    sicgroup == 4 ~ "Construction",
                                    sicgroup == 5 ~ "Distribution, hotels and restaurants",
                                    sicgroup == 6 ~ "Transport and communication",
                                    sicgroup == 7 ~ "Banking and finance",
                                    sicgroup == 8 ~ "Public admin, education and health",
                                    sicgroup == 9 ~ "Other services",
                                    sicgroup == -1 ~ "Not applicable")) %>%
  dplyr::select(pid_tus, psu, age, agegr1, agegr1_label, agegr2, agegr2_label, agegr3, agegr3_label, 
                agegr4, agegr4_label, sex, sex_label, pwkstat, pwkstat_label, nssec, nssec_label, 
                nssec5, nssec5_label, soc2010, soc2010group, soc2010_label, soc2010group_label,
                sic2, sic2_label, sic1, sic1_label, sicgroup, sicgroup_label)

# Reading in diary data
uktus15_diary_wide <- read_dta("Data/CaseStudy2/Raw/TimeUseSurvey/uktus15_diary_wide.dta")

# Preparing individual dataset
uktus15_diary_wide <- uktus15_diary_wide %>%
  # Removing individuals without any samples
  filter(psu != -2) %>%
  # Altering columns needed for analysis
  dplyr::mutate(pid_tus = as.numeric(paste(serial, pnum, sep = '')),
                act_id = 1:dplyr::n(),
                day_label = case_when(DiaryDay_Act == 1 ~ 'Sunday',
                                      DiaryDay_Act == 2 ~ 'Monday',
                                      DiaryDay_Act == 3 ~ 'Tuesday',
                                      DiaryDay_Act == 4 ~ 'Wednesday',
                                      DiaryDay_Act == 5 ~ 'Thursday',
                                      DiaryDay_Act == 6 ~ 'Friday',
                                      DiaryDay_Act == 7 ~ 'Saturday'),
                day = as.numeric(DiaryDay_Act),
                daytype = case_when(DiaryDay_Act %in% c(1,7) ~ 1,
                                    DiaryDay_Act %in% 2:6 ~ 2),
                daytype_label = case_when(DiaryDay_Act %in% c(1,7) ~ 'Weekend',
                                          DiaryDay_Act %in% 2:6 ~ 'Weekday'),
                season = case_when(dmonth %in% c(12, 1, 2) ~ 1,
                                   dmonth %in% c(3:5) ~ 2,
                                   dmonth %in% c(6:8) ~ 3,
                                   dmonth %in% c(9:11) ~ 4),
                season_label = case_when(dmonth %in% c(12, 1, 2) ~ 'Winter',
                                         dmonth %in% c(3:5) ~ 'Spring',
                                         dmonth %in% c(6:8) ~ 'Summer',
                                         dmonth %in% c(9:11) ~ 'Autumn')) %>%
  # Selecting relevant columns
  dplyr::select(pid_tus, act_id, date = DiaryDate_Act, day, day_label, daytype, daytype_label, season, season_label,
                weights_diary = dia_wt_a, weights_ind = dia_wt_b, act1_1:act1_144, wher_1:wher_144)

# Converting the wide dataset to long dataset
uktus15_diary_wide <- uktus15_diary_wide %>%
  # Long dataset to wide dataset
  melt(id.vars=c('pid_tus', 'act_id', 'date', 'day', 'day_label', 'daytype', 'daytype_label',
                 'season', 'season_label', 'weights_diary', 'weights_ind'),
       value.name = 'value',
       variable.name = 'time') %>%
  # Turning label into numeric
  mutate(tmp = case_when(substr(time, 1, 4) == 'act1' ~ 'activity',
                         substr(time, 1, 4) == 'wher' ~ 'location'),
         time = as.numeric(substr(time, 6, 100)))  %>%
  # Long to wide dataset
  reshape2::dcast(pid_tus + act_id + date + day + day_label + daytype + daytype_label + season + season_label + weights_diary + weights_ind + time ~ tmp,
                  value.var = "value") %>%
  # Sorting dataset
  arrange(act_id, time) %>%
  # Merging on time point labels
  left_join(read_csv("Data/CaseStudy2/Raw/TimeUseSurvey/uktus_metadata_timepoints.csv"),
            by = 'time') %>%
  # Merging on activity labels
  left_join(read_csv("Data/CaseStudy2/Raw/TimeUseSurvey/uktus_metadata_activity.csv"),
            by = 'activity')%>%
  # Merging on location labels
  left_join(read_csv("Data/CaseStudy2/Raw/TimeUseSurvey/uktus_metadata_location.csv"),
            by = 'location')

# Merging individual information to the diaries
tus_dat <- merge(uktus15_individual,
                 uktus15_diary_wide,
                 by = 'pid_tus',
                 all.y = TRUE)

# Removing unecessary datasets
rm(uktus15_individual, uktus15_diary_wide)

# Adding extra required variables
tus_dat <- tus_dat %>%
  # Removing those with missing nssec 
  filter(nssec != -9) %>%
  # Adding a new flag to know the level of missingness 
  # in location for each activity ID
  left_join(ddply(tus_dat, 
                  .(act_id), 
                  summarize, 
                  percmissing = sum(location %in% c(-9, 0, 10, 99))/144),
            by = 'act_id')

# Save shapefiles
save(tus_dat, file = "Data/CaseStudy2/Processed/TimeUseSurvey/tus_dat.RData")


