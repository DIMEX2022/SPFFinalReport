#####################
### Preliminaries ###
#####################
# Loading source code
library(here)
source(here("Code", "CaseStudy2", "0_Source.R"))

# Setting seed
set.seed(1409)

#################################
### Preparing population data ###
#################################
# lad_tus_hse_257 # Bolton
# lad_tus_hse_258 # Bury
# lad_tus_hse_259 # Manchester
# lad_tus_hse_260 # Oldham
# lad_tus_hse_261 # Rochdale
# lad_tus_hse_262 # Salford
# lad_tus_hse_263 # Stockport
# lad_tus_hse_264 # Tameside
# lad_tus_hse_265 # Trafford
# lad_tus_hse_266 # Wigan

# Empty dataset
pop_dat <- NULL

# Loop for each LA
for (i in 257:266){
  # Read in data
  test <- read.csv(paste('Data/CaseStudy2/Raw/Population/lad_TUS_', i, '.txt', sep = ''))
  # Appending pop_dat on
  pop_dat <- rbind(pop_dat, test)
  # Printing index
  print(i)
}

# Preparing population data
pop_dat <- pop_dat %>%
  # Adding population unique identifier
  dplyr::mutate(pop_id = 1:dplyr::n(),
                sex_label = case_when(Sex == 0 ~ 'Female',
                                      Sex == 1 ~ 'Male'),
                sex = Sex,
                agegr1_label = cut(age,
                                   breaks = c(0, 19, 29, 44, 59, 74, 100),
                                   labels = c('<20', '20-29', '30-44', '45-59', '60-74', '75+'),
                                   include.lowest = TRUE),
                agegr2_label = cut(age,
                                   breaks = c(0, seq(4, 64, by = 5), 100),
                                   labels = c('0-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34',
                                              '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65+')),
                agegr3_label = cut(age,
                                   breaks = c(0, 17, 29, 44, 59, 74, 100),
                                   labels = c('<18','18-29', '30-44', '45-59', '60-74', '75+'),
                                   include.lowest = TRUE),
                agegr4_label = cut(age,
                                   breaks = c(0, 15, 29, 44, 59, 74, 100),
                                   labels = c('<16','16-29', '30-44', '45-59', '60-74', '75+'),
                                   include.lowest = TRUE),
                agegr1 = as.numeric(agegr1_label),
                agegr2 = as.numeric(agegr2_label),
                agegr3 = as.numeric(agegr3_label),
                agegr4 = as.numeric(agegr4_label),
                pwkstat_label = case_when(pwkstat == "0. N/A (age<16)" ~ "Under 16",
                                          pwkstat == "1. Employee FT" ~ "Employee FT",
                                          pwkstat == "2. Employee PT" ~ "Employee PT",
                                          pwkstat == "3. Employee unspec." ~ "Employee unspec.",
                                          pwkstat == "4. Self-Employed" ~ "Self-Employed",
                                          pwkstat == "5. Unemployed" ~ "Unemployed",
                                          pwkstat == "6. Retired" ~ "Retired",
                                          pwkstat == "7. Homemaker/Mat.Leave" ~ "Homemaker/Mat.Leave",
                                          pwkstat == "8. Student FT" ~ "Student FT",
                                          pwkstat == "9. Long-term Sick/Dis" ~ "Long-term Sick/Dis",
                                          pwkstat == "10. Other" ~ "Other"),
                pwkstat = case_when(pwkstat == "0. N/A (age<16)" ~ 9,
                                    pwkstat == "1. Employee FT" ~ 1,
                                    pwkstat == "2. Employee PT" ~ 2,
                                    pwkstat == "3. Employee unspec." ~ 3,
                                    pwkstat == "4. Self-Employed" ~ 4,
                                    pwkstat == "5. Unemployed" ~ 5,
                                    pwkstat == "6. Retired" ~ 6,
                                    pwkstat == "7. Homemaker/Mat.Leave" ~ 7,
                                    pwkstat == "8. Student FT" ~ 8,
                                    pwkstat == "9. Long-term Sick/Dis" ~ 10,
                                    pwkstat == "10. Other" ~ 11),
                work = case_when(pwkstat %in% c(1, 2, 3, 4) ~ 1,
                                 TRUE ~ 0),
                nssec5 = case_when(nssec5 == "0. Not applicable" ~ 99,
                                   nssec5 == "1. Mngt, admin & prof. occup." ~ 1,
                                   nssec5 == "2. Intermed. occup." ~ 2,
                                   nssec5 == "3. Small empl. & own account wkrs" ~ 3,
                                   nssec5 == "4. Lower superv & technic. occup." ~ 4,
                                   nssec5 == "5. (Semi)routine occup." ~ 5),
                sic2 = as.numeric(if_else(sic2d07 == "Item not applicable", "-1", sic2d07)),
                soc2010 = as.numeric(if_else(soc2010 == "Item not applicable", "-1", soc2010))) %>%
  # Adding the National Statistics Socio-economic classification
  # Reading and merging information on
  left_join(read_csv("Data/CaseStudy2/Raw/Misc/nssec_classification.csv") %>%
              dplyr::select(nssec5, nssec5_label) %>%
              unique(),
            by = 'nssec5') %>%
  # Reading and merging information on
  left_join(read_csv("Data/CaseStudy2/Raw/Misc/nssec_classification.csv") %>%
              dplyr::select(hhnssec5 = nssec5, hhnssec5_label = nssec5_label) %>%
              unique(),
            by = 'hhnssec5') %>%
  # Tidying labels and setting missings as non employed
  mutate(nssec5 = if_else(nssec5 == 99, -1, nssec5),
         nssec5_label = if_else(nssec5 == -1, "Not applicable", nssec5_label),
         hhnssec5 = if_else(hhnssec5 == 99, -1, nssec5),
         hhnssec5_label = if_else(hhnssec5 == -1, "Not applicable", hhnssec5_label)) %>%
  # Adding the Standard Occupational Classification
  # Reading and merging information on
  left_join(read_csv("Data/CaseStudy2/Raw/Misc/soc2010_classification.csv") %>%
              dplyr::select(soc2010, soc2010_label),
            by = 'soc2010') %>%
  # Tidying SOC2010 labels and setting missings as not applicable
  mutate(soc2010_label = if_else(is.na(soc2010_label), 'Not applicable', soc2010_label))  %>%
  # Adding the Standard Industrial Classification (SIC2007 labels and codes)
  # Reading and merging information on
  left_join(read_csv("Data/CaseStudy2/Raw/Misc/sic2007_classification.csv") %>%
              dplyr::select(-c('sic3', 'sic3_label')) %>%
              unique(),
            by = 'sic2')%>%
  # Sampling and merging on housetype 
  left_join(data.frame(hid = unique(pop_dat$hid),
                       housetype = sample(c("detached", "semi-detached", "terrace", "flat"), 
                                          prob = c(0.3000, 0.2746, 0.2340, 0.1914), size = length(unique(pop_dat$hid)),
                                          replace = TRUE)),
            by = 'hid') %>%
  # Tidying labels and setting missings as non employed
  mutate(sic1_label = if_else(is.na(sic1_label), 'Not applicable', sic1_label),
         sic2_label = if_else(is.na(sic2_label), 'Not applicable', sic2_label)) %>%
  # Selecting relevant columns
  dplyr::select(pop_id, pid_tus, hid, housetype, area_id = area, age, agegr1, agegr1_label, agegr2, agegr2_label, 
                agegr3, agegr3_label,  agegr4, agegr4_label, sex, sex_label, pwkstat, pwkstat_label, 
                work, nssec5, nssec5_label, hhnssec5, hhnssec5_label, soc2010, soc2010_label, sic2, 
                sic2_label, sic1, sic1_label, punknown:pmunknown, keyworker = keyworkercasa, 
                keyworkergroup = keyworkeroccupationgroupcasa)

# Save shapefiles
save(pop_dat, file = "Data/CaseStudy2/Processed/Population/pop_dat.RData")
