################################################################################################
### This file contains all source code needed for the the Data Integration Model for personal###
### exposures (DIMEX)                                                                        ###
################################################################################################
### * sample_population - A function to sample activities sequences from the time use survey ###
################################################################################################

#################################
### Loading libraryd packages ###
#################################
# import libraries
library(DescTools)
library(doParallel)  
library(dplyr)
library(EnvStats)    
library(ggmap)  
library(haven)
library(lhs) 
library(lubridate)  
library(maptools)
library(openair)
library(parallel)
library(plyr)
library(raster)
library(readxl)
library(reshape2)
library(rgeos)    
library(rgdal)
library(sf)
library(tidyverse)  
library(truncnorm) 
library(zoo)

####################################################################
### sample_population - A function to sample activities sequences###
###      from the time use survey                                ###
####################################################################
### Input: * pop_dat - Population data to sample from            ###
###        * tus_dat - Time use survey data to sample from       ###
###        * nsample - Number of population samples              ###
###        * weights - Sampling weights for the time use diaries ###
###        * pop_strata - Stratification variables for sampling  ###
###                       population data                        ###
###        * tus_strata - Stratification variables for sampling  ###
###                       time use data                          ###
###        * start_date - Start date for personal exposures      ###
###        * end_date - End date for personal exposures          ###
###        * keep - Variables to keep from the TUS               ###
### Output: * Dataframe with sampled activity sequences for      ###
###           each individual between start_date end_date        ###
####################################################################
sample_population <- function(pop_dat,
                              tus_dat,
                              nsample,
                              weights = NULL, 
                              pop_strata,
                              tus_strata,
                              start_date,
                              end_date,
							  keep) {
  ################
  ### Preamble ###
  ################
  # If no weights are provided for sampling then use equal weighting 
  if (is.null(weights) == TRUE){
    tus_dat$weights <- 1
  }
  # Else use the weights specified
  else{
    tus_dat$weights <- tus_dat[,weights]
  }
  ########################
  ### Getting metadata ###
  ########################
  # Getting a list of strata for activities 
  lst_strata <- tus_dat %>% 
    # Grouping by stratification variables
    group_by_at(.vars = tus_strata)%>% 
    # Summarising 
    dplyr::summarise(n = dplyr::n()) %>%
    ungroup() %>%
    # Adding label to each strata
    dplyr::mutate(strata = 1:dplyr::n()) %>%
    dplyr::select(-c(n))
  # Getting activities ID and 
  tus_act_id <- tus_dat %>%
    # Merging on stratification labels
    left_join(lst_strata,
              by = tus_strata) 
  # Normalising the weights within each strata
  tus_act_id <- tus_act_id %>%
    # Merging on summary of weights in each strata
    left_join(tus_act_id%>%
                dplyr::group_by(strata) %>%
                dplyr::summarise(sums = sum(weights)),
              by = 'strata') %>%
    # Normalising weights
    mutate(weights = weights/sums) %>%
    # Removing unnecessary columns 
    dplyr::select(-c(sums))
  ##########################################
  ### Preparing population time profiles ###
  ##########################################
  # Sampling population to find exposures for
  pop_dat2 <- pop_dat %>%
    group_by_at(.vars = pop_strata) %>%
    sample_n(size = nsample, 
             replace = FALSE)
  # Preparing shell dataset for sampling 
  activities <- expand.grid(pop_id = pop_dat2$pop_id,
                            date = seq(as.Date(start_date), as.Date(end_date), by = 1)) %>%
    # Adding on day information
    mutate(day_label = weekdays(date),
           day = as.numeric(factor(weekdays(date), levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))), 
           daytype = case_when(day %in% c(1,7) ~ 1,
                               day %in% 2:6 ~ 2),
           daytype_label = case_when(day %in% c(1,7) ~ 'Weekend',
                                     day %in% 2:6 ~ 'Weekday'),
           season = case_when(month(date) %in% c(12, 1, 2) ~ 1,
                              month(date) %in% c(3:5) ~ 2,
                              month(date) %in% c(6:8) ~ 3,
                              month(date) %in% c(9:11) ~ 4),
           season_label = case_when(month(date) %in% c(12, 1, 2) ~ 'Winter',
                                    month(date) %in% c(3:5) ~ 'Spring',
                                    month(date) %in% c(6:8) ~ 'Summer',
                                    month(date) %in% c(9:11) ~ 'Autumn')) %>%
    # Merging on population data
    left_join(pop_dat %>%
                select_at(c('pop_id', 'sex', 'agegr4', 'nssec5')),
              by = 'pop_id') %>%
    # Merging on stratification labels
    left_join(lst_strata,
              by = tus_strata)
  # Removing unecessary datasets
  rm(pop_dat2, lst_strata)
  ###################################
  ### Sampling activity sequences ###
  ###################################
  # Empty dataset 
  activities$act_id <- as.numeric(NA)
  # Loop for each strata
  for (i in unique(activities$strata)){
    # Sampling within each strata
    activities$act_id[which(activities$strata == i)] <- 
      sample(x = tus_act_id$act_id[which(tus_act_id$strata == i)], 
             size = length(activities$pop_id[which(activities$strata == i)]),
             prob = tus_act_id$weights[which(tus_act_id$strata == i)],
             replace = TRUE)
  }
  # Merging on the activity data 
  activities <- merge(activities, 
                      tus_dat[, c('act_id', 'time', 'time_label', keep)],
                      by = 'act_id') %>%
    arrange(pop_id, date, time) %>%
	dplyr::select(-c(sex, agegr4, nssec5, strata))
  # Returning activity samples 
  return(activities)
}
