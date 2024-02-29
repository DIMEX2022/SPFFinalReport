################################################################################################
### This file contains all source code needed for the the Data Integration Model for personal###
### exposures (DIMEX) case studies for the SPF final report                                  ###
################################################################################################
### * calculate_outdoor - A function to calculate the outdoor air pollution                  ###
### * calculate_household - A function to calculate the household not home air pollution     ###
### * calculate_indoor - A function to calculate the indoor not home air pollution           ###
### * calculate_transport - A function to calculate the transportation air pollution         ###
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

################################################################
### calculate_outdoor - A function to calculate the outdoor  ###
### air pollution                                            ###
################################################################
### Inputs: dat - Dataset containing sampled population with ### 
###               outdoor activities                         ###
### Outputs: Dataset containing the sampled PM2.5 estimate   ###
###          for outdoors                                    ###
################################################################
calculate_outdoor <- function(dat) {
  # Parameters
  a <- 0
  b <- 1
  # Estimating ambient 
  dat$conc <- a + b * dat$pm25
  # Returning dataset
  return(dat)
}

################################################################
### calculate_household - A function to calculate the house- ###
###                       hold not home air pollution        ###
################################################################
### Inputs: act_dat - Dataset containing the act profiles    ###
###                   with associated ambient air pollution  ###
###         pop_dat - Population dataset with attributes     ###
###                   needed for the mass balance equations  ###
###         ambient - column containing the ambient air      ###
###                   pollution estimates                    ###
###         outvar - Column to output the household concent- ###
###                  rations to                              ###
### Outputs: Dataset containing the sampled PM2.5 estimate   ###
###          for household.                                  ###
################################################################
calculate_household <- function(act_dat, 
                             pop_dat, 
                             ambient,
                             outvar){
  # Preparing data for Mass balance equations
  act_dat <- act_dat %>%
    dplyr::arrange(pop_id, date, time) %>%
    dplyr::group_by(pop_id) %>%
    dplyr::mutate(uniid = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(date, time, pop_id)  %>%
    left_join(pop_dat %>%
                dplyr::select(pop_id, housetype),
              by = 'pop_id') %>%
	as.data.frame()
  
  # Getting number of population 
  Npop <- length(unique(act_dat$pop_id))
  
  # Empty column 
  act_dat[,outvar] <- NA
  
  # Getting initial level of hou
  act_dat[which(act_dat$uniid == 1), outvar] <- rnorm(n = Npop, mean = 12, sd = 2)
  
  # Loop for each time point 
  for (i in 2:max(act_dat$uniid)){
    # Getting time point and previous time point
    tmp1 <- subset(act_dat, uniid == i)
    
    # penetration factor (Özkaynak et al. (1996))
    Fp <- rnorm(n = Npop, mean = 1, sd = 0.055)
    
    # deposition rate (Özkaynak et al. (1996))
    Fd <- rnorm(n = Npop, mean = 0.39, sd = 0.0825)
    
    # emission generating source (Özkaynak et al. (1996))
    SCooking <- rnorm(n = Npop, mean = 1.7, sd = 0.325)
    SOther <- rnorm(n = Npop, mean = 1.1, sd = 0.525)
    
    # Getting total emissions 
    # assumption: non-smokers
    S <- 
      # First adding the cooking emissions if they are cooking at home
      ((tmp1$activity %in% c(3110, 3100, 4210, 3190) & 
          tmp1$micro_group == 'home') + 0) * SCooking + 
      # Add in other emissions
      SOther
    
    # calculate air exchange rate (Murray and Burmaster (1995) Region 3)
    v <- if (unique(tmp1$season_label) == 'Winter') {
      rlnorm(n = Npop, meanlog = -0.958, sdlog = 0.589)
    } else if (unique(tmp1$season_label) == 'Spring') { 
      rlnorm(n = Npop, meanlog = -0.802, sdlog = 0.782)
    } else if (unique(tmp1$season_label) == 'Summer') {
      rlnorm(n = Npop, meanlog = -0.588, sdlog = 0.612)
    } else if (unique(tmp1$season_label) == 'Autumn') {
      rlnorm(n = Npop, meanlog = -0.787, sdlog = 0.453)
    }
    
    # calculate volume of home (zoopla + onaverage.co.uk)
    V <- ((tmp1$housetype == 'detached') + 0) * rtri(Npop, min = 81, max = 214, mode = 159) * runif(1, min = 2.1, max = 2.6) + 
      ((tmp1$housetype == 'semi-detached') + 0) * rtri(Npop, min = 56, max = 204, mode = 84)  * runif(1, min = 2.1, max = 2.6) + 
      ((tmp1$housetype == 'terrace') + 0) * rtri(Npop, min = 33, max = 155, mode = 59)  * runif(1, min = 2.1, max = 2.6) + 
      ((tmp1$housetype == 'flat') + 0) * rtri(Npop, min = 34, max = 106, mode = 41)  * runif(1, min = 2.1, max = 2.6)
    
    # extract ambient concentration
    Cout <- tmp1[, ambient, drop = TRUE]
    
    # concentration added
    Cadd <-  S / V + v * Fp * Cout
    
    # Getting previous concentrations 
    Cbefore <- act_dat[which(act_dat$uniid == i-1), outvar, drop = TRUE]
    
    # calculate indoor concentration (Zidek et al. (2007))
    # Cbefore * (1 - v - Fd) + Cadd
    act_dat[which(act_dat$uniid == i), outvar] <- 
      (Cadd / (v + Fd)) + (Cbefore - (Cadd / (v + Fd))) * exp(-(v + Fd) * 1)
  }
  # Returning 
  return(act_dat %>% dplyr::select(-c(housetype)))
}

################################################################
### calculate_indoor - A function to calculate the indoor    ###
### not home air pollution                                   ###
################################################################
### Inputs: dat - Dataset containing sampled population with ### 
###               indoor not home activities                 ###
### Outputs: Dataset containing the sampled PM2.5 estimate   ###
###          for indoor not home                             ###
################################################################
calculate_indoor <- function(dat, ambient, outvar) {
  # parameters (Normal from Burke et al. (2001) truncated)
  a <- rtruncnorm(n = nrow(dat), a = 0, mean = 6.467, sd = 2.1)
  b <- rtruncnorm(n = nrow(dat), a = 0, mean = 0.507, sd = 0.11)
  # Estimating ambient 
  dat[,outvar] <- rtruncnorm(n = 1, a = 0, mean = a + b * dat[,ambient, drop = TRUE], sd = 3.467)
  # Returning dataset
  return(dat)
}

################################################################
### calculate_transport - A function to calculate the trans- ###
### portation air pollution                                  ###
################################################################
### Inputs: dat - Dataset containing sampled population with ### 
###               transportation activities                  ###
### Outputs: Dataset containing the sampled PM2.5 estimate   ###
###          for indoor not home                             ###
################################################################
calculate_transport <- function(dat, ambient, outvar) {
  # parameters (Normal from Burke et al. (2001) truncated)
  a <- rtruncnorm(n = nrow(dat), a = 0, mean = 33, sd = 7.2)
  b <- rtruncnorm(n = nrow(dat), a = 0, mean = 0.26, sd = 0.14)
  # Estimating ambient 
  dat[,outvar] <- rtruncnorm(n = 1, a = 0, mean = a + b * dat[,ambient, drop = TRUE], sd = 12)
  # Returning dataset
  return(dat)
}

