#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

# Setting working directory
setwd('~/Dropbox/Github/SPFFinalReport/')

# Loading source code
source('~/Dropbox/Github/SPFFinalReport/Code/CaseStudy2/0_Source.R')

#################################
### Preparing MSOA shapefiles ###
#################################
# Reading in whole UK shapefiles
uk_full <- st_read(dsn = 'Data/Raw/Shapefiles', 
                   layer = 'gadm36_GBR_0')

# import shapefile, converte to longitude/latitude coordinates and organise data
ew_msoa <- st_read(dsn = "Data/Raw/Shapefiles",
                   layer = "Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries")

# Getting MSOA centroids in long lat format
tmp <- ew_msoa %>%
  # Extract
  st_centroid(byid = TRUE) %>%
  # Transform
  st_transform('+proj=longlat') %>%
  # Extract coordinates
  st_coordinates() %>%
  # Converting to data frame
  as.data.frame() %>%
  # Renaming covariates
  dplyr::rename(long = X, lat = Y)

# Adding on long/lat to original MSOAs
ew_msoa <- cbind(ew_msoa, tmp)

# Getting relevant columns
ew_msoa <- ew_msoa %>%
  dplyr::rename(area_id = msoa11cd, area_name = msoa11nm, cent_long = long, cent_lat = lat)

# Adding on Local authority
ew_msoa <- ew_msoa %>%
  # Merging on parent_information
  left_join(
    # Read in data
    read_csv('Data/Raw/Shapefiles/area_hierarchy.csv') %>%
      # REmoving output area
      dplyr::select(-c(OA11CD)) %>%
      # Only keeping unique row
      unique()  %>%
      # Selecting relevant variables
      dplyr::select(area_id = MSOA11CD,
                    parent_area_name = LAD11NM,
                    parent_area_id = LAD11CD) %>%
      unique(),
    by = 'area_id') 


###############################
### Preparing LA shapefiles ###
###############################
# Aggregating to local authority
ew_msoa_region <- ew_msoa %>% 
  # Aggregating
  dplyr::group_by(parent_area_name) %>%
  dplyr::summarise() %>%
  dplyr::ungroup()  %>% 
  # Adding a unique idenitifier code
  left_join(
    # Read in data
    read_csv('Data/Raw/Shapefiles/area_hierarchy.csv') %>%
      # REmoving output area
      dplyr::select(-c(OA11CD)) %>%
      # Only keeping unique row
      unique()  %>%
      dplyr::select(parent_area_name = LAD11NM,
                    area_id = LAD11CD) %>%
      unique(),
    by = 'parent_area_name')%>%
  # Renaming column 
  dplyr::rename(area_name = parent_area_name)

# Getting LA centroids in long lat format
tmp <- ew_msoa_region %>%
  # Extract
  st_centroid(byid = TRUE) %>%
  # Transform
  st_transform('+proj=longlat') %>%
  # Extract coordinates
  st_coordinates() %>%
  # Converting to data frame
  as.data.frame() %>%
  # Renaming covariates
  dplyr::rename(long = X, lat = Y)

# Adding on long/lat to original MSOAs
ew_msoa_region <- cbind(ew_msoa_region, tmp)

######################
### Saving outputs ###
######################
# Save shapefiles
save(uk_full, ew_msoa, ew_msoa_region, file = "Data/Processed/Shapefiles/shapefiles.RData")
