#####################
### Preliminaries ###
#####################
# Loading source code
library(here)
source(here("Code", "CaseStudy2", "0_Source.R"))

#############################
### Reading in shapefiles ###
#############################
# Reading in whole UK shapefiles
uk_full <- rgdal::readOGR(dsn = 'Data/CaseStudy2/Raw/Shapefiles', 
                          layer = 'gadm36_GBR_0')

# import shapefile, converte to longitude/latitude coordinates and organise data
ew_msoa <- rgdal::readOGR(dsn = "Data/CaseStudy2/Raw/Shapefiles",
                          layer = "Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries")

# Getting MSOA centroids
tmp <- as.data.frame(spTransform(gCentroid(ew_msoa, byid = TRUE), '+proj=longlat'))
names(tmp) <- c('long', 'lat')
ew_msoa@data <- cbind(ew_msoa@data, tmp)

# Getting relevant columns
ew_msoa@data <- ew_msoa@data[,c('msoa11cd', 'msoa11nm', 'long', 'lat')]
names(ew_msoa) <- c('area_id', 'area_name', 'cent_long', 'cent_lat')

# Getting hierarchy
ew_msoa$parent_area_name <- NA
for (i in 1:nrow(ew_msoa)){
  ew_msoa$parent_area_name[i] <- substr(ew_msoa$area_name[i], 1, nchar(ew_msoa$area_name[i]) - 4)
}

# Aggregating
ew_msoa_region <- unionSpatialPolygons(ew_msoa, IDs = ew_msoa$parent_area_name)

# Converting to SpatialPointsDataFrame
ew_msoa_region <- as(ew_msoa_region, "SpatialPolygonsDataFrame")

# Adding ID variable
for (i in 1:nrow(ew_msoa_region)){
  ew_msoa_region$dummy[i] <- ew_msoa_region@polygons[[i]]@ID
}

# Altering column names
names(ew_msoa_region) <- 'parent_area_name'

# Getting MSOA centroids
tmp <- as.data.frame(spTransform(gCentroid(ew_msoa_region, byid = TRUE), '+proj=longlat'))
names(tmp) <- c('_cent_long', 'cent_lat')
ew_msoa_region@data <- cbind(ew_msoa_region@data, tmp)

# Save shapefiles
save(uk_full, ew_msoa, ew_msoa_region, file = "Data/CaseStudy2/Processed/Shapefiles/shapefiles.RData")
