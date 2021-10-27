## install.packages("devtools")
# install.packages("tidyverse")
# install.packages("geosphere")
# install.packages("sf")
# install.packages("leaflet")
# install.packages("leafgl")
# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("shinyjs")
# install.packages("shinyalert")
# install.packages("shinycssloaders")
# install.packages("shinyWidgets")
# install.packages("shinybusy")
# install.packages("gdalUtilities")
# install.packages("lwgeom")
# install.packages("raster")
# install.packages("elevatr")
# install.packages("nhdplusTools")
# install.packages("dataRetrieval")
# install.packages("maptools")
# install.packages("mapview")

library(tidyverse)
library(geosphere)
library(sf)
library(sp)
library(httr)
library(raster)
library(gdalUtilities)
library(rgdal)
library(leaflet)
library(leafgl)
library(lubridate)
library(ggplot2)
library(scales)
library(elevatr)


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# ----- User inputs --------------------------------------------------------------------------------------------
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
setwd("G:/Dropbox/root/projects/GID_Inland_Hydrofabric/eHydro/")
basedir <- getwd()
scratchdir <- paste0(basedir,"/survey_tmp")
dir.create(scratchdir, showWarnings = FALSE)
do.call(file.remove, list(list.files(scratchdir, recursive = TRUE,  full.names = TRUE)))
xx <- list() 
##############################################################################################################################
##############################################################################################################################



#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# ----- Initial folder setup -----------------------------------------------------------------------------------
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
dir.create(paste0(basedir,"/base_data"), showWarnings = FALSE)
dir.create(paste0(basedir,"/base_data/eHydro_Survey_Data"), showWarnings = FALSE)
dir.create(paste0(basedir,"/base_data/coastal_model_domains"), showWarnings = FALSE)
dir.create(paste0(basedir,"/base_data/coastal_model_domain_pacific"), showWarnings = FALSE)
dir.create(paste0(basedir,"/base_data/coastal_model_domain_atlantic_gulf"), showWarnings = FALSE)
##############################################################################################################################
##############################################################################################################################



#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# ----- Base data download -------------------------------------------------------------------------------------
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# Start by downloading surveys
eHydro_survey_url <- "https://opendata.arcgis.com/api/v3/datasets/80a394bae6b547f1b5788074261e11f1_0/downloads/data?format=shp&spatialRefId=4326"
if(file.exists(paste0(basedir, "/base_data/eHydro_Survey_Data.zip"))) {
  file.remove(paste0(basedir, "/base_data/eHydro_Survey_Data.zip"))
}
httr::GET(eHydro_survey_url, write_disk(paste0(basedir, "/base_data/eHydro_Survey_Data.zip")), overwrite=TRUE)
unzip(paste0(basedir, "/base_data/eHydro_Survey_Data.zip"), exdir = paste0(basedir, "/base_data/eHydro_Survey_Data"))

# And the coastal domain
coastal_model_domains <- "placeholderURL"
httr::GET(eHydro_survey_url, write_disk(paste0(basedir, "/base_data/coastal_model_domains.zip")), overwrite=TRUE)
unzip(paste0(basedir, "/base_data/coastal_model_domains.zip"), exdir = paste0(basedir, "/base_data/coastal_model_domains"))
unzip(paste0(basedir, "/base_data/coastal_model_domains/coastal_model_domain_atlantic_gulf.zip"), exdir = paste0(basedir, "/base_data/coastal_model_domains/coastal_model_domain_atlantic_gulf"))
unzip(paste0(basedir, "/base_data/coastal_model_domains/coastal_model_domain_pacific"), exdir = paste0(basedir, "/base_data/coastal_model_domains/coastal_model_domain_pacific"))
##############################################################################################################################
##############################################################################################################################



#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# ----- Load data ----------------------------------------------------------------------------------------------
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
xx$coastal_model_domain_atlantic_gulf <- sf::read_sf(paste0(basedir, "/base_data/coastal_model_domains/Atlantic_GulfCoast_Model_Domain/Atlantic_GulfCoast_Model_Domain_Polygon"))
xx$coastal_model_domain_atlantic_gulf <- sf::st_set_crs(xx$coastal_model_domain_atlantic_gulf, 4326)  # Missing CRS :(
xx$coastal_model_domain_pacific <- sf::read_sf(paste0(basedir, "/base_data/coastal_model_domains/coastal_model_domain_pacific/West_Coast_Polygon/Westcoastpolygon.shp"))
xx$eHydro_survey_footprints <- sf::read_sf(paste0(basedir, "/base_data/eHydro_Survey_Data/SurveyJob.shp")) %>%
  sf::st_transform(sf::st_crs(5070))
##############################################################################################################################
##############################################################################################################################



#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# ----- Initial data exploration -------------------------------------------------------------------------------
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
leaflet() %>%
  addPolygons(data=xx$eHydro_survey_footprints, color="#444444", fillOpacity=1,weight=2)

# Quick data stats
colnames(xx$eHydro_survey_footprints)
nrow(xx$eHydro_survey_footprints)

# sort (and Filter) by date?
xx$eHydro_survey_footprints <- xx$eHydro_survey_footprints[order(xx$eHydro_survey_footprints$surveydate),]
#xx$eHydro_survey_footprints <- xx$eHydro_survey_footprints %>% filter(surveydate >= as.Date("2014-01-05"))

# Simple AOI filter
sf::sf_use_s2(FALSE)
AOI_subset <- st_bbox(c(xmin = -123.416, xmax = -116.455, ymax = 46.7135, ymin = 45.2175), crs = st_crs(4326)) %>% sf::st_as_sfc()
xx$eHydro_survey_footprints[AOI_subset,]

leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addPolygons(data=AOI_subset, color="#444444", fillOpacity=1,weight=2)

range(xx$eHydro_survey_footprints$surveydate)

# Proj filter
unique(xx$eHydro_survey_footprints$sourceproj)
better_proj_list = c("NAD_1983_StatePlane_Michigan_Central_FIPS_2112_Feet","NAD_1983_2011_StatePlane_New_York_East_FIPS_3101_Ft_US","NAD_1983_StatePlane_Louisiana_South_FIPS_1702_Feet",
                     "NAD_1983_StatePlane_West_Virginia_South_FIPS_4702_Feet","NAD_1983_StatePlane_Pennsylvania_South_FIPS_3702_Feet","NAD_1983_StatePlane_Kentucky_South_FIPS_1602_Feet",
                     "NAD_1983_StatePlane_Massachusetts_Mainland_FIPS_2001_Feet","NAD_1983_StatePlane_Maryland_FIPS_1900_Feet","NAD_1983_StatePlane_Wisconsin_South_FIPS_4803_Feet",
                     "NAD_1983_StatePlane_Michigan_South_FIPS_2113_Feet","NAD_1983_StatePlane_Wisconsin_Central_FIPS_4802_Feet","NAD_1983_StatePlane_New_York_Long_Island_FIPS_3104_Feet",
                     "NAD_1983_StatePlane_Virginia_South_FIPS_4502_Feet","NAD_1983_StatePlane_Michigan_Central_FIPS_2112_Feet","NAD_1983_StatePlane_Virginia_South_FIPS_4502_Feet",
                     "NAD_1983_StatePlane_New_York_Long_Island_FIPS_3104_Feet","NAD_1983_StatePlane_Wisconsin_Central_FIPS_4802_Feet","NAD_1983_StatePlane_Illinois_East_FIPS_1201_Feet",             
                     "NAD_1983_StatePlane_Hawaii_3_FIPS_5103_Feet","NAD_1983_StatePlane_Washington_South_FIPS_4602_Feet","NAD_1983_StatePlane_Alabama_West_FIPS_0102_Feet",
                     "NAD_1983_StatePlane_Washington_North_FIPS_4601_Feet","NAD_1983_StatePlane_Georgia_East_FIPS_1001_Feet","NAD_1983_StatePlane_Texas_South_Central_FIPS_4204_Feet",
                     "NAD_1983_StatePlane_Arkansas_South_FIPS_0302_Feet","NAD_1983_StatePlane_Hawaii_2_FIPS_5102_Feet","NAD_1983_StatePlane_South_Carolina_FIPS_3900_Feet_Intl","WGS_1984_UTM_Zone_55N_USft",
                     "NAD_1983_StatePlane_Oregon_North_FIPS_3601_Feet","NAD_1983_StatePlane_Tennessee_FIPS_4100_Feet","NAD_1983_StatePlane_Oregon_South_FIPS_3602_Feet",
                     "NAD_1983_StatePlane_Massachusetts_Island_FIPS_2002_Feet","NAD_1983_StatePlane_Hawaii_4_FIPS_5104_Feet","NAD_1983_StatePlane_Mississippi_West_FIPS_2302_Feet",
                     "NAD_1983_StatePlane_Minnesota_South_FIPS_2203_Feet","NAD_1983_StatePlane_Texas_South_FIPS_4205_Feet","NAD_1983_StatePlane_Florida_West_FIPS_0902_Feet",
                     "NAD_1983_StatePlane_Florida_North_FIPS_0903_Feet","NAD_1983_StatePlane_New_York_West_FIPS_3103_Feet","NAD_1983_StatePlane_Indiana_West_FIPS_1302_Feet",
                     "NAD_1983_StatePlane_Kentucky_North_FIPS_1601_Feet","NAD_1983_StatePlane_Iowa_North_FIPS_1401_Feet","NAD_1983_StatePlane_Delaware_FIPS_0700_Feet",
                     "NAD_1983_StatePlane_Alaska_10_FIPS_5010_Feet","NAD_1983_StatePlane_Mississippi_East_FIPS_2301_Feet","NAD_1983_StatePlane_California_V_FIPS_0405_Feet",
                     "NAD_1983_StatePlane_Arkansas_North_FIPS_0301_Feet","NAD_1983_StatePlane_Ohio_North_FIPS_3401_Feet","NAD_1983_StatePlane_Pennsylvania_North_FIPS_3701_Feet",
                     "NAD_1983_StatePlane_California_II_FIPS_0402_Feet","NAD_1983_StatePlane_California_III_FIPS_0403_Feet","NAD_1983_StatePlane_California_I_FIPS_0401_Feet","NAD_1983_BLM_Zone_16N_ftUS",
                     "NAD_1983_StatePlane_New_York_Central_FIPS_3102_Feet","NAD_1983_StatePlane_Oklahoma_North_FIPS_3501_Feet","NAD_1983_StatePlane_New_York_East_FIPS_3101_Feet",
                     "NAD_1983_StatePlane_California_VI_FIPS_0406_Feet","NAD_1983_StatePlane_California_IV_FIPS_0404_Feet","NAD_1983_StatePlane_New_Hampshire_FIPS_2800_Feet",
                     "NAD_1983_StatePlane_Alabama_East_FIPS_0101_Feet","NAD_1983_StatePlane_Alaska_7_FIPS_5007_Feet","NAD_1983_StatePlane_Alaska_8_FIPS_5008_Feet","NAD_1983_StatePlane_Hawaii_1_FIPS_5101_Feet",
                     "NAD_1983_StatePlane_Missouri_East_FIPS_2401_Feet","NAD_1983_StatePlane_Illinois_West_FIPS_1202_Feet","NAD_1983_StatePlane_Rhode_Island_FIPS_3800_Feet",
                     "NAD_1983_StatePlane_Alaska_4_FIPS_5004_Feet","NAD_1983_StatePlane_Michigan_North_FIPS_2111_Feet","NAD_1983_StatePlane_North_Carolina_FIPS_3200_Feet",
                     "NAD_1983_StatePlane_Virginia_North_FIPS_4501_Feet","NAD_1983_StatePlane_Maine_East_FIPS_1801_Feet")
subset(xx$eHydro_survey_footprints, !(sourceproj %in% better_proj_list)) %>%
  nrow()
  
xx$eHydro_survey_footprints$date <- as.Date(xx$eHydro_survey_footprints$surveydate)
xx$eHydro_survey_footprints$date_num <- as.numeric(xx$eHydro_survey_footprints$date)
##############################################################################################################################
##############################################################################################################################



#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# ----- Generate tile schema for processing -------------------------------------------------------------------------------
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
xx$survey_tiles <- sf::st_make_grid(xx$eHydro_survey_footprints, cellsize = 15000, what = "polygons", square = FALSE, flat_topped = TRUE) %>%
  sf::st_as_sf()
xx$survey_tiles <- xx$survey_tiles[xx$eHydro_survey_footprints, ]
mapview::mapview(xx$survey_tiles)
##############################################################################################################################
##############################################################################################################################



# Now, for each tile in desired AOI:
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# ----- preprocess terrain elevation -------------------------------------------------------------------------------
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# Download

test <- sf::st_transform(xx$survey_tiles[180,], sf::st_crs(4326))
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=test)
testelv <- elevatr::get_elev_raster(test, z = 14, clip = 'locations', override_size_check=TRUE)
# reproject

# mosaic?
##############################################################################################################################
##############################################################################################################################




#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# ----- process eHydro data -------------------------------------------------------------------------------
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# Sort by (area and?) date
xx$eHydro_survey_footprints <- xx$eHydro_survey_footprints[rev(order(xx$eHydro_survey_footprints$surveydate)),]

generate_eHydro_footprint_list <- function(tile_feature) {
  intersecting_footprints <- xx$eHydro_survey_footprints[tile_feature,]
  
  # rgeos error related to https://github.com/r-spatial/sf/issues/1668 :(
  #intersecting_footprints_poly <- sf::st_cast(intersecting_footprints, "POLYGON")
  #tile_survey_footprints <- sf::st_intersection(intersecting_footprints_poly)
  
  tile_survey_footprints <- sf::st_intersection(intersecting_footprints[1:5,])
  out = lapply(1:nrow(tile_survey_footprints), function(x){
    dplyr::slice_max(intersecting_footprints[tile_survey_footprints[x,],], surveydate) # There could be more than one, that will be fine?
  })
  elemnts_to_scrape <- dplyr::bind_rows(out)
  elemnts_to_scrape <- elemnts_to_scrape[!duplicated(elemnts_to_scrape[ , c("objectid")]), ]
  return(elemnts_to_scrape)
}

elemnts <- generate_eHydro_footprint_list(xx$survey_tiles[180,])
elemnts
mapview::mapview(elemnts)

##############################################################################################################################
##############################################################################################################################
intersecting_footprints <- xx$eHydro_survey_footprints[xx$survey_tiles[180,],]
tile_survey_footprints <- sf::st_intersection(intersecting_footprints[1:6,])
out = lapply(1:nrow(tile_survey_footprints), function(x){
  dplyr::slice_max(intersecting_footprints[tile_survey_footprints[x,],], surveydate) # There could be more than one, that will be fine?
})
tile_survey_footprints
tile_survey_footprints_polycast <- sf::st_cast(tile_survey_footprints, "POLYGON")
sf::write_sf(tile_survey_footprints_polycast, paste0(scratchdir,"/tmp_footprints.shp"), delete_layer = TRUE, quiet = TRUE)
sf::st_set_precision(tile_survey_footprints_polycast,0.1)
snap_test <- lwgeom::st_snap_to_grid(tile_survey_footprints_polycast, 5)
snap_test$geometry
sf::st_intersection(snap_test)
sf::st_union(tile_survey_footprints)
sf::st_intersects(snap_test)
##############################################################################################################################
##############################################################################################################################
intersecting_footprints <- xx$eHydro_survey_footprints[xx$survey_tiles[180,],]
tile_fishnet <- sf::st_make_grid(intersecting_footprints, cellsize = 50, what = "polygons", square = TRUE) %>%
  sf::st_as_sf()
tile_fishnet <- tile_fishnet %>% sf::st_filter(intersecting_footprints)
ints <- st_intersects(tile_fishnet, intersecting_footprints)
tile_fishnet$ints = lengths(ints)
tile_fishnet$dates = lengths(ints)

intersecting_footprints_nogeo <- st_drop_geometry(intersecting_footprints)

out = lapply(1:nrow(tile_fishnet), function(x){
  #arrange(intersecting_footprints_nogeo[ints[[x]],],surveydate,SHAPE_Area) %>% slice_head(1))$objectid
  #dplyr::slice_max(intersecting_footprints_nogeo[ints[[x]],], c(surveydate,SHAPE_Area), with_ties=FALSE)$objectid
  arrange(intersecting_footprints_nogeo[ints[[x]],],surveydate,SHAPE_Area) %>% slice_head(n=1) %>% pull(objectid)
})
tile_fishnet$obsid <- unlist(out)
mapview::mapview(tile_fishnet['obsid'])

unique(tile_fishnet$obsid)
unique(intersecting_footprints$objectid)
elemnts_to_scrape <- dplyr::bind_rows(out)
elemnts_to_scrape <- elemnts_to_scrape[!duplicated(elemnts_to_scrape[ , c("objectid")]), ]

mapview::mapview(tile_fishnet['ints'])

xx$survey_tiles <- xx$survey_tiles[xx$eHydro_survey_footprints, ]
dplyr::slice_max(intersecting_footprints_nogeo[ints[[4]],], surveydate, with_ties=FALSE)$objectid
##############################################################################################################################
##############################################################################################################################

create_eHydro_mosaic_from_list <- function(footprint_list) {
  xx$eHydro_survey_footprints <- footprint_list[rev(order(xx$eHydro_survey_footprints$surveydate)),]
  
}

# Flatten footprint polygons

# Now, for each polygon in desired AOI:
  # subset footprints to tile

  # Get unique areas
  tests <- st_intersection(sub, sub)
  
  # For each unique area, grab the first polygon it intersetcs
  xx$eHydro_survey_footprints[tests,][1]
  
  # Take full list and grab unique features

  # Download and unzip
  
  # TIN to points
  
  # reproject

  # point to raster

  # crop to footprint bounds

# Mosaic



#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# ----- Create final terrain -------------------------------------------------------------------------------
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# Merge terrain and eHydro



#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# ----- Cross section wrangle -------------------------------------------------------------------------------
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
FEMA_cross_sections_avalible = FALSE
if(FEMA_cross_sections_avalible) {
  # Load in files
} else {
  # Flatten footprints to lines
  
  # Perpendicular line spacing
}



#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# ----- Extract terrain -------------------------------------------------------------------------------
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////



#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# ----- Join cross sections to lines? -------------------------------------------------------------------------------
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////



#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# ----- transform and export database -------------------------------------------------------------------------------
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////



saveRDS(tile_feature,"tile_features.RDS")
saveRDS(xx$eHydro_survey_footprints,"eHydro_footprints.RDS")

EPSG:3857

# URL @ sourcedata

# data proj @ sourceproj
xx$eHydro_survey_footprints


