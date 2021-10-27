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
# install.packages("adfExplorer")
# install.packages("interp")
## install.packages("gstat")
## install.packages("fields")
## install.packages("mgcv")
## install.packages("automap")
# devtools::install_github("hunzikp/velox")
# install.packages("RNetCDF")
# install.packages("ncdf4")

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
library(interp)
# library(velox)

# Quick helper functions
dir_size <- function(path, recursive = TRUE) {
  stopifnot(is.character(path))
  files <- list.files(path, full.names = T, recursive = recursive)
  vect_size <- sapply(files, function(x) file.size(x))
  size_files <- sum(vect_size)
  size_files
}
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# ----- User inputs --------------------------------------------------------------------------------------------
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
setwd("G:/Dropbox/root/projects/GID_Inland_Hydrofabric/eHydRo/")
basedir <- getwd()
scratchdir <- paste0(basedir,"/survey_tmp")
dir.create(scratchdir, showWarnings = FALSE)
scratch_limit <- 400
#do.call(file.remove, list(list.files(scratchdir, recursive = TRUE,  full.names = TRUE)))
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
  sf::st_transform(sf::st_crs(6349))
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
                     "NAD_1983_StatePlane_Alabama_East_FIPS_0101_Feet","NAD_1983_StatePlane_Alaska_7_FIPS_5007_Feet","NAD_19Lawrence, Kansas83_StatePlane_Alaska_8_FIPS_5008_Feet","NAD_1983_StatePlane_Hawaii_1_FIPS_5101_Feet",
                     "NAD_1983_StatePlane_Missouri_East_FIPS_2401_Feet","NAD_1983_StatePlane_Illinois_West_FIPS_1202_Feet","NAD_1983_StatePlane_Rhode_Island_FIPS_3800_Feet",
                     "NAD_1983_StatePlane_Alaska_4_FIPS_5004_Feet","NAD_1983_StatePlane_Michigan_North_FIPS_2111_Feet","NTampa, FloridaAD_1983_StatePlane_North_Carolina_FIPS_3200_Feet",
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
nwm_domain_x <- c(-168.5,-65.5)
nwm_domain_y <- c(71.365162,17.5)
nwm_domain_bbox = data.frame(lon = nwm_domain_x, lat = nwm_domain_y) %>% 
  sf::st_as_sf(coords = c("lon", "lat")) %>% 
  sf::st_set_crs(as.numeric(4326)) %>% 
  sf::st_transform(sf::st_crs(6349))
nwm_domain_bbox = sf::st_as_sfc(sf::st_bbox(nwm_domain_bbox))

xx$eHydro_survey_footprints <- sf::read_sf(paste0(basedir, "/base_data/eHydro_Survey_Data/SurveyJob.shp")) %>%
  sf::st_make_valid() %>%
  sf::st_transform(sf::st_crs(6349)) %>%
  sf::st_filter(nwm_domain_bbox) 

bbox_meters <- sf::st_as_sfc(sf::st_bbox(xx$eHydro_survey_footprints))
bbox_meters <- bbox_meters %>% 
  sf::st_transform(sf::st_crs(5070))

xx$survey_tiles <- sf::st_make_grid(bbox_meters, cellsize = 10000, what = "polygons", square = FALSE, flat_topped = TRUE) %>%
  sf::st_as_sf()
xx$survey_tiles <- sf::st_transform(xx$survey_tiles, sf::st_crs(6349))

xx$survey_tiles <- sf::st_make_valid(xx$survey_tiles)

xx$survey_tiles <- xx$survey_tiles %>% sf::st_filter(xx$eHydro_survey_footprints)
xx$survey_tiles$tile_id <- 1:nrow(xx$survey_tiles)

mapview::mapview(xx$survey_tiles)
sf::write_sf(xx$survey_tiles, paste0(basedir, "/survey/miss_tiles.shp"))
##############################################################################################################################
##############################################################################################################################



# Now, for each tile in desired AOI:
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# ----- preprocess terrain elevation -------------------------------------------------------------------------------
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
elevatr_wrapper_for_tile <- function(tile_id) {
  print("-- Generating terrain surface --")
  
  # Input parsing
  step_start_time <- Sys.time()
  if(is.data.frame(tile_id)) {
    AOI <- tile_id
    tile_id <- AOI$tile_id
  } else {
    AOI <- xx$survey_tiles[xx$survey_tiles$tile_id==tile_id,]
  }
  
  print(paste('Processing elevation for tile',tile_id))
  # small buffer around feature?
  
  elevatr_NED <- elevatr::get_elev_raster(AOI, z = 14, clip = 'locations', override_size_check=TRUE)
  
  print('reprojecting and saving data')
  raster::writeRaster(elevatr_NED,filename=file.path(basedir,"survey",paste0('tile_',tile_id),"NED_4326.tif"),bylayer=TRUE,format="GTiff")
  gdalUtilities::gdalwarp(srcfile = file.path(basedir,"survey",paste0('tile_',tile_id),"NED_4326.tif"), dstfile = file.path(basedir,"survey",paste0('tile_',tile_id),"NED.tif"), t_srs = sf::st_crs(6349), r = "bilinear")   

  #Toss tmp ned file.
  file.remove(file.path(basedir,"survey",paste0('tile_',tile_id),"NED_4326.tif"))

  step_run_time <- difftime(Sys.time(), step_start_time, units = "mins")
  print(paste0("NED processed in ",round(step_run_time, digits = 2), " minutes"))
  return()
}
# elevatr_wrapper_for_tile(277)

##############################################################################################################################
##############################################################################################################################




#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# ----- process eHydro data -------------------------------------------------------------------------------
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
aggregate_eHydro_footprints_within_AOI <- function(tile_feature,tile_feature_subset = NULL) {
  print("-- Appending surveys in 'first in time across space' --")
  
  # Input parsing
  step_start_time <- Sys.time()
  if(is.data.frame(tile_feature)) {
    tile_feature <- tile_feature
  } else {
    tile_feature <- xx$survey_tiles[xx$survey_tiles$tile_id==tile_feature,]
  }
  
  # Term for potential lines (should improve performance?)
  if(is.null(tile_feature_subset)) { tile_feature_subset <- tile_feature}
  if(sf::st_crs(tile_feature_subset) != sf::st_crs(5070)){
    tile_feature_subset <- sf::st_transform(tile_feature_subset, sf::st_crs(5070))
  }
  
  # Subset footprints and generate spatial index 
  print('Subsetting footprints')
  intersecting_footprints <- xx$eHydro_survey_footprints %>% sf::st_filter(tile_feature)
  intersecting_footprints_hardclip <- rgeos::gIntersection(spgeom1=as(tile_feature,'Spatial'),spgeom2=as(intersecting_footprints,'Spatial'), byid=TRUE,drop_lower_td=TRUE) %>% 
    sf::st_as_sf()
  intersecting_footprints_hardclip_proj <- sf::st_transform(intersecting_footprints_hardclip, sf::st_crs(5070))
  
  print('Generating fishnet tiles')
  tile_fishnet <- sf::st_make_grid(intersecting_footprints_hardclip_proj, cellsize = 10, what = "polygons", square = TRUE) %>%
    sf::st_as_sf() %>% 
    sf::st_filter(sf::st_as_sf(intersecting_footprints_hardclip_proj)) %>% 
    sf::st_filter(tile_feature_subset) %>% 
    sf::st_transform(sf::st_crs(6349))
  
  # Get list of all features that intersect cell, push into index, and drop geometry to save mem
  print('Intersecting footprints')
  tile_index_intersect <- sf::st_intersects(tile_fishnet, intersecting_footprints)
  tile_fishnet$index <- lengths(tile_index_intersect)
  intersecting_footprints_nogeo <- sf::st_drop_geometry(intersecting_footprints)
  
  # For each index, filter footprints by date and area and push into list
  print('aggregating surveys')
  out = lapply(1:nrow(tile_fishnet), function(x){
    arrange(intersecting_footprints_nogeo[tile_index_intersect[[x]],],surveydate,SHAPE_Area) %>% slice_head(n=1)
  })
  
  # Add output fields into index and return
  print('Generating outputs')
  # normally would be able to do this but intersect is picky :( out_df <- do.call(rbind.data.frame, out)
  out_df <- data.table::rbindlist(lapply(out, function(x) if(purrr::is_empty(x$objectid)) data.frame(objectid=NA,surveyjobi=NA,sdsid=NA,sdsfeature=NA,sdsmetadat=NA,surveytype=NA,channelare=NA,dateupload=as.Date('1990-01-01'),usacedistr=NA,surveydate=as.Date('1990-01-01'),surveyda_1=as.Date('1990-01-01'),sourcedata=NA,sourceproj=NA,mediaidfk=NA,projecteda=NA,sdsfeatu_1=NA,dateloaded=as.Date('1990-01-01'),datenotifi=as.Date('1990-01-01'),sourceda_1=NA,plotsheetl=NA,sourceagen=NA,globalid=NA,SHAPE_Leng=NA,SHAPE_Area=NA) else x))
  tile_fishnet$objectid <- out_df$objectid 
  tile_fishnet$surveydate <- out_df$surveydate
  tile_fishnet$sourcedata <- out_df$sourcedata
  tile_fishnet$sourceproj <- out_df$sourceproj
  tile_fishnet$plotsheetl <- out_df$plotsheetl
  tile_fishnet <- tile_fishnet[!is.na(tile_fishnet$sourcedata), ]
  
  step_run_time <- difftime(Sys.time(), step_start_time, units = "mins")
  print(paste0("Tile fishnet with 'first in time across space' assembled in ",round(step_run_time, digits = 2), " minutes"))
  
  return(tile_fishnet)
}

# we'd also test an average?
#crop_eHydro_footprints_within_AOI <- function(tile_feature,tile_feature_subset = NULL) {
#)
# id = 277
# tile_fishnet <- aggregate_eHydro_footprints_within_AOI(xx$survey_tiles[xx$survey_tiles$tile_id==id,])
# mapview::mapview(list(xx$survey_tiles[xx$survey_tiles$tile_id==id,],tile_fishnet,xx$eHydro_survey_footprints %>% sf::filter(tile_fishnet)))

##############################################################################################################################
append_eHydro_files_to_AOI <- function(AOI_tiles) {
  print("-- validating survey data --")
  # clean up scratch
  step_start_time <- Sys.time()
  #if(dir_size(scratchdir)/10**9>scratch_limit) { do.call(file.remove, list(list.files(scratchdir, recursive = TRUE,  full.names = TRUE))) }
  
  # add extra filepath fields
  AOI_tiles <- AOI_tiles %>%
    add_column(tin_path = NA) %>%
    add_column(survey_path = NA) %>%
    add_column(surveyHD_path = NA) %>%
    add_column(plot_path = NA)
  
  # Download and unzip all unique surveys
  out <- unique(AOI_tiles$sourcedata)
  for (x in 1:length(out)) {
    print(paste('Processing survey',x,'of',length(out)))
    survey_url <- out[[x]]
    file_name <- sub('.*\\/', '', survey_url)
    folder_name <- substr(file_name,1,nchar(file_name)-4)
    if(dir.exists(file.path(scratchdir, folder_name))) {
      print('Survey already in scratch')
    } else {
      # Download and unzip
      httr::GET(survey_url, write_disk(file.path(scratchdir,file_name), overwrite=TRUE))
      dir.create(file.path(scratchdir,folder_name), showWarnings = FALSE)
      tryCatch({
        unzip(file.path(scratchdir,file_name), exdir = file.path(scratchdir, folder_name))
      }, error = function(e) {
        print("ZIP corrupted")
        unlink(file.path(scratchdir, folder_name), recursive = TRUE) 
        return(NULL)
      })
      
    }
    # Add paths to index
    fgdb <- file.path(scratchdir, folder_name, paste0(folder_name,'.gdb'))
    subset(ogrDrivers(), grepl("GDB", name))
    
    # would be better to find next survey for this instead...
    tryCatch({
      fc_list <<- ogrListLayers(fgdb)
    }, error = function(e) {
      fc_list <<- "NA"
      unlink(file.path(scratchdir, folder_name), recursive = TRUE) 
      return(NULL)
    })
    
    AOI_tiles <- AOI_tiles %>%
      mutate(tin_path = ifelse(((sourcedata==survey_url) & file.exists(file.path(scratchdir, folder_name, paste0(folder_name,"_tin")))), 
                               file.path(scratchdir, folder_name, paste0(folder_name,"_tin")), 
                               .$tin_path)) %>%
      mutate(survey_path = ifelse(((sourcedata==survey_url) & ("SurveyPoint" %in% fc_list)), 
                                  fgdb, 
                                  .$survey_path)) %>%
      mutate(surveyHD_path = ifelse(((sourcedata==survey_url) & ("SurveyPointHD" %in% fc_list)),
                                    fgdb,
                                    .$surveyHD_path)) %>%
      mutate(plot_path = ifelse(((sourcedata==survey_url & file.exists(file.path(scratchdir, folder_name)))),
                                file.path(scratchdir, folder_name, paste0(folder_name,"_tin")),
                                .$plot_path))
    
  }
  
  step_run_time <- difftime(Sys.time(), step_start_time, units = "mins")
  print(paste0("-- Surveys downloaded and appended to tiles in ",round(step_run_time, digits = 2), " minutes"))
  
  return(AOI_tiles)
}
# tile_fishnet_survey <- append_eHydro_files_to_AOI(tile_fishnet)
# tile_fishnet_survey

##############################################################################################################################

mosaic_bathy_surface <- function(AOI_tiles_with_paths,tile_id) {
  
  normalize_dbase <- function(pts_frame, survey_name) {
    print('normalizeing data to datum and format')
    this_crs <- pts_frame@proj4string 
    this_datum <- head(pts_frame,1) %>% pull(elevationDatum)
    this_datum_unit <- head(pts_frame,1) %>% pull(elevationUOM)
    original_depths_line_up <- pts_frame[1,]$surveyPointElev==pts_frame[1,]$Z_depth
    
    # Crappy approach to dict
    if(is.na(this_datum_unit) || is.na(this_datum)) {
      if(survey_name %in% 
         c("https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_13_SWP_20150422_FORUM.ZIP",  #03-2011
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_12_SWP_20150129_FORUM.ZIP",  #03-2011
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_12_SWP_20150408.ZIP",         #03-2011
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_12_SWP_20150610.ZIP",        #03-2011
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_12_SWP_20150513.ZIP",        #03-2011
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_12_SWP_20150325_FORUM.ZIP",  #03-2011
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_12_SWP_20150226_FORUM.ZIP",  #03-2011
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_11_SWP_20150226_FORUM.ZIP",   #03-2011
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_07_SWP_20150218.ZIP",         #03-2011
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_07_SWP_20131016.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_05_SWP_20130723.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_05_SWP_20130716.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_05_SWP_20140612.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_05_SWP_20150425.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_07_SWP_20150330_FORUM.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_06_SWP_20150526.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_06_SWP_20150330_FORUM.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_06_SWP_20150521.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_06_SWP_20150502.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_06_SWP_20150319.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_06_SWP_20150512.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_06_SWP_20150428.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_05_SWP_20150122_PTA.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_05_SWP_20150419.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_05_SWP_20150601_FORUM.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_05_SWP_20150213_HDDA.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_05_SWP_20150416.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_05_SWP_20150130.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_05_SWP_20150513.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_05_SWP_20150421.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_05_SWP_20150330_FORUM.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_05_SWP_20150320.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_05_SWP_20150303_FORUM.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_04_SWP_20150601_FORUM.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_04_SWP_20150425.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_04_SWP_20150528.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_03_SWP_20150409.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/OV_02_TIG_20140618.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/OV_02_TIG_20150225.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/OV_02_TIG_20141216.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/OV_01_TIG_20150225.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/OV_01_TIG_20140618.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/OV_01_TIG_20141216.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_01_SWP_20150429.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/OV_02_BAP_20150513.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/OV_02_BAP_20150224.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/OV_02_BAP_20141002.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/EM_01_EFG_20140731.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/EM_05_BAR_20150226.ZIP"
         )) { 
        this_datum_unit <-"Foot"
        this_datum <-"MLG"
      }
    }
    
    # https://www.mvn.usace.army.mil/Portals/56/docs/Navigation/EDRs/EDR-OD-02_Calcasieu_MLG_to_MLLW___FINAL_2APR2018.pdf
    if(this_datum=="MLG") {
      this_datum <- "LMSL"
      this_date_now <- lubridate::decimal_date(lubridate::ymd(paste0(1944,"-",06,"-",15)))
      datum_delta <- -0.237744
    }
    
    # https://www.mvn.usace.army.mil/portals/56/docs/engineering/Geospatial/LWRP_White_Paper.pdf
    if(this_datum=="LWRP07") {
      if(survey_name %in% c(
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_97_VENX_20210721_CS_B2B.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_97_VENX_20210512_CS.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_95_FJ2_20160330.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_93_BVPX_20210512_CS.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_96_BTVX_20190118_CS.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_94_FJ1X_20210512_CS.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_95_FJ2_20190118_CS.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_95_FJ2_20180517_CS.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_95_FJ2X_20210512_CS.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_95_FJ2_20180725_CS.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_97_VENX_20200603_CS.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_95_FJ2_20160330.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_90_TB2X_20210513_CS.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_89_TB1X_20210513_CS.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_88_SX2X_20210513_CS.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_87_SX1X_20210513_CS.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_86_PS3X_20210514_CS.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_85_PS2X_20210514_CS.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_84_PS1X_20210514_CS.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_83_PH5X_20210514_CS.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_82_PH4X_20210519_CS.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_81_PH3X_20210519_CS.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_80_PH2X_20210519_CS.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_78_JR3X_20210519_CS.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_77_JR2X_20210518_CS.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_76_JR1X_20210518_CS.ZIP"
      )) {
        print('Low water datum skipped for now :( ')
        skip_flag <<- TRUE
      } else {
        print("Low water datum skipped for now :(  !!!!!Add survey to the list!!!!!!")
        skip_flag <<- TRUE
      }
    }
    
    if(this_datum_unit=="usSurveyFoot"){
      pts_frame$surveyPointElev <- pts_frame$surveyPointElev * (1200/3937)
    } else if(this_datum_unit=="Foot") {
      pts_frame$surveyPointElev <- pts_frame$surveyPointElev * 0.3048
    }
    
    if(nchar(this_datum)==0) {
      print('ALERT - Datum empty?')
      print(paste0("Survey:",pts))
      print(paste0("Datum value:",this_datum))
    }
    
    # A better approach would involve lubridate::parse_date_time(head(pts_frame,1) %>% pull(SurveyDateStamp),orders=c("%Y/%m/%d %H:%M:%S+H","ymd","%Y%m%d %H%M%S", "ymd HM"))
    this_date_obj <- head(pts_frame,1) %>% pull(SurveyDateStamp)
    this_date_YYYY <- stringr::str_sub(this_date_obj, 1, 4)
    this_date_mm <- stringr::str_sub(this_date_obj, 6, 7)
    this_date_dd <- stringr::str_sub(this_date_obj, 9, 10)
    this_date_start <- lubridate::decimal_date(lubridate::ymd(paste0(this_date_YYYY,"-",this_date_mm,"-",this_date_dd)))
    this_date_now <- lubridate::decimal_date(Sys.time())
    
    # transform datum
    mean_X <- mean(pts_frame@coords[,1])
    mean_Y <- mean(pts_frame@coords[,2])
    center_point = data.frame(lon = mean_X, lat = mean_Y) %>% 
      sf::st_as_sf(coords = c("lon", "lat")) %>% 
      sf::st_set_crs(sf::st_crs(pts_frame)) %>% 
      sf::st_transform(sf::st_crs(6349))
    
    # determine and apply z transform
    datum_url <- paste0(
      "https://vdatum.noaa.gov/vdatumweb/api/convert?",
      "s_x=",as.character(sf::st_coordinates(center_point)[1,][1]),
      "&s_y=",as.character(sf::st_coordinates(center_point)[1,][2]),
      "&s_v_unit=m&t_v_unit=m",
      "&s_h_frame=NAD83_2011&s_v_frame=",this_datum,
      "&t_h_frame=NAD83_2011&t_v_frame=NAVD88",
      "&epoch_in=",this_date_start,"&epoch_out=",this_date_now
    )
    resp <- httr::GET(datum_url)
    if(httr::http_error(resp)) {
      print('ALERT!!')
      print(paste('poorly formed url - Request URL:', datum_url))
      skip_flag <<- TRUE
    }
    jsonRespParsed <- httr::content(resp,as="parsed")
    
    # print(paste0("df:",pts_frame))
    # print(jsonRespParsed$t_z)
    # print(datum_delta)
    
    # how on gods green earth does anything ever line up?
    if(original_depths_line_up) {
      pts_frame$surveyPointElev <- -pts_frame$surveyPointElev - as.numeric(jsonRespParsed$t_z) + datum_delta
    } else {
      pts_frame$surveyPointElev <- pts_frame$surveyPointElev - as.numeric(jsonRespParsed$t_z) + datum_delta
    }
    
    # clean and readd coords in 5499
    pts_frame <- sf::st_transform(sf::st_as_sf(pts_frame), sf::st_crs(6349))
    # tmp_pts1 <- sf::st_transform(sf::st_as_sf(pts_frame), sf::st_crs(4957))
    pts_frame <- pts_frame %>%
      dplyr::mutate(X = unlist(map(pts_frame$geometry,1)),
                    Y = unlist(map(pts_frame$geometry,2))) %>%
      dplyr::select_if(!names(.) %in% c("Text_Angle","xLocation","yLocation","Z_depth","Z_use","Z_label","SurveyDateStamp","SurveyId","surveyPointIDPK","sdsID","sdsFeatureName","sdsFeatureDescription","sdsMetadataID","elevationDatum","elevationUOM","mediaIDFK","sourceType","surveyJobIDFK","channelAreaIDFK","xLocationUOM","yLocationUOM","Z_use_tin")) %>%
      dplyr::rename(Z = surveyPointElev)
    return(pts_frame)
  }
  
  print("-- moaicing bathymetery surface --")
  
  # AOI_tiles_with_paths<-tile_fishnet_survey
  # tile_id<-341
  
  # Input parsing
  step_start_time <- Sys.time()
  if(is.data.frame(tile_id)) {
    AOI <- tile_id
    dst_folder <- AOI$tile_id
  } else {
    AOI <- xx$survey_tiles[xx$survey_tiles$tile_id==tile_id,]
    dst_folder <- AOI$tile_id
  }
  aoi_bbox <- sf::st_bbox(sf::st_transform(AOI, sf::st_crs(5070)))
  
  dir.create(file.path(basedir,"survey",paste0('tile_',dst_folder),"tmp"), showWarnings = FALSE)
  #do.call(file.remove, list(list.files(file.path(basedir,"survey",paste0('tile_',dst_folder),"tmp"), recursive = TRUE,  full.names = TRUE)))
  #if(dir_size(scratchdir)/10**9>scratch_limit) { do.call(file.remove, list(list.files(scratchdir, recursive = TRUE,  full.names = TRUE))) }
  
  distinct_surveys <- unique(AOI_tiles_with_paths$sourcedata)
  for (x in 1:length(distinct_surveys)) {
    # x <- 117
    # If it's already processed
    skip_flag <<- FALSE
    
    if(file.exists(file.path(basedir,"survey",paste0('tile_',dst_folder),"tmp",paste0("BATHY_proj",x,".tif")))) {
      print(paste('Survey',x,'of',length(distinct_surveys),'already processed'))
      next
    }
    
    # If it's empty
    tmp <- AOI_tiles_with_paths[AOI_tiles_with_paths$sourcedata==distinct_surveys[[x]],] %>% head(1)
    if(is.na(tmp$survey_path) && is.na(tmp$surveyHD_path) && is.na(tmp$tin_path)) {
      print('Corrupt survey, skipping')
      next
    }
    
    print(paste0('Processing survey ',x,' of ',length(distinct_surveys),': "',distinct_surveys[[x]]))
    tmp_pts = data.frame()
    datum_delta = 0
    
    # load in points
    if(TRUE) {
      pts <- AOI_tiles_with_paths[AOI_tiles_with_paths$sourcedata==distinct_surveys[[x]],] %>% head(1) %>% pull(survey_path)
      HDpts <- AOI_tiles_with_paths[AOI_tiles_with_paths$sourcedata==distinct_surveys[[x]],] %>% head(1) %>% pull(surveyHD_path)
      if(is.character(pts)) {
        #print(paste0("processing ",pts))
        tmp_pts1 <- rgdal::readOGR(dsn=pts,layer="SurveyPoint",verbose = FALSE)
        
        tmp_pts1 = tryCatch({
          normalize_dbase(tmp_pts1,distinct_surveys[[x]])
        }, warning = function(w) {
          print(w)
        }, error = function(e) {
          print(e)
          skip_flag <<- TRUE
        })
        
        if(skip_flag) { next }
        
        tmp_pts1 <- data.frame(tmp_pts1$X,tmp_pts1$Y,tmp_pts1$Z)
        tmp_pts <- rbind(tmp_pts, tmp_pts1)
      }
      if(is.character(HDpts)) {
        #print(paste0("processing ",HDpts))
        # NEED TO CHECK FIELD NAMES
        tmp_pts1 <- rgdal::readOGR(dsn=pts,layer="SurveyPointHD",verbose = FALSE)
        
        tmp_pts1 = tryCatch({
          normalize_dbase(tmp_pts1,distinct_surveys[[x]])
        }, warning = function(w) {
          print(w)
        }, error = function(e) {
          print(e)
          skip_flag <<- TRUE
        })
        
        if(skip_flag) { next }
        
        tmp_pts1 <- data.frame(tmp_pts1$X,tmp_pts1$Y,tmp_pts1$Z)
        tmp_pts <- rbind(tmp_pts, tmp_pts1)
      }
      print(paste0(nrow(tmp_pts), ' points accumulated'))
    }
    
    if(nrow(tmp_pts) > 10) {
      
      pts <- sf::st_as_sf(data.frame(tmp_pts), coords = c("tmp_pts1.X", "tmp_pts1.Y", "tmp_pts1.Z"), crs = sf::st_crs(6349))
      pts_proj <- sf::st_transform(pts, sf::st_crs(5070))
      pts_proj_geometry <- sf::st_coordinates(pts_proj) %>% as.data.frame()
      pts_proj$X <- pts_proj_geometry$X
      pts_proj$Y <- pts_proj_geometry$Y
      pts_proj$Z <- pts_proj_geometry$Z
      
      # Trim points to AOI
      pts_proj <- pts_proj[!(pts_proj$X>aoi_bbox$xmax),]
      pts_proj <- pts_proj[!(pts_proj$X<aoi_bbox$xmin),]
      pts_proj <- pts_proj[!(pts_proj$Y<aoi_bbox$ymin),]
      pts_proj <- pts_proj[!(pts_proj$Y>aoi_bbox$ymax),]
      pts_proj <- pts_proj[!duplicated(pts_proj[,1:2]),]
      
      # Generate interp grid
      print('Generating raster frame for interpolation') 
      # grd_template <- expand.grid(
      #   X = seq(from = min(pts_proj$X), to = max(pts_proj$X), by = 5),
      #   Y = seq(from = min(pts_proj$Y), to = max(pts_proj$Y), by = 5) # 20 m resolution
      # )
      grd_template <- expand.grid(
        X = seq(from = aoi_bbox$xmin, to = aoi_bbox$xmax, by = 5),
        Y = seq(from = aoi_bbox$ymin, to = aoi_bbox$ymax, by = 5) # 20 m resolution
      )
      print('Generating TIN')
      tryCatch({
        fit_TIN <<- interp::interp( # using {interp}
          x = pts_proj$X,           # the function actually accepts coordinate vectors
          y = pts_proj$Y,
          z = pts_proj$Z,
          xo = grd_template$X,     # here we already define the target grid
          yo = grd_template$Y,
          duplicate="mean",
          output = "points"
        )
      }, error=function(e) {
        sp <- as(pts_proj,"Spatial")
        pts_proj <<- sp::remove.duplicates(sp, zero = 2, remove.second = TRUE, memcmp = TRUE) %>% sf::st_as_sf()
        pts_proj_geometry <<- sf::st_coordinates(pts_proj) %>% as.data.frame()
        pts_proj$X <<- pts_proj_geometry$X
        pts_proj$Y <<- pts_proj_geometry$Y
        pts_proj$Z <<- pts_proj_geometry$Z
        
        fit_TIN <<- interp::interp( # using {interp}
          x = pts_proj$X,           # the function actually accepts coordinate vectors
          y = pts_proj$Y,
          z = pts_proj$Z,
          xo = grd_template$X,     # here we already define the target grid
          yo = grd_template$Y,
          duplicate="mean",
          output = "points"
        )
      })
      fit_TIN <<- fit_TIN %>% bind_cols()
        
      print('Rasterizing TIN')
      interp_TIN <- raster::rasterFromXYZ(fit_TIN, crs = sf::st_crs(5070))
      raster::crs(interp_TIN) <- "EPSG:5070"
      
      print('Masking TIN')
      interp_TIN <- raster::setExtent(interp_TIN,aoi_bbox,TRUE,TRUE)
      survey_mask <- AOI_tiles_with_paths[AOI_tiles_with_paths$sourcedata==distinct_surveys[[x]],] %>% 
        sf::st_union() %>% 
        sf::st_as_sf() %>% 
        sf::st_transform(sf::st_crs(5070))
      raster::mask(interp_TIN, survey_mask, file.path(basedir,"survey",paste0('tile_',dst_folder),"tmp",paste0("BATHY_mask",x,".tif")))
      
      print('Writing bathy raster for survey')
      gdalUtilities::gdalwarp(file.path(basedir,"survey",paste0('tile_',dst_folder),"tmp",paste0("BATHY_mask",x,".tif")), dstfile = file.path(basedir,"survey",paste0('tile_',dst_folder),"tmp",paste0("BATHY_proj",x,".tif")), t_srs = sf::st_crs(6349), r = "bilinear") 
    } else {
      print(paste0("Error in survey:",distinct_surveys[[x]]))
    }
  }
  
  print('Mosaicing bathymetry to single layer')
  rast_list <- list.files(file.path(basedir,"survey",paste0('tile_',dst_folder),"tmp"), pattern="BATHY_proj*", full.names=TRUE, recursive=TRUE)
  rast_files <- lapply(1:length(rast_list), function(x) { raster::raster(rast_list[[x]]) })
  
  rast_files$fun <- mean
  rast_mosaic <- do.call(raster::mosaic,rast_files)
  raster::crs(rast_mosaic) <- "EPSG:6349"
  
  raster::writeRaster(rast_mosaic,file.path(basedir,"survey",paste0('tile_',dst_folder),paste0("eHydro_bathy.tif")),format="GTiff")
  #unlink(file.path(basedir,"survey",paste0('tile_',dst_folder),"tmp"), recursive=TRUE)
  
  step_run_time <- difftime(Sys.time(), step_start_time, units = "mins")
  print(paste0("Generating eHydro bathymetric surface took ",round(step_run_time, digits = 2), " minutes"))
  
  return()
}
mosaic_bathy_surface(tile_fishnet_survey,415)  
# we'd also test a reduce approach?
#reduce_bathy_surface <- function(AOI_tiles_with_paths,reducer = min) {
#)
# mosaic_bathy_surface(tile_fishnet_survey,id)

##############################################################################################################################
mosaic_bathy_terrain_surface <- function(tile_id) {
  step_start_time <- Sys.time()
  print("-- moaicing bathymetery and terrain surfaces --")
  
  print('Loading processed data and downsampling')
  elevation <- raster::raster(file.path(basedir,"survey",paste0('tile_',tile_id),"NED.tif"))
  bathy <- raster::raster(file.path(basedir,"survey",paste0('tile_',tile_id),paste0("eHydro_bathy.tif")))
  
  # resample to lowest resolution
  if(raster::res(bathy) < raster::res(elevation)){
    elevation <- raster::resample(elevation, bathy, method='bilinear')
  } else {
    bathy <- raster::resample(bathy, elevation, method='bilinear')
  }
  
  raster::merge(bathy,elevation, filename=file.path(basedir,"survey",paste0('tile_',tile_id),paste0("merged_surface.tif")))
  
  step_run_time <- difftime(Sys.time(), step_start_time, units = "mins")
  print(paste0("Full tile terrain and bathymetry created in ",round(step_run_time, digits = 2), " minutes"))
  
  return()
}
# mosaic_bathy_terrain_surface(277)

##############################################################################################################################
# cross section helper code for geographic coordinate systems, fuck me
NWM_channel_geom <- ncdf4::nc_open(file.path(basedir,"base_data","Route_Link.nc"))
cut_simple_cross_sections <- function(tile_id,cross_section_locations=NULL) {
  # Takes full terrain and cuts cross sections (There are edge cases all over the place in this one)
  tile_id
  cross_section_locations='miss'
  # Input parsing
  step_start_time <- Sys.time()
  if(is.data.frame(tile_id)) {
    AOI <- tile_id
    dst_folder <- AOI$tile_id
  } else {
    AOI <- xx$survey_tiles[xx$survey_tiles$tile_id==tile_id,]
    dst_folder <- AOI$tile_id
  }
  
  # load in line data
  if(cross_section_locations=='NWM' || is.null(cross_section_locations)){
    if(is.null(cross_section_locations)) {
      print('no lines prvided, defaulting to midpoint of NWM stream segments')
    }
    print('subsetting NWM lines to tile')
    stream_lines = sf::read_sf(file.path(basedir,"processed_data","nwm_streams","nwm_streams.shp")) %>% 
      # sf::st_set_crs(5070) %>%z
      sf::st_transform(sf::st_crs(6349)) %>% 
      sf::st_filter(AOI)
    #stream_lines = sf::read_sf(file.path(basedir,"processed_data","SurveyJob_toline.shp"))
  } else if(cross_section_locations=='miss') {
    print('subsetting miss lines to tile')
    stream_lines = sf::read_sf(file.path(basedir,"processed_data","miss_streams","miss_streams.shp")) %>% 
      sf::st_transform(sf::st_crs(6349)) %>% 
      sf::st_filter(AOI)
  }
  
  cross_section_vector <- list(length=nrow(stream_lines))
  
  print('generating cross section lines')
  # for each line
  for (i in 1:nrow(stream_lines)) {
    line <- stream_lines[i,]
    # Identify point of cross section crossing
    #cross_section_points <- rgeos::gInterpolate(as(sf_lines, "Spatial"), d=seq(0, 1, by = 0.2), normalized = TRUE) %>% 
    cross_section_points <- rgeos::gInterpolate(as(line, "Spatial"), d=0.5, normalized = TRUE) %>% 
      sf::st_as_sfc()
    #Warning message:In proj4string(spgeom) : CRS object has comment, which is lost in output
    
    distance_of_cross_section_points <- rgeos::gProject(as(line,"Spatial"), as(sf::st_cast(cross_section_points,"POINT"),"Spatial"), normalized=FALSE)
    distance_of_all_line_points <- rgeos::gProject(as(line,"Spatial"), as(sf::st_cast(line,"POINT"),"Spatial"), normalized=FALSE)
    #Warning message: In st_cast.sf(line, "POINT") :repeating attributes for all sub-geometries for which they may not be constant
    
    # calculate bearing of points
    bearing_coord_index = match(distance_of_all_line_points[distance_of_all_line_points<=distance_of_cross_section_points][1],distance_of_all_line_points)
    bearing_point <- as(sf::st_cast(line,"POINT"),"Spatial")[bearing_coord_index,]
    #Warning message:In st_cast.sf(line, "POINT") :repeating attributes for all sub-geometries for which they may not be constant
    bearing <- geosphere::bearing(sf::st_coordinates(cross_section_points), sf::st_coordinates(sf::st_as_sf(bearing_point)), a=6378137, f=1/298.25722210)
    
    # Throw points to left and right
    left_bank_bearing = bearing - 90
    if(left_bank_bearing<0) {
      left_bank_bearing <- 360 - abs(left_bank_bearing)
    } else if(left_bank_bearing>360) {
      left_bank_bearing <- left_bank_bearing - 360
    }  
    left_bank <- geosphere::destPoint(sf::st_coordinates(cross_section_points), left_bank_bearing, 2000, a=6378137, f=1/298.25722210)
    
    right_bank_bearing = bearing + 90
    if(right_bank_bearing<0) {
      right_bank_bearing <- 360 - abs(right_bank_bearing)
    } else if(right_bank_bearing>360) {
      right_bank_bearing <- right_bank_bearing - 360
    }  
    right_bank <- geosphere::destPoint(sf::st_coordinates(cross_section_points), right_bank_bearing, 2000, a=6378137, f=1/298.25722210)
    
    # Construct lines
    points_ready <- rbind(left_bank,right_bank)
    cross_section_vector[[i]] <- sf::st_sf(comid=line$ID, Slope=line$Slope,geometry=sf::st_sfc(sf::st_linestring(points_ready)))
  }
  
  print('extracting surface')
  # Add crs and flatten
  cross_sections = map(cross_section_vector, ~ st_set_crs(., sf::st_crs(6349)))
  cross_sections = do.call(rbind, cross_sections)
  
  # Densify lines
  cross_sections$line_distance <- geosphere::lengthLine(as(cross_sections,"Spatial"))
  little_lines <- sf::st_segmentize(cross_sections,2)
  extract_points <- sf::st_cast(little_lines,"POINT")
  #Warning message:In st_cast.sf(little_lines, "POINT") : repeating attributes for all sub-geometries for which they may not be constant
  #mapview::mapview(list(little_lines,extract_points))
  
  rgeos::gProject(as(cross_sections,"Spatial"), as(extract_points,"Spatial"), normalized=TRUE)
  
  # Extract surface
  surface <- raster::raster(file.path(basedir,"survey",paste0('tile_',tile_id),paste0("merged_surface.tif")))
  extract_points$z <-  raster::extract(surface, extract_points,method='simple')
  extract_points$relative_distance <- rgeos::gProject(sf::as_Spatial(cross_sections), sf::as_Spatial(extract_points), normalized=TRUE)
  extract_points$d <- extract_points$relative_distance * extract_points$line_distance - (0.5* extract_points$line_distance)
  # extract_points$comid
  #NWM_channel_geom <- ncdf4::nc_open(file.path(basedir,"base_data","Route_Link.nc"))
  #bottom_width <- ncdf4::ncvar_get(NWM_channel_geom, "BtmWdth")
  #side_slope <- ncdf4::ncvar_get(NWM_channel_geom, "ChSlp")
  # Elevation <- ncdf4::ncvar_get(NWM_channel_geom, "alt")
  # Length
  # So
  # NWM_channel_geom[22814597,]
  
  step_run_time <- difftime(Sys.time(), step_start_time, units = "mins")
  print(paste0("Cross section database assembled in ",round(step_run_time, digits = 2), " minutes"))
  
  return(extract_points)
}
# tile_cross_sections <- cut_simple_cross_sections(180)


# ggplot(data = tile_cross_sections[tile_cross_sections$comid==16878454,], aes(d_degrees, z, color = z))+
#   geom_point()+
#   theme_light()+
#   scale_color_gradientn(colors = terrain.colors(10))+
#   labs(x = "Distance along profile [degrees]", y = "Elevation [m]",
#        color = "Elevation [m]")

##############################################################################################################################
wrangle_cross_sections_from_eHydro <- function(tiles_in_bbox) {
  # Filter bounding box
  
  cross_section_vector <- list(length=nrow(tiles_in_bbox))
  
  for (i in 1:nrow(tiles_in_bbox)) {
    tile <- tiles_in_bbox[i,]
    processing_tile_id <- tile$tile_id
    dir.create(file.path(basedir,"survey",paste0('tile_',processing_tile_id)), showWarnings = FALSE)
    
    elevatr_wrapper_for_tile(tile)
    tile_fishnet <- aggregate_eHydro_footprints_within_AOI(processing_tile_id,tile_feature_subset = NULL)
    tile_fishnet <- append_eHydro_files_to_AOI(tile_fishnet)
    mosaic_bathy_surface(tile_fishnet,processing_tile_id)  # Check for empty elemnts? mosaic_bathy_surface(tile_fishnet,180)
    mosaic_bathy_terrain_surface(processing_tile_id)
    cross_section_vector[[i]] <- cut_cross_sections(processing_tile_id,cross_section_locations='miss')
  }
  
  cross_sections = do.call(rbind, cross_section_vector)
  return(cross_sections)
}

##############################################################################################################################
##############################################################################################################################
# xx$survey_tiles <- sf::st_read()
for (i in 1:nrow(xx$survey_tiles)) {
  
  tile <- xx$survey_tiles[i,]
  processing_tile_id <- tile$tile_id
  
  print(paste0('Processing tile ',i,' of ',nrow(xx$survey_tiles),": ",processing_tile_id))
  
  # If it's already processed
  if(file.exists(file.path(basedir,"survey",paste0('tile_',processing_tile_id),paste0("merged_surface.tif")))) {
    next
  }
  
  dir.create(file.path(basedir,"survey",paste0('tile_',processing_tile_id)), showWarnings = FALSE)
  
  if(!file.exists(file.path(basedir,"survey",paste0('tile_',processing_tile_id),paste0("NED.tif")))) {
    elevatr_wrapper_for_tile(tile)
  }
  
  tile_fishnet <- aggregate_eHydro_footprints_within_AOI(processing_tile_id,tile_feature_subset = NULL)
  tile_fishnet_survey <- append_eHydro_files_to_AOI(tile_fishnet)
  mosaic_bathy_surface(tile_fishnet_survey,processing_tile_id)  # Check for empty elemnts? mosaic_bathy_surface(tile_fishnet,180)
  mosaic_bathy_terrain_surface(processing_tile_id)
  #cross_section_vector[[i]] <- cut_cross_sections(processing_tile_id,cross_section_locations='miss')
}
mosaic_bathy_surface(tile_fishnet_survey,384)  
mosaic_bathy_terrain_surface(355)

export_dbase <- list(length=6)
for(i in 1:6){
  export_dbase[[i]] <- cut_simple_cross_sections(xx$survey_tiles[i,]$tile_id,cross_section_locations="miss")
}
sf::st_write(sf::st_as_sf(data.table::rbindlist(export_dbase)), file.path(basedir,"survey",'miss_x_sections.gpkg'),driver = "GPKG")

#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# ----- Automated workflow -------------------------------------------------------------------------------
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////

# Start by downloading surveys
eHydro_survey_url <- "https://opendata.arcgis.com/api/v3/datasets/80a394bae6b547f1b5788074261e11f1_0/downloads/data?format=shp&spatialRefId=4326"
if(file.exists(paste0(basedir, "/base_data/eHydro_Survey_Data.zip"))) {
  file.remove(paste0(basedir, "/base_data/eHydro_Survey_Data.zip"))
}
if(file.exists(paste0(basedir, "/base_data/eHydro_Survey_Data/SurveyJob.shp"))) {
  file.remove(paste0(basedir, "/base_data/eHydro_Survey_Data/SurveyJob.shp"))
  file.remove(paste0(basedir, "/base_data/eHydro_Survey_Data/SurveyJob.shx"))
  file.remove(paste0(basedir, "/base_data/eHydro_Survey_Data/SurveyJob.prj"))
  file.remove(paste0(basedir, "/base_data/eHydro_Survey_Data/SurveyJob.dbf"))
  file.remove(paste0(basedir, "/base_data/eHydro_Survey_Data/SurveyJob.cpg"))
}
httr::GET(eHydro_survey_url, write_disk(paste0(basedir, "/base_data/eHydro_Survey_Data.zip")), overwrite=TRUE)
unzip(paste0(basedir, "/base_data/eHydro_Survey_Data.zip"), exdir = paste0(basedir, "/base_data/eHydro_Survey_Data"))

# Create processing tiles
nwm_domain_x <- c(-168.5,-65.5)
nwm_domain_y <- c(71.365162,17.5)
nwm_domain_bbox = data.frame(lon = nwm_domain_x, lat = nwm_domain_y) %>% 
  sf::st_as_sf(coords = c("lon", "lat")) %>% 
  sf::st_set_crs(as.numeric(4326)) %>% 
  sf::st_transform(sf::st_crs(6349))
nwm_domain_bbox = sf::st_as_sfc(sf::st_bbox(nwm_domain_bbox))
xx$eHydro_survey_footprints <- sf::read_sf(paste0(basedir, "/base_data/eHydro_Survey_Data/SurveyJob.shp")) %>%
  sf::st_make_valid() %>%
  sf::st_transform(sf::st_crs(6349)) %>%
  sf::st_filter(nwm_domain_bbox) 
bbox_meters <- sf::st_as_sfc(sf::st_bbox(xx$eHydro_survey_footprints))
bbox_meters <- bbox_meters %>% 
  sf::st_transform(sf::st_crs(5070))
xx$survey_tiles <- sf::st_make_grid(bbox_meters, cellsize = 10000, what = "polygons", square = FALSE, flat_topped = TRUE) %>%
  sf::st_as_sf()
xx$survey_tiles <- sf::st_transform(xx$survey_tiles, sf::st_crs(6349))
xx$survey_tiles <- sf::st_make_valid(xx$survey_tiles)
xx$survey_tiles <- xx$survey_tiles %>% sf::st_filter(xx$eHydro_survey_footprints)
xx$survey_tiles$tile_id <- 1:nrow(xx$survey_tiles)

# xx$eHydro_survey_footprints <- sf::read_sf(paste0(basedir, "/base_data/eHydro_Survey_Data/SurveyJob.shp")) %>%
#   sf::st_transform(sf::st_crs(5070))
# xx$survey_tiles <- sf::st_make_grid(xx$eHydro_survey_footprints, cellsize = 15000, what = "polygons", square = FALSE, flat_topped = TRUE) %>%
#   sf::st_as_sf()
# xx$survey_tiles <- xx$survey_tiles[xx$eHydro_survey_footprints, ]
# xx$survey_tiles$tile_id <- 1:nrow(xx$survey_tiles)

# Find tiles that intersect the area we want
stream_lines = sf::read_sf(file.path(basedir,"processed_data","miss_streams","miss_streams.shp")) %>% 
  sf::st_transform(sf::st_crs(6349))
xx$survey_tiles <- xx$survey_tiles %>% 
  sf::st_filter(stream_lines)

xx$survey_tiles
mapview::mapview(list(xx$survey_tiles,stream_lines))

wrangle_cross_sections_from_eHydro(xx$survey_tiles)












##############################################################################################################################

tinTest <- sf::st_read(file.path(scratchdir,"CL_47_BGR_20180816_CS","CL_47_BGR_20180816_CS_tin"))
return(as.matrix(cbind(dataset@nodes[,1:2], 
                       dataset@z))) 


##############################################################################################################################
##############################################################################################################################
