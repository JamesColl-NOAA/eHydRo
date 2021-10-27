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
setwd("G:/Dropbox/root/projects/GID_Inland_Hydrofabric/eHydro/")
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
##############################################################################################################################
##############################################################################################################################



# Now, for each tile in desired AOI:
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# ----- preprocess terrain elevation -------------------------------------------------------------------------------
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
elevatr_wrapper_for_tile <- function(tile_id) {
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
  return(TRUE)
}
elevatr_wrapper_for_tile(180)

##############################################################################################################################
##############################################################################################################################




#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# ----- process eHydro data -------------------------------------------------------------------------------
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
aggregate_eHydro_footprints_within_AOI <- function(tile_feature,tile_feature_subset = NULL) {
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
tile_fishnet <- aggregate_eHydro_footprints_within_AOI(xx$survey_tiles[180,])

##############################################################################################################################
append_eHydro_files_to_AOI <- function(AOI_tiles) {
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
      # print('Survey already in scratch')
    } else {
      # Download and unzip
      httr::GET(survey_url, write_disk(file.path(scratchdir,file_name), overwrite=TRUE))
      dir.create(file.path(scratchdir,folder_name), showWarnings = FALSE)
      unzip(file.path(scratchdir,file_name), exdir = file.path(scratchdir, folder_name))
    }
    # Add paths to index
    fgdb <- file.path(scratchdir, folder_name, paste0(folder_name,'.gdb'))
    subset(ogrDrivers(), grepl("GDB", name))
    fc_list <- ogrListLayers(fgdb)
    
    AOI_tiles <- AOI_tiles %>%
      mutate(tin_path = ifelse((sourcedata==survey_url) & file.exists(file.path(scratchdir, folder_name)), 
                               file.path(scratchdir, folder_name, paste0(folder_name,"_tin")), 
                               .$tin_path)) %>%
      mutate(survey_path = ifelse((sourcedata==survey_url) & ("SurveyPoint" %in% fc_list), 
                                  fgdb, 
                                  .$survey_path)) %>%
      mutate(surveyHD_path = ifelse((sourcedata==survey_url) & ("SurveyPointHD" %in% fc_list),
                                    fgdb,
                                    .$surveyHD_path)) %>%
      mutate(plot_path = ifelse((sourcedata==survey_url & file.exists(file.path(scratchdir, folder_name))),
                                file.path(scratchdir, folder_name, paste0(folder_name,"_tin")),
                                .$plot_path))
    
  }
  
  step_run_time <- difftime(Sys.time(), step_start_time, units = "mins")
  print(paste0("-- Surveys downloaded and appended to tiles in ",round(step_run_time, digits = 2), " minutes"))
  
  return(AOI_tiles)
}

tile_fishnet <- append_eHydro_files_to_AOI(tile_fishnet)

##############################################################################################################################
mosaic_bathy_surface <- function(AOI_tiles_with_paths,tile_id) {
  
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
  do.call(file.remove, list(list.files(file.path(basedir,"survey",paste0('tile_',dst_folder),"tmp"), recursive = TRUE,  full.names = TRUE)))
  #if(dir_size(scratchdir)/10**9>scratch_limit) { do.call(file.remove, list(list.files(scratchdir, recursive = TRUE,  full.names = TRUE))) }
  
  distinct_surveys <- unique(AOI_tiles_with_paths$sourcedata)
  for (x in 1:length(distinct_surveys)) {
    print(paste('Processing survey',x,'of',length(distinct_surveys)))
    tmp_pts = data.frame()
    
    # load in points
    #if(tin) {  AOI_tiles_with_paths[AOI_tiles_with_paths$sourcedata==distinct_surveys[[5]],] %>% head(1) %>% pull(tin_path)
    #load tin
    #transform to xyz
    #} else {
    if(TRUE) {
      pts <- AOI_tiles_with_paths[AOI_tiles_with_paths$sourcedata==distinct_surveys[[x]],] %>% head(1) %>% pull(survey_path)
      HDpts <- AOI_tiles_with_paths[AOI_tiles_with_paths$sourcedata==distinct_surveys[[x]],] %>% head(1) %>% pull(surveyHD_path)
      if(is.character(pts)) {
        tryCatch({
          tmp_pts1 <- rgdal::readOGR(dsn=pts,layer="SurveyPoint",verbose = FALSE)
          
          this_crs <- tmp_pts1@proj4string 
          this_datum <- head(tmp_pts1,1) %>% pull(elevationDatum)
          this_datum_unit <- head(tmp_pts1,1) %>% pull(elevationUOM)
          if(this_datum_unit=="usSurveyFoot"){
            survey_in_unit="us_ft"
          } else if(this_datum_unit=="Foot") {
            survey_in_unit="ft"
          } else {
            survey_in_unit="m"
          }
          if(length(this_datum_unit)==1) {
            print('ALERT - Datum empty?')
            print(paste0("Survey:",pts))
            print(paste0("Datum value:",this_datum_unit))
          }
          # A better approach would involve lubridate::parse_date_time(head(tmp_pts1,1) %>% pull(SurveyDateStamp),orders=c("%Y/%m/%d %H:%M:%S+H","ymd","%Y%m%d %H%M%S", "ymd HM"))
          this_date_obj <- head(tmp_pts1,1) %>% pull(SurveyDateStamp)
          this_date_YYYY <- stringr::str_sub(this_date_obj, 1, 4)
          this_date_mm <- stringr::str_sub(this_date_obj, 6, 7)
          this_date_dd <- stringr::str_sub(this_date_obj, 9, 10)
          this_date_start <- lubridate::decimal_date(lubridate::ymd(paste0(this_date_YYYY,"-",this_date_mm,"-",this_date_dd)))
          this_date_now <- lubridate::decimal_date(Sys.time())
          
          # transform datum
          mean_X <- mean(tmp_pts1@coords[,1])
          mean_Y <- mean(tmp_pts1@coords[,2])
          center_point = data.frame(lon = mean_X, lat = mean_Y) %>% 
            sf::st_as_sf(coords = c("lon", "lat")) %>% 
            sf::st_set_crs(sf::st_crs(tmp_pts1)) %>% 
            sf::st_transform(sf::st_crs(6349))
          
          # determine and apply z transform
          datum_url <- paste0(
            "https://vdatum.noaa.gov/vdatumweb/api/convert?",
            "s_x=",as.character(sf::st_coordinates(center_point)[1,][1]),
            "&s_y=",as.character(sf::st_coordinates(center_point)[1,][2]),
            "&s_v_unit=",survey_in_unit,"&t_v_unit=m",
            "&s_h_frame=NAD83_2011&s_v_frame=",this_datum,
            "&t_h_frame=NAD83_2011&t_v_frame=NAVD88",
            "&epoch_in=",this_date_start,"&epoch_out=",this_date_now
          )
          print(datum_url)
          resp <- httr::GET(datum_url)
          jsonRespParsed<-content(resp,as="parsed")
          
          # transform z units as needed
          # if(this_datum_unit=="usSurveyFoot"){
          #   tmp_pts1$surveyPointElev <- tmp_pts1$surveyPointElev * 0.3048006096
          # }
          tmp_pts1$surveyPointElev <- -tmp_pts1$surveyPointElev + as.numeric(jsonRespParsed$t_z)
          
          # clean and readd coords in 5499
          tmp_pts1 <- sf::st_transform(sf::st_as_sf(tmp_pts1), sf::st_crs(6349))
          # tmp_pts1 <- sf::st_transform(sf::st_as_sf(tmp_pts1), sf::st_crs(4957))
          tmp_pts1 <- tmp_pts1 %>%
            dplyr::mutate(X = unlist(map(tmp_pts1$geometry,1)),
                          Y = unlist(map(tmp_pts1$geometry,2))) %>%
            dplyr::select(-c(xLocation,yLocation,Z_depth,Z_use,Z_label,SurveyDateStamp,SurveyId,surveyPointIDPK,sdsID,sdsFeatureName,sdsFeatureDescription,sdsMetadataID,elevationDatum,elevationUOM,mediaIDFK,sourceType,surveyJobIDFK,channelAreaIDFK,xLocationUOM,yLocationUOM,Z_use_tin)) %>%
            dplyr::rename(Z = surveyPointElev)
          
          tmp_pts1 <- data.frame(tmp_pts1$X,tmp_pts1$Y,tmp_pts1$Z)
          tmp_pts <- rbind(tmp_pts, tmp_pts1)
        }, error=function(e){
          if(httr::http_error(resp)) {
            print(paste('Error transforming datum - Request URL:', datum_url))
          }
        })
      }
      if(is.character(HDpts)) {
        tryCatch({
          # NEED TO CHECK FIELD NAMES
          tmp_pts1 <- rgdal::readOGR(dsn=pts,layer="SurveyPointHD",verbose = FALSE)
          
          this_crs <- tmp_pts1@proj4string 
          this_datum <- head(tmp_pts1,1) %>% pull(elevationDatum)
          this_datum_unit <- head(tmp_pts1,1) %>% pull(elevationUOM)
          if(this_datum_unit=="usSurveyFoot"){
            survey_in_unit="us_ft"
          } else if(this_datum_unit=="Foot") {
            survey_in_unit="ft"
          } else {
            survey_in_unit="m"
          }
          if(length(this_datum_unit)==1){
            print('ALERT - Datum empty?')
            print(paste0("Survey:",pts))
            print(paste0("Datum value:",this_datum_unit))
          }
          # A better approach would involve lubridate::parse_date_time(head(tmp_pts1,1) %>% pull(SurveyDateStamp),orders=c("%Y/%m/%d %H:%M:%S+H","ymd","%Y%m%d %H%M%S", "ymd HM"))
          this_date_obj <- head(tmp_pts1,1) %>% pull(SurveyDateStamp)
          this_date_YYYY <- stringr::str_sub(this_date_obj, 1, 4)
          this_date_mm <- stringr::str_sub(this_date_obj, 6, 7)
          this_date_dd <- stringr::str_sub(this_date_obj, 9, 10)
          this_date_start <- lubridate::decimal_date(lubridate::ymd(paste0(this_date_YYYY,"-",this_date_mm,"-",this_date_dd)))
          this_date_now <- lubridate::decimal_date(Sys.time())
          
          # transform datum
          mean_X <- mean(tmp_pts1@coords[,1])
          mean_Y <- mean(tmp_pts1@coords[,2])
          center_point = data.frame(lon = mean_X, lat = mean_Y) %>% 
            sf::st_as_sf(coords = c("lon", "lat")) %>% 
            sf::st_set_crs(sf::st_crs(tmp_pts1)) %>% 
            sf::st_transform(sf::st_crs(6349))
          
          # determine and apply z transform
          datum_url <- paste0(
            "https://vdatum.noaa.gov/vdatumweb/api/convert?",
            "s_x=",as.character(sf::st_coordinates(center_point)[1,][1]),
            "&s_y=",as.character(sf::st_coordinates(center_point)[1,][2]),
            "&s_v_unit=",survey_in_unit,"&t_v_unit=m",
            "&s_h_frame=NAD83_2011&s_v_frame=",this_datum,
            "&t_h_frame=NAD83_2011&t_v_frame=NAVD88",
            "&epoch_in=",this_date_start,"&epoch_out=",this_date_now
          )
          print(datum_url)
          resp <- httr::GET(datum_url)
          jsonRespParsed<-content(resp,as="parsed")
          
          # transform z units as needed
          # if(this_datum_unit=="usSurveyFoot"){
          #   tmp_pts1$surveyPointElev <- tmp_pts1$surveyPointElev * 0.3048006096
          # }
          tmp_pts1$surveyPointElev <- -tmp_pts1$surveyPointElev + as.numeric(jsonRespParsed$t_z)
          
          # clean and readd coords in 5499
          tmp_pts1 <- sf::st_transform(sf::st_as_sf(tmp_pts1), sf::st_crs(6349))
          # tmp_pts1 <- sf::st_transform(sf::st_as_sf(tmp_pts1), sf::st_crs(4957))
          tmp_pts1 <- tmp_pts1 %>%
            dplyr::mutate(X = unlist(map(tmp_pts1$geometry,1)),
                          Y = unlist(map(tmp_pts1$geometry,2))) %>%
            dplyr::select(-c(xLocation,yLocation,Z_depth,Z_use,Z_label,SurveyDateStamp,SurveyId,surveyPointIDPK,sdsID,sdsFeatureName,sdsFeatureDescription,sdsMetadataID,elevationDatum,elevationUOM,mediaIDFK,sourceType,surveyJobIDFK,channelAreaIDFK,xLocationUOM,yLocationUOM,Z_use_tin)) %>%
            dplyr::rename(Z = surveyPointElev)
          
          tmp_pts1 <- data.frame(tmp_pts1$X,tmp_pts1$Y,tmp_pts1$Z)
          tmp_pts <- rbind(tmp_pts, tmp_pts1)
        }, error=function(e){
          if(httr::http_error(resp)) {
            print(paste('Error transforming datum - Request URL:', datum_url))
          }
        })
      }
      print(paste0(nrow(tmp_pts), ' points accumulated'))
    }
    
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
    fit_TIN <- interp::interp( # using {interp}
      x = pts_proj$X,           # the function actually accepts coordinate vectors
      y = pts_proj$Y,
      z = pts_proj$Z,
      xo = grd_template$X,     # here we already define the target grid
      yo = grd_template$Y,
      output = "points"
    ) %>% bind_cols()
    
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
  }
  
  print('Mosaicing bathymetry to single layer')
  rast.list <- list.files(file.path(basedir,"survey",paste0('tile_',dst_folder),"tmp"), pattern="BATHY_proj*", full.names=TRUE, recursive=TRUE)
  rast.list <- lapply(1:length(rast.list), function(x) { raster::raster(rast.list[x]) })
  
  rast.list$fun <- mean
  rast.mosaic <- do.call(raster::mosaic,rast.list)
  
  raster::writeRaster(rast.mosaic,file.path(basedir,"survey",paste0('tile_',dst_folder),paste0("eHydro_bathy.tif")),format="GTiff")
  unlink(file.path(basedir,"survey",paste0('tile_',dst_folder),"tmp"), recursive=TRUE)
  
  step_run_time <- difftime(Sys.time(), step_start_time, units = "mins")
  print(paste0("Generating eHydro bathymetric surface took ",round(step_run_time, digits = 2), " minutes"))
  
  return(TRUE)
}

# we'd also test a reduce approach?
#reduce_bathy_surface <- function(AOI_tiles_with_paths,reducer = min) {
#)
mosaic_bathy_surface(tile_fishnet,180)

##############################################################################################################################
mosaic_bathy_terrain_surface <- function(tile_id) {
  step_start_time <- Sys.time()
  
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
  
  return(TRUE)
}
mosaic_bathy_terrain_surface(180)

##############################################################################################################################
# cross section helper code for geographic coordinate systems, fuck me
cut_cross_sections <- function(tile_id,cross_section_locations=NULL) {
  # Takes full terrain and cuts cross sections (There are edge cases all over the place in this one)
  
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
  }
  
  cross_section_vector <- list(length=length(stream_lines))
  
  # for each line
  for (i in 1:length(stream_lines)) {
    line <- stream_lines[i,]
    # Identify point of cross section crossing
    #cross_section_points <- rgeos::gInterpolate(as(sf_lines, "Spatial"), d=seq(0, 1, by = 0.2), normalized = TRUE) %>% 
    cross_section_points <- rgeos::gInterpolate(as(line, "Spatial"), d=0.5, normalized = TRUE) %>% 
      sf::st_as_sfc()
    distance_of_cross_section_points <- rgeos::gProject(as(line,"Spatial"), as(sf::st_cast(cross_section_points,"POINT"),"Spatial"), normalized=FALSE)
    distance_of_all_line_points <- rgeos::gProject(as(line,"Spatial"), as(sf::st_cast(line,"POINT"),"Spatial"), normalized=FALSE)
    
    # calculate bearing of points
    bearing_coord_index = match(distance_of_all_line_points[distance_of_all_line_points<=distance_of_cross_section_points][1],distance_of_all_line_points)
    bearing_point <- as(sf::st_cast(line,"POINT"),"Spatial")[bearing_coord_index,]
    bearing <- geosphere::bearing(sf::st_coordinates(cross_section_points), sf::st_coordinates(sf::st_as_sf(bearing_point)), a=6378137, f=1/298.25722210)

    # Throw points to left and right
    left_bank_bearing = bearing - 90
    if(left_bank_bearing<0) {
      left_bank_bearing <- 360 - abs(left_bank_bearing)
    } else if(left_bank_bearing>360) {
      left_bank_bearing <- left_bank_bearing - 360
    }  
    left_bank <- geosphere::destPoint(sf::st_coordinates(cross_section_points), left_bank_bearing, 400, a=6378137, f=1/298.25722210)
    
    right_bank_bearing = bearing + 90
    if(right_bank_bearing<0) {
      right_bank_bearing <- 360 - abs(right_bank_bearing)
    } else if(right_bank_bearing>360) {
      right_bank_bearing <- right_bank_bearing - 360
    }  
    right_bank <- geosphere::destPoint(sf::st_coordinates(cross_section_points), right_bank_bearing, 400, a=6378137, f=1/298.25722210)
    
    # Construct lines
    points_ready <- rbind(left_bank,right_bank)
    cross_section_vector[[i]] <- sf::st_sf(comid=line$ID, Slope=line$Slope,geometry=sf::st_sfc(sf::st_linestring(points_ready)))
  }
  
  # Add crs and flatten
  cross_sections = map(cross_section_vector, ~ st_set_crs(., sf::st_crs(6349)))
  cross_sections = do.call(rbind, cross_sections)
  
  # Densify lines
  little_lines <- sf::st_segmentize(cross_sections,2)
  extract_points <- sf::st_cast(little_lines,"POINT")
  mapview::mapview(extract_points)
  
  # Extract surface
  surface <- raster::raster(file.path(basedir,"survey",paste0('tile_',tile_id),paste0("merged_surface.tif")))
  extract_points$z = raster::extract(surface, extract_points,method='simple')
  extract_points$d_degrees = rgeos::gProject(sf::as_Spatial(cross_sections), sf::as_Spatial(extract_points), normalized=FALSE)
  
  step_run_time <- difftime(Sys.time(), step_start_time, units = "mins")
  print(paste0("Cross section database assembled in ",round(step_run_time, digits = 2), " minutes"))
  
  return(extract_points)
}
tile_cross_sections <- cut_cross_sections(180)

ggplot(data = tile_cross_sections[tile_cross_sections$comid==16878454,], aes(d_degrees, z, color = z))+
  geom_point()+
  theme_light()+
  scale_color_gradientn(colors = terrain.colors(10))+
  labs(x = "Distance along profile [degrees]", y = "Elevation [m]",
       color = "Elevation [m]")

##############################################################################################################################
wrangle_cross_sections_from_eHydro <- function(AOI_bb) {
  for (tile in AOI_bb){
    processing_tile_id <- NULL
    dir.create(file.path(basedir,"survey",paste0('tile_',tile)), showWarnings = FALSE)
    
    elevatr_wrapper_for_tile(tile)
    elemnts <- aggregate_eHydro_footprints_within_AOI(tile_feature,tile_feature_subset = NULL)
    elemnts <- append_eHydro_files_to_AOI(elemnts)
    mosaic_bathy_surface(elemnts,tile)  # Check for empty elemnts?
    mosaic_bathy_terrain_surface(tile)
    cut_cross_sections(tile)
  }
  
}
##############################################################################################################################
##############################################################################################################################




#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# ----- Automated workflow -------------------------------------------------------------------------------
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////

# Start by downloading surveys
eHydro_survey_url <- "https://opendata.arcgis.com/api/v3/datasets/80a394bae6b547f1b5788074261e11f1_0/downloads/data?format=shp&spatialRefId=4326"
if(file.exists(paste0(basedir, "/base_data/eHydro_Survey_Data.zip"))) {
  file.remove(paste0(basedir, "/base_data/eHydro_Survey_Data.zip"))
}
httr::GET(eHydro_survey_url, write_disk(paste0(basedir, "/base_data/eHydro_Survey_Data.zip")), overwrite=TRUE)
unzip(paste0(basedir, "/base_data/eHydro_Survey_Data.zip"), exdir = paste0(basedir, "/base_data/eHydro_Survey_Data"))

xx$eHydro_survey_footprints <- sf::read_sf(paste0(basedir, "/base_data/eHydro_Survey_Data/SurveyJob.shp")) %>%
  sf::st_transform(sf::st_crs(5070))
xx$survey_tiles <- sf::st_make_grid(xx$eHydro_survey_footprints, cellsize = 15000, what = "polygons", square = FALSE, flat_topped = TRUE) %>%
  sf::st_as_sf()
xx$survey_tiles <- xx$survey_tiles[xx$eHydro_survey_footprints, ]
xx$survey_tiles$tile_id <- 1:nrow(xx$survey_tiles)

map(wrangle_cross_sections_from_eHydro)












##############################################################################################################################

tinTest <- sf::st_read(file.path(scratchdir,"CL_47_BGR_20180816_CS","CL_47_BGR_20180816_CS_tin"))
return(as.matrix(cbind(dataset@nodes[,1:2], 
                       dataset@z))) 


##############################################################################################################################
##############################################################################################################################
