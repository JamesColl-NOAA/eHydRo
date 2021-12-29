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
# install.packages("rgdal", dependencies = TRUE)
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
# install.packages("gdalUtils")
# install.packages("BiocManager")
# BiocManager::install("rhdf5")

library(tidyverse)
library(geosphere)
library(sf)
library(sp)
library(httr)
library(gdalUtilities)
library(rgdal)
library(leaflet)
library(leafgl)
library(lubridate)
library(elevatr)
library(interp)
library(raster)
library(rhdf5)
library(stringi)
library(stringr)

# library(velox)

##############################################################################################################################
##############################################################################################################################
# Proposed workflow:
# devtools::install_github("jmcoll/eHydRo")
# library(eHydRo)

##############################################################################################################################
##############################################################################################################################
#' @title Set a working directiory
#' @description Probably bad practice, but ehydro is a dynamic database so when providing reproducable science we can include a snapshot of the database to work from.
#' It can be downloaded from here: http://nco.sourceforge.net/#Source
#' @return a list containing the list of tiles 
#' @importFrom sys exec_internal
#' @export
# establish_survey_dir(my_location) {
establish_survey_dir = function(my_location) {
  xx <- list()

  if(!file.exists(file.path(my_location, "processing_tiles.shp"))) {
    print('-- No eHydro survey found, processing base data --')
    eHydro_survey_url <- "https://opendata.arcgis.com/api/v3/datasets/80a394bae6b547f1b5788074261e11f1_0/downloads/data?format=shp&spatialRefId=4326"
    date_stamp <- gsub("-","_",Sys.Date())
    snapshot_dir <- file.path(my_location,paste0("survey_",date_stamp))
    dir.create(snapshot_dir, showWarnings = FALSE)

    httr::GET(eHydro_survey_url, write_disk(file.path(snapshot_dir,"eHydro_Survey_Data.zip")), overwrite=TRUE)
    unzip(file.path(snapshot_dir,"eHydro_Survey_Data.zip"), exdir = snapshot_dir)

    # OCONUS
    # nwm_domain_x <- c(-168.5,-65.5)
    # nwm_domain_y <- c(71.365162,17.5)

    # CONUS
    nwm_domain_x <- c(-127,-65.5)
    nwm_domain_y <- c(51,17.5)

    nwm_domain_bbox = data.frame(lon = nwm_domain_x, lat = nwm_domain_y) %>%
      sf::st_as_sf(coords = c("lon", "lat")) %>%
      sf::st_set_crs(as.numeric(4326)) %>%
      sf::st_transform(sf::st_crs(6349))
    nwm_domain_bbox = sf::st_as_sfc(sf::st_bbox(nwm_domain_bbox))

    xx$eHydro_survey_footprints <- sf::read_sf(file.path(snapshot_dir, "SurveyJob.shp")) %>%
      sf::st_make_valid() %>%
      sf::st_transform(sf::st_crs(6349)) %>%
      sf::st_filter(nwm_domain_bbox)

    bbox_meters <- sf::st_as_sfc(sf::st_bbox(xx$eHydro_survey_footprints))
    bbox_meters <- bbox_meters %>%
      sf::st_transform(sf::st_crs(5070))

    xx$survey_tiles <- sf::st_make_grid(bbox_meters, cellsize = 10500, what = "polygons", square = FALSE, flat_topped = TRUE) %>%
      sf::st_as_sf()
    xx$survey_tiles <- sf::st_transform(xx$survey_tiles, sf::st_crs(6349))
    xx$survey_tiles <- sf::st_make_valid(xx$survey_tiles)
    xx$survey_tiles$tile_id <- 1:nrow(xx$survey_tiles)
    sf::st_write(xx$survey_tiles, file.path(snapshot_dir,"processing_tiles.shp"))

  } else {
    print('-- Existing survey in place, loading data --')
    xx$eHydro_survey_footprints <- sf::read_sf(file.path(my_location, "SurveyJob.shp")) %>%
      sf::st_make_valid() %>%
      sf::st_transform(sf::st_crs(6349))
    xx$survey_tiles <- sf::read_sf(file.path(my_location, "processing_tiles.shp"))

    # xx$NWM_channel_routelink <- ncdf4::nc_open(file.path(my_location,"RouteLink_CONUS.nc"))
    # xx$NWM_channels <- ncdf4::nc_open(file.path(my_location,"base_data","RouteLink_CONUS.nc"))
  }
  return(xx)
}
# xx = establish_survey_dir('G:/Dropbox/root/projects/GID_Inland_Hydrofabric/eHydRo/survey_2021_11_01')


##############################################################################################################################
##############################################################################################################################
#' @title elevatr AWS wrapper for tile
#' @description elevatr package wrapper for tile. Given a tile, return the NAD
#' It can be downloaded from here: http://nco.sourceforge.net/#Source
#' @return a boolean condition
#' @importFrom sys exec_internal
#' @examples
#' check_nco()
#' @export
# elevatr_wrapper_for_tile(survey_location, AOI or TILE_ID) {
elevation_wrapper_for_tile = function(survey_location, TILE_ID, elevation_source="AWS") {
  print("-- Generating terrain surface --")

  dir.create(file.path(survey_location,paste0('tile_',id)), showWarnings = FALSE)

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

  if(elevation_source=="AWS") {
      elevatr_NED <- elevatr::get_elev_raster(AOI, z = 14, clip = 'locations', override_size_check=TRUE)
  
      if(is.null(elevatr_NED)) {
        print('No NED within the tile')
        return(FALSE)
      }
  
    print('reprojecting and saving data')
    raster::writeRaster(elevatr_NED,filename=file.path(survey_location,paste0('tile_',tile_id),"NED_4326.tif"),bylayer=TRUE,format="GTiff")
    gdalUtilities::gdalwarp(srcfile = file.path(survey_location,paste0('tile_',tile_id),"NED_4326.tif"),
                            dstfile = file.path(survey_location,paste0('tile_',tile_id),"NED.tif"), t_srs = sf::st_crs(6349), r = "bilinear")
  
    #Toss tmp ned file.
    file.remove(file.path(survey_location,paste0('tile_',tile_id),"NED_4326.tif"))
  
    step_run_time <- difftime(Sys.time(), step_start_time, units = "mins")
    print(paste0("NED processed in ",round(step_run_time, digits = 2), " minutes"))
    return(TRUE)
  }
  return(FALSE)
}
elevatr_aws_wrapper_for_tile('G:/Dropbox/root/projects/GID_Inland_Hydrofabric/eHydRo/survey_2021_11_01',86647)
# breaks: 79647

# TO DO
##############################################################################################################################
##############################################################################################################################
#' @title elevatr AWS wrapper for tile
#' @description elevatr package wrapper for tile. Given a tile, return the NAD
#' It can be downloaded from here: http://nco.sourceforge.net/#Source
#' @return a boolean condition
#' @importFrom sys exec_internal
#' @examples
#' check_nco()
#' @export
# elevatr_wrapper_for_tile(survey_location, AOI or TILE_ID) {
elevatr_aws_wrapper_for_tile = function(survey_location, TILE_ID) {
  print("-- Generating terrain surface --")

  dir.create(file.path(survey_location,paste0('tile_',id)), showWarnings = FALSE)

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

  if(is.null(elevatr_NED)) {
    print('No NED within the tile')
    return(FALSE)
  }

  print('reprojecting and saving data')
  raster::writeRaster(elevatr_NED,filename=file.path(survey_location,paste0('tile_',tile_id),"NED_4326.tif"),bylayer=TRUE,format="GTiff")
  gdalUtilities::gdalwarp(srcfile = file.path(survey_location,paste0('tile_',tile_id),"NED_4326.tif"),
                          dstfile = file.path(survey_location,paste0('tile_',tile_id),"NED.tif"), t_srs = sf::st_crs(6349), r = "bilinear")

  #Toss tmp ned file.
  file.remove(file.path(survey_location,paste0('tile_',tile_id),"NED_4326.tif"))

  step_run_time <- difftime(Sys.time(), step_start_time, units = "mins")
  print(paste0("NED processed in ",round(step_run_time, digits = 2), " minutes"))
  return(TRUE)
}
elevatr_aws_wrapper_for_tile('G:/Dropbox/root/projects/GID_Inland_Hydrofabric/eHydRo/survey_2021_11_01',86647)

##############################################################################################################################
##############################################################################################################################
#' @title elevatr AWS wrapper for tile
#' @description elevatr package wrapper for tile. Given a tile, return the NAD
#' It can be downloaded from here: http://nco.sourceforge.net/#Source
#' @return a boolean condition
#' @importFrom sys exec_internal
#' @examples
#' check_nco()
#' @export
# elevatr_wrapper_for_tile(survey_location, AOI or TILE_ID) {
elevatr_aws_wrapper_for_tile = function(survey_location, TILE_ID) {
  print("-- Generating terrain surface --")

  dir.create(file.path(survey_location,paste0('tile_',id)), showWarnings = FALSE)

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

  if(is.null(elevatr_NED)) {
    print('No NED within the tile')
    return(FALSE)
  }

  print('reprojecting and saving data')
  raster::writeRaster(elevatr_NED,filename=file.path(survey_location,paste0('tile_',tile_id),"NED_4326.tif"),bylayer=TRUE,format="GTiff")
  gdalUtilities::gdalwarp(srcfile = file.path(survey_location,paste0('tile_',tile_id),"NED_4326.tif"),
                          dstfile = file.path(survey_location,paste0('tile_',tile_id),"NED.tif"), t_srs = sf::st_crs(6349), r = "bilinear")

  #Toss tmp ned file.
  file.remove(file.path(survey_location,paste0('tile_',tile_id),"NED_4326.tif"))

  step_run_time <- difftime(Sys.time(), step_start_time, units = "mins")
  print(paste0("NED processed in ",round(step_run_time, digits = 2), " minutes"))
  return(TRUE)
}
elevatr_aws_wrapper_for_tile('G:/Dropbox/root/projects/GID_Inland_Hydrofabric/eHydRo/survey_2021_11_01',86647)

##############################################################################################################################
##############################################################################################################################
#' @title Check to See if NCO is on the system
#' @description NCO is a fantastic open source tool for working with NetCDF files.
#' It can be downloaded from here: http://nco.sourceforge.net/#Source
#' @return a boolean condition
#' @importFrom sys exec_internal
#' @examples
#' check_nco()
#' @export
# aggregate_eHydro_footprints_within_AOI(survey_location, AOI or TILE_ID) {
aggregate_eHydro_footprints_within_AOI <- function(survey_location, AOI, TILE_ID,tile_feature_subset = NULL) {
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

  if(nrow(intersecting_footprints)==0) {
    print('No surveys in footprint')
    return(NULL)
  }

  intersecting_footprints_hardclip <- rgeos::gIntersection(spgeom1=as(tile_feature,'Spatial'),spgeom2=as(intersecting_footprints,'Spatial'), byid=TRUE,drop_lower_td=TRUE) %>%
    sf::st_as_sf()
  intersecting_footprints_hardclip_proj <- sf::st_transform(intersecting_footprints_hardclip, sf::st_crs(5070))

  print('Generating fishnet tiles')
  tile_fishnet <- sf::st_make_grid(intersecting_footprints_hardclip_proj, cellsize = 9, what = "polygons", square = TRUE) %>%
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
# tile_fishnet <- aggregate_eHydro_footprints_within_AOI(xx$survey_tiles[xx$survey_tiles$tile_id==id,])
# mapview::mapview(list(xx$survey_tiles[xx$survey_tiles$tile_id==id,],tile_fishnet,xx$eHydro_survey_footprints %>% sf::st_filter(tile_fishnet)))
# mapview::mapview(xx$survey_tiles[xx$survey_tiles$tile_id==id]+tile_fishnet)


##############################################################################################################################
##############################################################################################################################
#' @title Check to See if NCO is on the system
#' @description NCO is a fantastic open source tool for working with NetCDF files.
#' It can be downloaded from here: http://nco.sourceforge.net/#Source
#' @return a boolean condition
#' @importFrom sys exec_internal
#' @examples
#' check_nco()
#' @export
# append_eHydro_files_to_AOI(survey_location, AOI or TILE_ID) {
append_eHydro_files_to_AOI <- function(AOI_tiles) {
  if(is.null(AOI_tiles)) {
    return(NULL)
  }

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
##############################################################################################################################
#' @title Check to See if NCO is on the system
#' @description NCO is a fantastic open source tool for working with NetCDF files.
#' It can be downloaded from here: http://nco.sourceforge.net/#Source
#' @return a boolean condition
#' @importFrom sys exec_internal
#' @examples
#' check_nco()
#' @export
# process_survey_tiles(survey_location, AOI or TILE_ID) {
mosaic_bathy_surface <- function(AOI_tiles_with_paths,tile_id) {

  # AOI_tiles_with_paths <- tile_fishnet_survey
  # tile_id <- processing_tile_id

  normalize_dbase <- function(pts_frame, survey_name) {
    print('normalizeing data to datum and format')
    this_crs <- pts_frame@proj4string
    this_datum <- head(pts_frame,1) %>% pull(elevationDatum)
    this_datum_unit <- head(pts_frame,1) %>% pull(elevationUOM)
    original_depths_line_up <- pts_frame[1,]$surveyPointElev==pts_frame[1,]$Z_depth

    print(paste0("Datum:",this_datum))
    print(paste0("Datum:",this_datum_unit))

    if(this_datum=="NAVD_88"){
      this_datum <- gsub("_","",this_datum)
    }

    # Crappy approach to dict
    if(is.na(this_datum_unit) || is.na(this_datum)) {
      if(survey_name %in%
         c("https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_13_SWP_20150422_FORUM.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_12_SWP_20150129_FORUM.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_12_SWP_20150408.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_12_SWP_20150610.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_12_SWP_20150513.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_12_SWP_20150325_FORUM.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_12_SWP_20150226_FORUM.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_11_SWP_20150226_FORUM.ZIP",
           "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/SW_07_SWP_20150218.ZIP",
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
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_76_JR1X_20210518_CS.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_91_BU1_20210906_CS_5X5_POSTIDA.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_91_BU1X_20210513_CS.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_92_BU2_20210906_CS_5X5_POSTIDA.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVN/MD_92_BU2X_20210513_CS.ZIP",
        "https://ehydrotest.blob.core.usgovcloudapi.net/ehydro-surveys/CEMVM/LM_16_OVLX_20210314_CS_6104_6075_LACONIA_LWRP.ZIP"
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
    print(paste0("URL:",datum_url))
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

  if(is.null(AOI_tiles_with_paths)) {
    print('No bathy to burn in, just using NED')
    return(NULL)
  }

  # AOI_tiles_with_paths<-tile_fishnet_survey
  # tile_id<-88748

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

  dir.create(file.path(snapshot_dir,paste0('tile_',dst_folder),"tmp"), showWarnings = FALSE)
  #do.call(file.remove, list(list.files(file.path(basedir,"survey",paste0('tile_',dst_folder),"tmp"), recursive = TRUE,  full.names = TRUE)))
  #if(dir_size(scratchdir)/10**9>scratch_limit) { do.call(file.remove, list(list.files(scratchdir, recursive = TRUE,  full.names = TRUE))) }

  distinct_surveys <- unique(AOI_tiles_with_paths$sourcedata)
  for (x in 1:length(distinct_surveys)) {
    # x <-11
    # If it's already processed
    skip_flag <<- FALSE

    if(file.exists(file.path(snapshot_dir,paste0('tile_',dst_folder),"tmp",paste0("BATHY_proj",x,".tif")))) {
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
    } else {
      # This is where TIN would go
      tinTest <- sf::st_read(file.path(scratchdir,"CL_47_BGR_20180816_CS","CL_47_BGR_20180816_CS_tin"))
      #return(as.matrix(cbind(dataset@nodes[,1:2], dataset@z)))
    }

    if(nrow(tmp_pts) > 10) {

      valid_grid_cells <- AOI_tiles_with_paths[AOI_tiles_with_paths$sourcedata==distinct_surveys[[x]],]
      valid_grid_cells$rownames <- rownames(valid_grid_cells)
      drops <- c('surveydate','sourcedata','sourceproj','plotsheetl','tin_path','survey_path','surveyHD_path','plot_path')
      valid_grid_cells <- valid_grid_cells[,!(names(valid_grid_cells) %in% drops)]

      obs <- SpatialPointsDataFrame(coords = tmp_pts[,c(1,2)], data = tmp_pts, proj4string = sp::CRS("EPSG:6349"))
      thin_tin <- sp::over(sf::as_Spatial(valid_grid_cells),obs,fn = mean)
      thin_tin$rownames <- rownames(thin_tin)

      thin_tin <- left_join(valid_grid_cells,thin_tin,by = "rownames") %>%
        sf::st_cast("POINT",warn=FALSE)
      thin_tin$tmp_pts1.X <- unlist(map(thin_tin$x,1))
      thin_tin$tmp_pts1.Y <- unlist(map(thin_tin$x,2))
      thin_tin <- thin_tin[complete.cases(thin_tin$tmp_pts1.Z),]

      pts <- sf::st_as_sf(data.frame(thin_tin), coords = c("tmp_pts1.X", "tmp_pts1.Y", "tmp_pts1.Z"), crs = sf::st_crs(6349))
      pts_proj <- sf::st_transform(pts, sf::st_crs(5070))
      pts_proj_geometry <- sf::st_coordinates(pts_proj) %>% as.data.frame()
      pts_proj$X <- pts_proj_geometry$X
      pts_proj$Y <- pts_proj_geometry$Y
      pts_proj$Z <- unlist(map(pts$geometry,3))

      # Trim points to AOI
      pts_proj <- pts_proj[!(pts_proj$X>aoi_bbox$xmax),]
      pts_proj <- pts_proj[!(pts_proj$X<aoi_bbox$xmin),]
      pts_proj <- pts_proj[!(pts_proj$Y<aoi_bbox$ymin),]
      pts_proj <- pts_proj[!(pts_proj$Y>aoi_bbox$ymax),]
      pts_proj <- pts_proj[!duplicated(pts_proj[,1:2]),]

      if(nrow(pts_proj) == 0) {
        print(paste0("!!!**!!! Error in survey: ",distinct_surveys[[x]]))
        next
      }

      # Generate interp grid
      print('Generating raster frame for interpolation')
      # grd_template <- expand.grid(
      #   X = seq(from = min(pts_proj$X), to = max(pts_proj$X), by = 5),
      #   Y = seq(from = min(pts_proj$Y), to = max(pts_proj$Y), by = 5) # 20 m resolution
      # )
      grd_template <- expand.grid(
        X = seq(from = aoi_bbox$xmin, to = aoi_bbox$xmax, by = 3),
        Y = seq(from = aoi_bbox$ymin, to = aoi_bbox$ymax, by = 3) # 20 m resolution
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
      raster::mask(interp_TIN, survey_mask, file.path(snapshot_dir,paste0('tile_',dst_folder),"tmp",paste0("BATHY_mask",x,".tif")))

      print('Writing bathy raster for survey')
      gdalUtilities::gdalwarp(file.path(snapshot_dir,paste0('tile_',dst_folder),"tmp",paste0("BATHY_mask",x,".tif")), dstfile = file.path(snapshot_dir,paste0('tile_',dst_folder),"tmp",paste0("BATHY_proj",x,".tif")), t_srs = sf::st_crs(6349), r = "bilinear")
    } else {
      print(paste0("Error in survey:",distinct_surveys[[x]]))
    }
  }

  print('Mosaicing bathymetry to single layer')
  rast_list <- list.files(file.path(snapshot_dir,paste0('tile_',dst_folder),"tmp"), pattern="BATHY_proj*", full.names=TRUE, recursive=TRUE)
  rast_files <- lapply(1:length(rast_list), function(x) { raster::raster(rast_list[[x]]) })

  if(length(rast_list)==0) {
    print('No bathy to mosaic')
    return(NULL)
  } else if(length(rast_list)==1) {
    rast_mosaic <<- rast_files[[1]]
  } else {
    rast_files$fun <- mean
    rast_mosaic <<- do.call(raster::mosaic,rast_files)
  }

  raster::crs(rast_mosaic) <- "EPSG:6349"
  raster::writeRaster(rast_mosaic,file.path(snapshot_dir,paste0('tile_',dst_folder),paste0("eHydro_bathy.tif")),format="GTiff")

  #unlink(file.path(basedir,"survey",paste0('tile_',dst_folder),"tmp"), recursive=TRUE)

  step_run_time <- difftime(Sys.time(), step_start_time, units = "mins")
  print(paste0("Generating eHydro bathymetric surface took ",round(step_run_time, digits = 2), " minutes"))

  return(NULL)
}


##############################################################################################################################
##############################################################################################################################
#' @title Check to See if NCO is on the system
#' @description NCO is a fantastic open source tool for working with NetCDF files.
#' It can be downloaded from here: http://nco.sourceforge.net/#Source
#' @return a boolean condition
#' @importFrom sys exec_internal
#' @examples
#' check_nco()
#' @export
# process_survey_tiles(survey_location, AOI or TILE_ID) {
mosaic_bathy_terrain_surface <- function(tile_id) {
  step_start_time <- Sys.time()
  print("-- moaicing bathymetery and terrain surfaces --")

  if(is.data.frame(tile_id)) {
    tile_id <- tile_id$tile_id
  }

  NED_path <- file.path(snapshot_dir,paste0('tile_',tile_id),"NED.tif")
  bathy_path <- file.path(snapshot_dir,paste0('tile_',tile_id),paste0("eHydro_bathy.tif"))

  if(!file.exists(NED_path) | !file.exists(bathy_path)) {
    print('Tile missing surfaces')
    return(FALSE)
  }

  print('Loading processed data and downsampling')
  elevation <- raster::raster(NED_path)
  bathy <- raster::raster(bathy_path)

  # resample to lowest resolution
  if(raster::res(bathy) < raster::res(elevation)){
    elevation <- raster::resample(elevation, bathy, method='bilinear')
  } else {
    bathy <- raster::resample(bathy, elevation, method='bilinear')
  }

  raster::merge(bathy,elevation, filename=file.path(snapshot_dir,paste0('tile_',tile_id),paste0("merged_surface.tif")))

  step_run_time <- difftime(Sys.time(), step_start_time, units = "mins")
  print(paste0("Full tile terrain and bathymetry created in ",round(step_run_time, digits = 2), " minutes"))

  return(TRUE)
}


##############################################################################################################################
##############################################################################################################################
#' @title Check to See if NCO is on the system
#' @description NCO is a fantastic open source tool for working with NetCDF files.
#' It can be downloaded from here: http://nco.sourceforge.net/#Source
#' @return a boolean condition
#' @importFrom sys exec_internal
#' @examples
#' check_nco()
#' @export
# process_survey_tiles(survey_location, AOI or TILE_ID) {
mosaic_surfaces <- function(type_string) {
  if(type_string=='NED') {
    rast_list <- list.files(path = snapshot_dir, pattern = "NED.tif", full.names = TRUE, recursive = TRUE)
    dest_file <- "merged_ned.tif"
  } else if(type_string=='bathy') {
    rast_list <- list.files(path = snapshot_dir, pattern = "eHydro_bathy.tif", full.names = TRUE, recursive = TRUE)
    dest_file <- "merged_bathy.tif"
  } else if(type_string=='bathy_NED') {
    rast_list <- list.files(path = snapshot_dir, pattern = "merged_surface.tif", full.names = TRUE, recursive = TRUE)
    dest_file <- "final_surface.tif"
  }

  tmp_file <- tempfile(fileext = ".tif")
  sf::gdal_utils(util = "warp", source = rast_list, destination = tmp_file, options = c("-t_srs",as.character(raster::crs("EPSG:6349")),"-r", "bilinear"))

  export <- raster::raster(tmp_file)
  raster::crs(export) <- "EPSG:6349"

  raster::writeRaster(export,file.path(snapshot_dir,dest_file),format="GTiff")
  return(NULL)
}


##############################################################################################################################
##############################################################################################################################
#' @title Check to See if NCO is on the system
#' @description NCO is a fantastic open source tool for working with NetCDF files.
#' It can be downloaded from here: http://nco.sourceforge.net/#Source
#' @return a boolean condition
#' @importFrom sys exec_internal
#' @examples
#' check_nco()
#' @export
# process_survey_tiles(survey_location, AOI or TILE_ID) {
process_survey_tiles = function(survey_location, AOI or TILE_ID) {

}
process_survey_tiles(survey_location, AOI or TILE_ID)

##############################################################################################################################
##############################################################################################################################
#' @title Check to See if NCO is on the system
#' @description NCO is a fantastic open source tool for working with NetCDF files.
#' It can be downloaded from here: http://nco.sourceforge.net/#Source
#' @return a boolean condition
#' @importFrom sys exec_internal
#' @examples
#' check_nco()
#' @export
parse_HECRAS_G_files(survey_location, HEC_path) {
  survey_location <- 'G:/Dropbox/root/projects/GID_Inland_Hydrofabric/eHydRo/survey_2021_11_01'
  HEC_path <- NULL

  hec_geo_files <- data.frame('geo'=c('test'),'proj'=c('test'))
  i=2

  print('Processing HECRAS surveys')
  if(is.null(HEC_PATH)) {
    HEC_PATH <- survey_location
  }

  files <- list.files(path = HEC_PATH,full.names = T,recursive = T)
  for(file in files) {
    if((stringi::stri_sub(file,-3,-3) == 'g') | (stringi::stri_sub(file,-3,-3) == 'G')) {

      # edge case: more than one projection in a model...
      proj_files <- list.files(path = dirname(file),pattern = '*\\.prj',full.names = T,recursive = T)
      for(proj_file in proj_files){
        if(!stringi::stri_detect_fixed(stringi::stri_sub(readLines(file(proj_file,"r"),n=1),0,15),"=")){
          hec_geo_files[i,]$geo <- file
          hec_geo_files[i,]$proj <- proj_file
          i=i+1
        }
      }
    }
  }
  hec_geo_files <- hec_geo_files[-1,]
  hec_geo_files <- hec_geo_files[!duplicated(hec_geo_files[ , c("geo")]), ]

  # # smarter Error handling (what if this survey has already been run?)
  # for(entry in hec_geo_files)

  rownames(hec_geo_files) <- 1:nrow(hec_geo_files)

  # Edge case: Alert for models that don't have a proj...
  for(hec_geo in hec_geo_files) {
    # locate all cross section data
    proj_file <- hec_geo_files[1,]$geo
    g_file_full_text <- readLines(file(proj_file,"r"))

      # Transform line geom to lines
      # transform station/elevation to points
      # use station to place elevation along line
      # project line to 6349
    # Stick geom in same folder as "eHydRo_cross_sections.shp" (see smarter error handling above)
  }

}
parse_HECRAS_G_files('G:/Dropbox/root/projects/GID_Inland_Hydrofabric/eHydRo/survey_2021_11_01')

##############################################################################################################################
##############################################################################################################################
#' @title Check to See if NCO is on the system
#' @description NCO is a fantastic open source tool for working with NetCDF files.
#' It can be downloaded from here: http://nco.sourceforge.net/#Source
#' @return a boolean condition
#' @importFrom sys exec_internal
#' @examples
#' check_nco()
#' @export
merge_all_eHydRo_hecras_cross_sections(survey_location) {

}

##############################################################################################################################
##############################################################################################################################
#' @title Check to See if NCO is on the system
#' @description NCO is a fantastic open source tool for working with NetCDF files.
#' It can be downloaded from here: http://nco.sourceforge.net/#Source
#' @return a boolean condition
#' @importFrom sys exec_internal
#' @examples
#' check_nco()
#' @export
cut_cross_sections(survey_location, lines, export) {

}

##############################################################################################################################
##############################################################################################################################
#' @title Check to See if NCO is on the system
#' @description NCO is a fantastic open source tool for working with NetCDF files.
#' It can be downloaded from here: http://nco.sourceforge.net/#Source
#' @return a boolean condition
#' @importFrom sys exec_internal
#' @examples
#' check_nco()
#' @export
export_surveys = function(survey_location) {
  file <- file.path(snapshot_dir,paste0('eHydro_ned_cross_sections_',gsub("-","_",Sys.Date()),'.nc'))
  cross_section_points <- cross_section_points %>% sf::st_drop_geometry()
  cross_section_points <- export_dbase
  nc <- RNetCDF::create.nc(file)
  RNetCDF::dim.def.nc(nc, "xid", unlim=TRUE)

  RNetCDF::var.def.nc(nc, "xid", "NC_INT", "xid")
  RNetCDF::var.def.nc(nc, "xid_length", "NC_DOUBLE", 0)
  RNetCDF::var.def.nc(nc, "comid", "NC_INT", 0)
  RNetCDF::var.def.nc(nc, "comid_rel_dist_ds", "NC_DOUBLE", 0)
  RNetCDF::var.def.nc(nc, "xid_d", "NC_DOUBLE", 0)
  RNetCDF::var.def.nc(nc, "x", "NC_DOUBLE", 0)
  RNetCDF::var.def.nc(nc, "y", "NC_DOUBLE", 0)
  RNetCDF::var.def.nc(nc, "z", "NC_DOUBLE", 0)
  RNetCDF::var.def.nc(nc, "source", "NC_INT", 0)

  ##  Put some _FillValue attributes
  RNetCDF::att.put.nc(nc, "xid", "_FillValue", "NC_INT", -99999)
  RNetCDF::att.put.nc(nc, "xid_length", "_FillValue", "NC_DOUBLE", -99999)
  RNetCDF::att.put.nc(nc, "comid", "_FillValue", "NC_INT", -99999)
  RNetCDF::att.put.nc(nc, "comid_rel_dist_ds", "_FillValue", "NC_DOUBLE", -99999.9)
  RNetCDF::att.put.nc(nc, "xid_d", "_FillValue", "NC_DOUBLE", -99999.9)
  RNetCDF::att.put.nc(nc, "x", "_FillValue", "NC_DOUBLE", -99999.9)
  RNetCDF::att.put.nc(nc, "y", "_FillValue", "NC_DOUBLE", -99999.9)
  RNetCDF::att.put.nc(nc, "z", "_FillValue", "NC_DOUBLE", -99999.9)
  RNetCDF::att.put.nc(nc, "source", "_FillValue", "NC_INT", -99999.9)

  ##  Put all  the data:
  RNetCDF::var.put.nc(nc, "xid", cross_section_points$xid)
  RNetCDF::var.put.nc(nc, "xid_length", cross_section_points$xid_length)
  RNetCDF::var.put.nc(nc, "comid", cross_section_points$comid)
  RNetCDF::var.put.nc(nc, "comid_rel_dist_ds", cross_section_points$comid_rel_dist_ds)
  RNetCDF::var.put.nc(nc, "xid_d", cross_section_points$xid_d)
  RNetCDF::var.put.nc(nc, "x", cross_section_points$x)
  RNetCDF::var.put.nc(nc, "y", cross_section_points$y)
  RNetCDF::var.put.nc(nc, "z", cross_section_points$z)
  RNetCDF::var.put.nc(nc, "source", cross_section_points$source)

  RNetCDF::att.put.nc(nc, "NC_GLOBAL", "title", "NC_CHAR", "Natural cross section data for inland routing task")
  RNetCDF::att.put.nc(nc, "NC_GLOBAL", "date_scraped", "NC_CHAR", "eHydro data scraped on 2021-11-01")
  RNetCDF::att.put.nc(nc, "NC_GLOBAL", "date_generated", "NC_CHAR", paste("database generated on ",gsub("-","_",Sys.Date())))
  RNetCDF::att.put.nc(nc, "NC_GLOBAL", "code_repo", "NC_CHAR", 'https://github.com/JamesColl-NOAA/eHydRo')
  RNetCDF::att.put.nc(nc, "NC_GLOBAL", "contact", "NC_CHAR", "james.coll@noaa.gov")
  RNetCDF::att.put.nc(nc, "NC_GLOBAL", "projection", "NC_CHAR", "epsg:6349")
  RNetCDF::att.put.nc(nc, "xid", "title", "NC_CHAR", "cross section ID")
  RNetCDF::att.put.nc(nc, "xid", "interpretation", "NC_CHAR", "a unique cross section id (fid)")
  RNetCDF::att.put.nc(nc, "xid_length", "title", "NC_CHAR", "cross section ID length")
  RNetCDF::att.put.nc(nc, "xid_length", "interpretation", "NC_CHAR", "the total length (in meters) of a cross section")
  RNetCDF::att.put.nc(nc, "xid_length", "unit", "NC_CHAR", "meters")
  RNetCDF::att.put.nc(nc, "comid", "title", "NC_CHAR", "Spatially assosiated COMID")
  RNetCDF::att.put.nc(nc, "comid", "interpretation", "NC_CHAR", "the comid from the NHD that the cross section intersects: should join with the routelink link field")
  RNetCDF::att.put.nc(nc, "comid_rel_dist_ds", "title", "NC_CHAR", "the relative (0-1) distance from the start of the comid that the cross section crosses at")
  RNetCDF::att.put.nc(nc, "comid_rel_dist_ds", "interpretation", "NC_CHAR", "the relative (0-1) distance from the start of the comid that the cross section crosses at")
  RNetCDF::att.put.nc(nc, "comid_rel_dist_ds", "unit", "NC_CHAR", "percentage (0-1)")
  RNetCDF::att.put.nc(nc, "xid_d", "title", "NC_CHAR", "Cross section distance")
  RNetCDF::att.put.nc(nc, "xid_d", "interpretation", "NC_CHAR", "The distance (meters from the left-most point on the cross section) of the observation.  0 is centered on the intersection of the cross section and the NHD reach")
  RNetCDF::att.put.nc(nc, "xid_d", "unit", "NC_CHAR", "meters")
  RNetCDF::att.put.nc(nc, "x", "title", "NC_CHAR", "x")
  RNetCDF::att.put.nc(nc, "x", "interpretation", "NC_CHAR", "x coordinate (Longitude)")
  RNetCDF::att.put.nc(nc, "x", "unit", "NC_CHAR", "degree")
  RNetCDF::att.put.nc(nc, "x", "projection", "NC_CHAR", "epsg:6349")
  RNetCDF::att.put.nc(nc, "y", "title", "NC_CHAR", "y")
  RNetCDF::att.put.nc(nc, "y", "interpretation", "NC_CHAR", "y coordinate (Latitude)")
  RNetCDF::att.put.nc(nc, "y", "unit", "NC_CHAR", "degree")
  RNetCDF::att.put.nc(nc, "y", "projection", "NC_CHAR", "epsg:6349")
  RNetCDF::att.put.nc(nc, "z", "title", "NC_CHAR", "z")
  RNetCDF::att.put.nc(nc, "z", "interpretation", "NC_CHAR", "vertical elevation (meters, positive up)")
  RNetCDF::att.put.nc(nc, "z", "unit", "NC_CHAR", "meters")
  RNetCDF::att.put.nc(nc, "z", "projection", "NC_CHAR", "epsg:6349")
  RNetCDF::att.put.nc(nc, "source", "title", "NC_CHAR", "source")
  RNetCDF::att.put.nc(nc, "source", "interpretation", "NC_CHAR", "The source of the data: 1=eHydro, 2=CWMS. 3=RFC HEC-RAS Model")

  RNetCDF::close.nc(nc)
  unlink(nc)
}
export_surveys

##############################################################################################################################
##############################################################################################################################






#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# ----- Merge CWMS data -------------------------------------------------------------------------------
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
generate_merged_CWMS_survey <- function() {
  print('mergeing cwms data')
  step_start_time <- Sys.time()
  cwms_survey_files <- list.files(path = file.path(snapshot_dir,'cwms','tmp'),pattern = "z.shp", full.names = TRUE, recursive = TRUE)
  cwms_survey = list(length=nrow(cwms_survey_files))

  fn_open_shapefiles <- function(file_path) { return( sf::read_sf(file_path) )}
  cwms_survey <- lapply(cwms_survey_files,fn_open_shapefiles)
  cwms_cross_sections <- do.call(rbind, cwms_survey)
  cwms_cross_sections <- sf::st_transform(cwms_cross_sections, sf::st_crs(6349))
  step_run_time <- difftime(Sys.time(), step_start_time, units = "mins")
  print(paste0("cwms data extracted, merged, and projected in ",round(step_run_time, digits = 2), " minutes"))
  return(cwms_cross_sections)
}
# cwms_cross_section_data <- generate_merged_CWMS_survey()
##############################################################################################################################
##############################################################################################################################



#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# ----- Cut cross sections -------------------------------------------------------------------------------
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# older and trash?
cut_simple_cross_sections <- function(tile_id,cross_section_locations=NULL) {
  # Takes full terrain and cuts cross sections (There are edge cases all over the place in this one)
  tile_id = 92081
  cross_section_locations="miss"

  # Input parsing
  step_start_time <- Sys.time()
  if(is.data.frame(tile_id)) {
    AOI <- tile_id
    dst_folder <- AOI$tile_id
    tile_id <- AOI$tile_id
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

  print(paste0("number of streams:",nrow(stream_lines)))
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
    bearing_coord_index = match(distance_of_all_line_points[distance_of_all_line_points<distance_of_cross_section_points][1],distance_of_all_line_points)
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
    left_bank <- geosphere::destPoint(sf::st_coordinates(cross_section_points), left_bank_bearing, 610, a=6378137, f=1/298.25722210)

    right_bank_bearing = bearing + 90
    if(right_bank_bearing<0) {
      right_bank_bearing <- 360 - abs(right_bank_bearing)
    } else if(right_bank_bearing>360) {
      right_bank_bearing <- right_bank_bearing - 360
    }
    right_bank <- geosphere::destPoint(sf::st_coordinates(cross_section_points), right_bank_bearing, 610, a=6378137, f=1/298.25722210)

    # Construct lines
    points_ready <- rbind(left_bank,right_bank)
    cross_section_vector[[i]] <- sf::st_sf(comid=line$ID,
                                           # Slope=line$Slope,
                                           geometry=sf::st_sfc(sf::st_linestring(points_ready)))
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
  #rgeos::gProject(as(cross_sections,"Spatial"), as(extract_points,"Spatial"), normalized=TRUE)

  # Extract surface
  surface <- raster::raster(file.path(snapshot_dir,"ehydro_ned_surface.tif"))

  extract_points$z <-  raster::extract(surface, extract_points,method='simple')
  # extract_points$z <-  raster::extract(surface, cross_sections,method='bilinear')
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

# Also need a custom cross section approach?
#reduce_bathy_surface <- function(AOI_tiles_with_paths,reducer = min) {
#)

pull_cross_sections_from_cwms_by_comid <- function(list_of_comids) {
  # list_of_comids <- cwms_enabled_coms
  print('Generating CWMS cross sections')
  step_start_time <- Sys.time()

  cwms_cross_section_data <- generate_merged_CWMS_survey()

  comid_midpoints <- list(length=nrow(list_of_comids))
  for (i in 1:nrow(list_of_comids)) {
    comid_midpoints[[i]] <- rgeos::gInterpolate(as(list_of_comids[i,], "Spatial"), d=0.5, normalized = TRUE) %>%
      sf::st_as_sf()
  }
  comid_midpoints <- sf::st_as_sf(data.table::rbindlist(comid_midpoints)) %>%
    sf::st_set_crs(sf::st_crs(6349))
  cwms_index <- sf::st_nearest_feature(comid_midpoints, cwms_cross_section_data)
  cwms_cross_sections <- dplyr::slice(cwms_cross_section_data,cwms_index)
  cwms_cross_sections$comid <- list_of_comids$ID

  cwms_extract_point_list <- list(length=nrow(cwms_cross_sections))
  for (i in 1:nrow(cwms_cross_sections)) {
    this_cross_section_line <- cwms_cross_sections[i,]

    this_cross_section_line$xid <- j+i
    this_cross_section_line$xid_length <- as.numeric(sf::st_length(sf::st_zm(this_cross_section_line, drop = TRUE, what = "ZM")))

    cross_section_midpoint <- sf::st_intersection(this_cross_section_line, stream_lines[stream_lines$ID==this_cross_section_line$comid,])
    if(nrow(cross_section_midpoint)==0) {
      print('CWIMS cross sections do not intersect here')
      next
    }
    this_cross_section_line$comid_rel_dist_ds <- rgeos::gProject(as(stream_lines[stream_lines$ID==this_cross_section_line$comid,],"Spatial"), as(sf::st_cast(cross_section_midpoint,"POINT"),"Spatial"), normalized=TRUE)[[1]]

    cwms_extract_point <- sf::st_cast(this_cross_section_line,"POINT")

    relative_distance_along_line_to_midpoint <- rgeos::gProject(as(sf::st_zm(this_cross_section_line, drop = TRUE, what = "ZM"),"Spatial"), as(sf::st_cast(cross_section_midpoint,"POINT"),"Spatial"), normalized=TRUE)
    cwms_extract_point$xid_rel_dist <- rgeos::gProject(as(sf::st_zm(this_cross_section_line, drop = TRUE, what = "ZM"),"Spatial"), as(sf::st_cast(sf::st_zm(this_cross_section_line, drop = TRUE, what = "ZM"),"POINT"),"Spatial"), normalized=TRUE)

    cwms_extract_point$xid_d <- (cwms_extract_point$xid_length * cwms_extract_point$xid_rel_dist) - (relative_distance_along_line_to_midpoint * cwms_extract_point$xid_length)
    cwms_extract_point_list[[i]] <- cwms_extract_point
  }
  print('merging cwms points')
  # cwms_extract_point_list = do.call(rbind, cwms_extract_point_list)
  cwms_extract_point_list = data.table::rbindlist(cwms_extract_point_list)
  return(cwms_extract_point_list)
}

cut_simple_cross_sections_from_eHydro <- function(ras_path,cross_section_locations=NULL) {

  # Input parsing
  step_start_time <- Sys.time()
  # if(is.data.frame(tile_id)) {
  #   AOI <- tile_id
  #   dst_folder <- AOI$tile_id
  #   tile_id <- AOI$tile_id
  # } else {
  #   AOI <- xx$survey_tiles[xx$survey_tiles$tile_id==tile_id,]
  #   dst_folder <- AOI$tile_id
  # }

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
      sf::st_transform(sf::st_crs(6349))# %>%
      #sf::st_filter(AOI)
  }

  print(paste0("number of streams:",nrow(stream_lines)))
  cross_section_vector <- list(length=nrow(stream_lines))

  sample_point_distance <- 2
  total_cross_section_distance <- 1220
  extract_points_list <- list(length=nrow(stream_lines))

  print('generating cross section lines')
  # for each line
  for (i in 1:nrow(stream_lines)) {
    # i=1

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
    bearing_coord_index = match(distance_of_all_line_points[distance_of_all_line_points<distance_of_cross_section_points][1],distance_of_all_line_points)
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
    left_bank <- geosphere::destPoint(sf::st_coordinates(cross_section_points), left_bank_bearing, 610, a=6378137, f=1/298.25722210)

    right_bank_bearing = bearing + 90
    if(right_bank_bearing<0) {
      right_bank_bearing <- 360 - abs(right_bank_bearing)
    } else if(right_bank_bearing>360) {
      right_bank_bearing <- right_bank_bearing - 360
    }
    right_bank <- geosphere::destPoint(sf::st_coordinates(cross_section_points), right_bank_bearing, 610, a=6378137, f=1/298.25722210)

    # Construct lines
    points_ready <- rbind(left_bank,right_bank)
    cross_section_vector[[i]] <- sf::st_sf(comid=line$ID,
                                           # Slope=line$Slope,
                                           geometry=sf::st_sfc(sf::st_linestring(points_ready))) %>%
      sf::st_set_crs(sf::st_crs(6349))

    cross_section_vector[[i]]$xid_length <- geosphere::lengthLine(as(cross_section_vector[[i]],"Spatial"))
    little_lines <- sf::st_segmentize(cross_section_vector[[i]],2)
    extract_points <- sf::st_cast(little_lines,"POINT")

    # xid runner
    j <<- i

    extract_points$xid <- i
    extract_points$comid_rel_dist_ds <- 0.5
    extract_points$xid_rel_dist <- rgeos::gProject(sf::as_Spatial(cross_section_vector[[i]]), sf::as_Spatial(extract_points), normalized=TRUE)
    extract_points$d <- (extract_points$xid_length * extract_points$xid_rel_dist) - (0.5* extract_points$xid_length)
    extract_points_list[[i]] <- extract_points
  }

  print('extracting surface')
  # Add crs and flatten
  # cross_sections = map(cross_section_vector, ~ st_set_crs(., sf::st_crs(6349)))
  cross_sections = do.call(rbind, cross_section_vector)
  extract_points_list = do.call(rbind, extract_points_list)

  surface <- raster::raster(ras_path)
  extract_points_list$z <-  raster::extract(surface, extract_points_list,method='simple')

  # Densify lines
  # cross_sections$line_distance <- geosphere::lengthLine(as(cross_sections,"Spatial"))
  # little_lines <- sf::st_segmentize(cross_sections,2)

  #Warning message:In st_cast.sf(little_lines, "POINT") : repeating attributes for all sub-geometries for which they may not be constant
  #mapview::mapview(list(little_lines,extract_points))
  #rgeos::gProject(as(cross_sections,"Spatial"), as(extract_points,"Spatial"), normalized=TRUE)

  # Extract surface
  # surface <- raster::raster(file.path(snapshot_dir,"ehydro_ned_surface.tif"))

  # extract_points$z <-  raster::extract(surface, extract_points,method='simple')
  # extract_points$z <-  raster::extract(surface, cross_sections,method='bilinear')
  # extract_points$relative_distance <- rgeos::gProject(sf::as_Spatial(cross_sections), sf::as_Spatial(extract_points), normalized=TRUE)
  extract_points$xid_d <- (extract_points$xid_rel_dist * extract_points$xid_length) #- (0.5* extract_points$line_distance)

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

  return(extract_points_list)
}

##############################################################################################################################
cross_section_points <- cut_simple_cross_sections_from_eHydro(file.path(snapshot_dir,"final_surface.tif"),cross_section_locations='miss')

stream_lines = sf::read_sf(file.path(basedir,"processed_data","nwm_streams","nwm_streams.shp")) %>%
  sf::st_transform(sf::st_crs(6349))
cwms_cross_section_data <- generate_merged_CWMS_survey()
cwms_enabled_coms <- stream_lines[cwms_cross_section_data,]

cwms_cross_section_points <- pull_cross_sections_from_cwms_by_comid(cwms_enabled_coms)
row.names(cwms_cross_section_points) <- NULL
cwms_cross_section_points$source <- 2
cwms_cross_section_points$x <- unlist(map(cwms_cross_section_points$geometry,1))
cwms_cross_section_points$y <- unlist(map(cwms_cross_section_points$geometry,2))
cwms_cross_section_points$z <- unlist(map(cwms_cross_section_points$geometry,3))
##############################################################################################################################

cross_section_points <- cross_section_points[complete.cases(cross_section_points$z),]
row.names(cross_section_points) <- NULL
cross_section_points$source <- 1
cross_section_points$x <- unlist(map(cross_section_points$geometry,1))
cross_section_points$y <- unlist(map(cross_section_points$geometry,2))
##############################################################################################################################
##############################################################################################################################

cross_section_points_df <- cross_section_points %>%
  sf::st_drop_geometry() %>%
  dplyr::select(-one_of("geometry","xid_rel_dist")) %>%
  data.table::setDT() %>%
  data.table::setcolorder(c("xid", "xid_length", "comid",'comid_rel_dist_ds','xid_d','x','y','z','source'))
cwms_cross_section_points_df <- cwms_cross_section_points %>%
  dplyr::select(-one_of("geometry","xid_rel_dist","FID")) %>%
  data.table::setcolorder(c("xid", "xid_length", "comid",'comid_rel_dist_ds','xid_d','x','y','z','source'))

export_dbase <- do.call(rbind, list(cwms_cross_section_points_df, cross_section_points_df))
export_dbase <- export_dbase[complete.cases(export_dbase$z),]

#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# ----- View and export -------------------------------------------------------------------------------
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# sent_data
cross_section_points_nc <- ncdf4::nc_open(file.path(snapshot_dir,'eHydro_ned_cross_sections_2021_11_17.nc'))
cross_section_points$comid <- ncdf4::ncvar_get(cross_section_points_nc,varid = 'eHydro_ned_cross_sections_2021_11_08_field_comid')  ross_section_points_nc
attributes(cross_section_points_nc$var)$names
lon <- ncdf4::ncvar_get(cross_section_points_nc,"comid","ogr_field_name")

# View
mapview::mapview(cross_section_points[cross_section_points$comid==18055920,])
unique(extract_points$comid)
unique(extract_points[complete.cases(extract_points$z),]$comid)
ggplot(data = extract_points_list[extract_points_list$comid==18055920,], aes(d, z, color = z))+
  geom_point()+
  theme_light()+
  scale_color_gradientn(colors = terrain.colors(10))+
  labs(x = "Distance along profile [m]", y = "Elevation [m]", color = "Elevation [m]")
##############################################################################################################################
##############################################################################################################################

# database export example



file <- file.path(snapshot_dir,paste0('tmp_eHydro_ned_cross_sections_',gsub("-","_",Sys.Date()),'.nc'))
file <- file.path(snapshot_dir,'eHydro_ned_cross_sections_2021_11_22.nc')
cross_section_points_nc <- ncdf4::nc_open(file)
cross_section_points.comid <- ncdf4::ncvar_get(cross_section_points_nc,'xid')
cross_section_points.comid <- ncdf4::ncvar_get(cross_section_points_nc,'comid')
attributes(cross_section_points_nc$var)$names
lon <- ncdf4::ncvar_get(cross_section_points_nc,"comid","ogr_field_name")

# View
mapview::mapview(cross_section_points[cross_section_points$comid==18055920,])
unique(extract_points$comid)
unique(extract_points[complete.cases(extract_points$z),]$comid)
ggplot(data = extract_points_list[extract_points_list$comid==18055920,], aes(d, z, color = z))+
  geom_point()+
  theme_light()+
  scale_color_gradientn(colors = terrain.colors(10))+
  labs(x = "Distance along profile [m]", y = "Elevation [m]",
       color = "Elevation [m]")
##############################################################################################################################
##############################################################################################################################

# database export example
file <- file.path(snapshot_dir,paste0('eHydro_ned_cross_sections_',gsub("-","_",Sys.Date()),'.nc'))


##############################################################################################################################
##############################################################################################################################



#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# ----- append NWM channel geom -------------------------------------------------------------------------------
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
append_NWM_channel_geom <- function(database) {
  comids_to_process <- unique(database$comid)

}
# cross section helper code for geographic coordinate systems, fuck me
# NWM_channel_geom <- ncdf4::nc_open(file.path(basedir,"base_data","Route_Link.nc"))
# topwdthcc = floodplane width
# btmwdth = bed width
# topwdth = top of bottom trap
# ChSlp = side slope
##############################################################################################################################
##############################################################################################################################






##############################################################################################################################
wrangle_cross_sections_from_eHydro <- function(tiles_in_bbox) {
  # Filter bounding box

  cross_section_vector <- list(length=nrow(tiles_in_bbox))

  for (i in 1:nrow(tiles_in_bbox)) {
    tile <- tiles_in_bbox[i,]
    processing_tile_id <- tile$tile_id
    #dir.create(file.path(basedir,"survey",paste0('tile_',processing_tile_id)), showWarnings = FALSE)

    #elevatr_wrapper_for_tile(tile)
    #tile_fishnet <- aggregate_eHydro_footprints_within_AOI(processing_tile_id,tile_feature_subset = NULL)
    #tile_fishnet <- append_eHydro_files_to_AOI(tile_fishnet)
    #mosaic_bathy_surface(tile_fishnet,processing_tile_id)  # Check for empty elemnts? mosaic_bathy_surface(tile_fishnet,180)
    #mosaic_bathy_terrain_surface(processing_tile_id)
    cross_section_vector[[i]] <- cut_cross_sections(processing_tile_id,cross_section_locations='miss')
  }

  cross_sections = do.call(rbind, cross_section_vector)
  return(cross_sections)
}

xx$survey_tiles[c(1,3,4),]


##############################################################################################################################
##############################################################################################################################
generate_LWRP <- function(yyyy) {
  # Gather gage stage data

  start_date <- 01-01-1954
  end_date <- yyyy
  xx$nwis.discharge <- dataRetrieval::readNWISuv(xx$gage.point$site_no, "00060", as.Date(firstdate)-2, as.Date(lastdate)+1, tz="UTC")
  xx$nwis.stage <- readNWISuv(xx$gage.point$site_no, "00065", as.Date(firstdate)-2, as.Date(lastdate)+1, tz="UTC")
}

##############################################################################################################################
##############################################################################################################################

for (i in 1:nrow(xx$survey_tiles)) {

  if(i %in% list(57,58,59,60,61,62,63,64,65,66)) {
    next
  }

  # i=1
  tile <- xx$survey_tiles[i,]
  processing_tile_id <- tile$tile_id

  print(paste0('Processing tile ',i,' of ',nrow(xx$survey_tiles),": ",processing_tile_id))

  # If it's already processed
  if(file.exists(file.path(snapshot_dir,paste0('tile_',processing_tile_id),paste0("merged_surface.tif")))) {
    print("tile already processed")
    next
  }

  dir.create(file.path(snapshot_dir,paste0('tile_',processing_tile_id)), showWarnings = FALSE)

  if(!file.exists(file.path(snapshot_dir,paste0('tile_',processing_tile_id),paste0("NED.tif")))) {
    elevatr_wrapper_for_tile(tile)
  }

  if(!file.exists(file.path(snapshot_dir,paste0('tile_',processing_tile_id),paste0("eHydro_bathy.tif")))) {
    tile_fishnet <- aggregate_eHydro_footprints_within_AOI(processing_tile_id,tile_feature_subset = NULL)
    tile_fishnet_survey <- append_eHydro_files_to_AOI(tile_fishnet)
    mosaic_bathy_surface(tile_fishnet_survey,processing_tile_id)  # Check for empty elemnts? mosaic_bathy_surface(tile_fishnet,180)
  }
  #
  # mosaic_bathy_terrain_surface(processing_tile_id)
  #cross_section_vector[[i]] <- cut_cross_sections(processing_tile_id,cross_section_locations='miss')
}
for (i in 1:nrow(xx$survey_tiles)) {
  # i=1
  tile <- xx$survey_tiles[i,]
  processing_tile_id <- tile$tile_id

  print(paste0('Processing tile ',i,' of ',nrow(xx$survey_tiles),": ",processing_tile_id))

  mosaic_bathy_terrain_surface(processing_tile_id)
}
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
xx$survey_tiles$tile_id <- 1:nrow(xx$survey_tiles)
xx$survey_tiles <- xx$survey_tiles %>% sf::st_filter(xx$eHydro_survey_footprints)


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



fully_run_tiles <- list.files(path = snapshot_dir, pattern = "merged_surface.tif", full.names = TRUE, recursive = TRUE)


cwms_tiles <- xx$survey_tiles[cwms_cross_section_data,]
eHydro_tiles <- xx$survey_tiles[xx$survey_tiles$tile_id %in% list(78247,78947,79296,79647,79996,80347,80696,81047,81396,82096,82447,82796,83147,83496,83846,84196,84546,84896,85247,85596,85947,86647,86996,86997,87347,87697),]

eHydro_tiles$val <- 1
cwms_tiles$val <- 2
AOI <- do.call(rbind, list(eHydro_tiles, cwms_tiles))
mapview::mapview(AOI,zcol='val')
