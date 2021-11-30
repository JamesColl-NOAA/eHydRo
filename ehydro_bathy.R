#!/usr/bin/env Rscript 

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
library(raster)

source("ehydro_functions.R")


# Define paths
basedir    <- file.path(Sys.getenv('base_dir'))
bathy_dir  <- file.path(Sys.getenv('bathy_dir'))
user_survey <- file.path(basedir,Sys.getenv('use_survey_path'))

# Create temp workspace directory
scratchdir <- file.path(basedir,"survey_tmp")
dir.create(scratchdir, showWarnings = FALSE)

xx <- list() 
memory.size(max = TRUE)
memory.size()

# Read in Routelink file (do we need to bring in line data instead?)
NWM_channel_geom <- ncdf4::nc_open(file.path(Sys.getenv('NWM_route_link')))

## Check if survey data exists localy 
if(new_survey) {
  
  # Start by downloading surveys
  date_stamp <- gsub("-","_",Sys.Date())
  snapshot_dir <- file.path(basedir,paste0("survey_",date_stamp))
  dir.create(snapshot_dir, showWarnings = FALSE)
  
  if(file.exists(file.path(snapshot_dir,"eHydro_Survey_Data.zip"))) {
    file.remove(file.path(snapshot_dir,"eHydro_Survey_Data.zip"))
  }
  if(file.exists(file.path(snapshot_dir, "SurveyJob.shp"))) {
    file.remove(file.path(snapshot_dir, "SurveyJob.shp"))
    file.remove(file.path(snapshot_dir, "SurveyJob.shx"))
    file.remove(file.path(snapshot_dir, "SurveyJob.prj"))
    file.remove(file.path(snapshot_dir, "SurveyJob.dbf"))
    file.remove(file.path(snapshot_dir, "SurveyJob.cpg"))
  }
  if(file.exists(file.path(snapshot_dir, "processing_tiles.shp"))) {
    file.remove(file.path(snapshot_dir, "processing_tiles.shp"))
    file.remove(file.path(snapshot_dir, "processing_tiles.shx"))
    file.remove(file.path(snapshot_dir, "processing_tiles.prj"))
    file.remove(file.path(snapshot_dir, "processing_tiles.dbf"))
  }
  
  httr::GET(Sys.getenv('eHydro_survey_url'), write_disk(file.path(snapshot_dir,"eHydro_Survey_Data.zip")), overwrite=TRUE)
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
  # sf::st_write(xx$survey_tiles, file.path(snapshot_dir,"full_processing_tiles.shp"))
  
} else {
  
  snapshot_dir <- file.path(user_survey)
  xx$eHydro_survey_footprints <- sf::read_sf(file.path(snapshot_dir, "SurveyJob.shp")) %>%
    sf::st_make_valid() %>%
    sf::st_transform(sf::st_crs(6349))
  xx$survey_tiles <- sf::read_sf(file.path(snapshot_dir, "processing_tiles.shp"))
  
}
xx$survey_tiles <- xx$survey_tiles[order(xx$survey_tiles$tile_id,decreasing = TRUE),]


## Preprocess terrain elevation
tile_id <- Sys.getenv('tile_id')
dir.create(file.path(snapshot_dir,paste0('tile_',tile_id)), showWarnings = FALSE)
elevatr_wrapper_for_tile(tile_id)


## Process eHydro data
# we'd also test an average?
#crop_eHydro_footprints_within_AOI <- function(tile_feature,tile_feature_subset = NULL) {
tile_fishnet <- aggregate_eHydro_footprints_within_AOI(xx$survey_tiles[xx$survey_tiles$tile_id==tile_id,])
# mapview::mapview(list(xx$survey_tiles[xx$survey_tiles$tile_id==id,],tile_fishnet,xx$eHydro_survey_footprints %>% sf::st_filter(tile_fishnet)))

## Add survey info to AOI
tile_fishnet_survey <- append_eHydro_files_to_AOI(tile_fishnet)

## Create the bathy surface
# we'd also test a reduce approach?
#reduce_bathy_surface <- function(AOI_tiles_with_paths,reducer = min) {
mosaic_bathy_surface(tile_fishnet_survey,tile_id)

# Create the final tile surface 
mosaic_bathy_terrain_surface(tile_id)


## Mosaic surfaces
mosaic_bathy_terrain_surface('bathy_NED')


## Cut cross sections
# older and trash?
# Also need a custom cross section approach?
#reduce_bathy_surface <- function(AOI_tiles_with_paths,reducer = min) {
cross_section_points <- cut_simple_cross_sections_from_full(file.path(snapshot_dir,"final_surface.tif"),cross_section_locations='miss') 

cross_section_points <- cross_section_points[complete.cases(cross_section_points$z),]
row.names(cross_section_points) <- NULL
cross_section_points$source <- 1
cross_section_points$x <- unlist(map(cross_section_points$geometry,1))
cross_section_points$y <- unlist(map(cross_section_points$geometry,2))


## View and export
# sent_data
cross_section_points_nc <- ncdf4::nc_open(file.path(snapshot_dir,'eHydro_ned_cross_sections_2021_11_17.nc'))
cross_section_points$comid <- ncdf4::ncvar_get(cross_section_points_nc,varid = 'eHydro_ned_cross_sections_2021_11_08_field_comid')  ross_section_points_nc
attributes(cross_section_points_nc$var)$names
lon <- ncdf4::ncvar_get(cross_section_points_nc,"comid","ogr_field_name")

## View
# mapview::mapview(cross_section_points[cross_section_points$comid==18055920,])
# unique(extract_points$comid)
# unique(extract_points[complete.cases(extract_points$z),]$comid)
# ggplot(data = extract_points_list[extract_points_list$comid==18055920,], aes(d, z, color = z))+
#   geom_point()+
#   theme_light()+
#   scale_color_gradientn(colors = terrain.colors(10))+
#   labs(x = "Distance along profile [m]", y = "Elevation [m]",
#        color = "Elevation [m]")

## database export example
file <- file.path(snapshot_dir,paste0('eHydro_ned_cross_sections_',gsub("-","_",Sys.Date()),'.nc'))
cross_section_points <- cross_section_points %>% sf::st_drop_geometry()

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

att.put.nc(nc, "NC_GLOBAL", "title", "NC_CHAR", "Data from Foo")

RNetCDF::close.nc(nc)
unlink(nc)


## Append NWM channel geom
# append_NWM_channel_geom(database)


## Merge CWMS data
# generate_merged_CWMS_survey()


xx$survey_tiles[c(1,3,4),]


for (i in 1:nrow(xx$survey_tiles)) {
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

## Find tiles that intersect the area we want
stream_lines = sf::read_sf(file.path(basedir,"processed_data","miss_streams","miss_streams.shp")) %>% 
  sf::st_transform(sf::st_crs(6349))
xx$survey_tiles <- xx$survey_tiles %>% 
  sf::st_filter(stream_lines)

xx$survey_tiles
# mapview::mapview(list(xx$survey_tiles,stream_lines))

wrangle_cross_sections_from_eHydro(xx$survey_tiles)
