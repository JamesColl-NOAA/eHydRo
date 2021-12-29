#' Blends point clouds together as vertial bins (replace part of one image with another
#' @description Point at a directory to establish (create) the folder and base data needed for eHydro to run
#' @param my_location Path on local system
#' @return list object
#' @export
#' @importFrom  httr GET write_disk
#' @importFrom  sf st_as_sf st_set_crs st_as_sfc st_bbox read_sf st_make_valid st_transform st_crs st_filter st_make_grid st_write

establish_survey_dir = function(my_location) {
 
   xx <- list()
  
  if(!file.exists(file.path(my_location, "processing_tiles.shp"))) {
    print('-- No eHydro survey found, processing base 6data --')
    eHydro_survey_url <- "https://opendata.arcgis.com/api/v3/datasets/80a394bae6b547f1b5788074261e11f1_0/downloads/data?format=shp&spatialRefId=4326"
    date_stamp <- gsub("-","_",Sys.Date())
    snapshot_dir <- file.path(my_location,paste0("survey_",date_stamp))
    dir.create(snapshot_dir, showWarnings = FALSE)
    
    httr::GET(eHydro_survey_url, write_disk(file.path(snapshot_dir,"eHydro_Survey_Data.zip")), overwrite=TRUE)
    unzip(file.path(snapshot_dir,"eHydro_Survey_Data.zip"), exdir = snapshot_dir)
    
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