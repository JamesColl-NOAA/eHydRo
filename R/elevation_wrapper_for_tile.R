#' @title download terrain elevations from a source and clip to tile AOI
#' @description wrappers around elevatr and alternative sources of elevation
#' @param my_location Path on local system
#' @param source one of "AWS", 
#' @param tile_id either a tile itself or an id for the tile (generated in )
#' @return list object
#' @export
#' @importFrom  httr GET write_disk
#' @importFrom  sf st_as_sf st_set_crs st_as_sfc st_bbox read_sf st_make_valid st_transform st_crs st_filter st_make_grid st_write

elevation_wrapper_for_tile = function(my_location, tile_id, source) {
 
  print("-- Generating terrain surface --")
  
  # Input parsing
  step_start_time <- Sys.time()
  if(is.data.frame(tile_id)) {
    AOI <- tile_id
    tile_id <- AOI$tile_id
  } else {
    survey_tiles <- sf::read_sf(file.path(my_location, "processing_tiles.shp"))
    AOI <- survey_tiles[survey_tiles$tile_id==tile_id,]
  }

  dir.create(file.path(my_location,paste0('tile_',id)), showWarnings = FALSE)

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