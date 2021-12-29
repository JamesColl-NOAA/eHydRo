#' Blends point clouds together as vertial bins (replace part of one image with another
#' @description Point at a directory to establish (create) the folder and base data needed for eHydro to run
#' @param my_location Path on local system
#' @return list object
#' @export
#' @importFrom  httr GET write_disk
#' @importFrom  sf st_as_sf st_set_crs st_as_sfc st_bbox read_sf st_make_valid st_transform st_crs st_filter st_make_grid st_write

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