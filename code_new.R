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
  sf::st_transform(sf::st_crs(5498))
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
  if(is.data.frame(tile_id)) {
    AOI <- tile_id
    tile_id <- AOI$tile_id
  } else {
    AOI <- xx$survey_tiles[xx$survey_tiles$tile_id==tile_id,]
  }
  
  print('Processing elevation for tile',tile_id)
  # small buffer around feature?
  AOI_4326 <- sf::st_transform(AOI, sf::st_crs(4326))
  elevatr_NED <- elevatr::get_elev_raster(AOI_4326, z = 14, clip = 'locations', override_size_check=TRUE)
  print('reprojecting and saving data')
  raster::writeRaster(elevatr_NED,filename=file.path(basedir,"survey",paste0('tile_',tile_id),"NED_4326.tif"),bylayer=TRUE,format="GTiff")
  gdalUtilities::gdalwarp(srcfile = file.path(basedir,"survey",paste0('tile_',tile_id),"NED_4326.tif"), dstfile = file.path(basedir,"survey",paste0('tile_',tile_id),"NED.tif"), t_srs = sf::st_crs(5070), r = "bilinear")   
  # Toss tmp ned file.remove(file.path(basedir,"survey",paste0('tile_',tile_id),"NED_4326.tif"))
}

##############################################################################################################################
##############################################################################################################################




#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
# ----- process eHydro data -------------------------------------------------------------------------------
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
aggregate_eHydro_footprints_within_AOI <- function(tile_feature,tile_feature_subset = NULL) {
  # Input parsing
  if(is.data.frame(tile_feature)) {
    tile_feature <- tile_feature
  } else {
    tile_feature <- xx$survey_tiles[xx$survey_tiles$tile_id==tile_feature,]
  }
  
  # Term for potential lines (should improve performance?)
  if(is.null(tile_feature_subset)) { tile_feature_subset <- tile_feature}
  
  # Subset footprints and generate spatial index 
  print('Generating tiles')
  intersecting_footprints <- xx$eHydro_survey_footprints %>% sf::st_filter(tile_feature)
  intersecting_footprints_hardclip <- rgeos::gIntersection(spgeom1=as(tile_feature,'Spatial'),spgeom2=as(intersecting_footprints,'Spatial'), byid=TRUE,drop_lower_td=TRUE)
  
  tile_fishnet <- NULL
  tile_fishnet <- sf::st_make_grid(intersecting_footprints_hardclip, cellsize = 10, what = "polygons", square = TRUE) %>%
    sf::st_as_sf() %>% 
    sf::st_filter(sf::st_as_sf(intersecting_footprints_hardclip))%>% 
    sf::st_filter(tile_feature_subset)
  
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
  out_df <- do.call(rbind.data.frame, out)
  tile_fishnet$objectid <- out_df$objectid 
  tile_fishnet$surveydate <- out_df$surveydate
  tile_fishnet$sourcedata <- out_df$sourcedata
  tile_fishnet$sourceproj <- out_df$sourceproj
  tile_fishnet$plotsheetl <- out_df$plotsheetl
  return(tile_fishnet)
}

# we'd also test an average?
#crop_eHydro_footprints_within_AOI <- function(tile_feature,tile_feature_subset = NULL) {
#)
elemnts <- aggregate_eHydro_footprints_within_AOI(xx$survey_tiles[180,])
elemnts
mapview::mapview(xx$survey_tiles)

##############################################################################################################################
append_eHydro_files_to_AOI <- function(AOI_tiles) {
  # clean up scratch
  #if(dir_size(scratchdir)/10**9>scratch_limit) { do.call(file.remove, list(list.files(scratchdir, recursive = TRUE,  full.names = TRUE))) }
  
  # add extra filepath fields
  AOI_tiles <- AOI_tiles %>%
    add_column(tin_path = NA) %>%
    add_column(survey_path = NA) %>%
    add_column(surveyHD_path = NA) %>%
    add_column(plot_path = NA) %>%
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
  return(AOI_tiles)
}

elemnts1 <- append_eHydro_files_to_AOI(elemnts)
elemnts1

##############################################################################################################################
mosaic_bathy_surface <- function(AOI_tiles_with_paths,dst_folder) {
  # clean up scratch
  #if(dir_size(scratchdir)/10**9>scratch_limit) { do.call(file.remove, list(list.files(scratchdir, recursive = TRUE,  full.names = TRUE))) }
  
  datumlist <- list() 
  tmp_pts = data.frame()
  distinct_surveys <- unique(AOI_tiles_with_paths$sourcedata)
  for (x in 1:length(distinct_surveys)) {
    print(paste('Processing survey',x,'of',length(distinct_surveys)))
    # load in points
    #if(tin) {  AOI_tiles_with_paths[AOI_tiles_with_paths$sourcedata==distinct_surveys[[5]],] %>% head(1) %>% pull(tin_path)
      #load tin
      #transform to xyz
    #} else {
    if(TRUE) {
      tmp_pts = data.frame()
      pts <- AOI_tiles_with_paths[AOI_tiles_with_paths$sourcedata==distinct_surveys[[5]],] %>% head(1) %>% pull(survey_path)
      HDpts <- AOI_tiles_with_paths[AOI_tiles_with_paths$sourcedata==distinct_surveys[[5]],] %>% head(1) %>% pull(surveyHD_path)
      if(is.character(pts)) {
        tmp_pts1 <- rgdal::readOGR(dsn=pts,layer="SurveyPoint",verbose = FALSE)
        this_crs <- tmp_pts1@proj4string 
        this_datum <- head(tmp_pts1,1) %>% pull(elevationDatum)
        tmp_pts1 <- data.frame(tmp_pts1$xLocation,tmp_pts1$yLocation,tmp_pts1$surveyPointElev)
        tmp_pts <- rbind(tmp_pts, tmp_pts1)
      }
      if(is.character(HDpts)) {
        # NEED TO CHECK FIELD NAMES
        tmp_pts1 <- rgdal::readOGR(dsn=pts,layer="SurveyPointHD",verbose = FALSE)
        this_crs <- tmp_pts1@proj4string 
        this_datum <- head(tmp_pts1,1) %>% pull(elevationDatum)
        tmp_pts1 <- data.frame(tmp_pts1$xLocation,tmp_pts1$yLocation,tmp_pts1$surveyPointElev)
        tmp_pts <- rbind(tmp_pts, tmp_pts1)
      }
      datumlist <- append(datumlist, this_datum)
      print(paste0(nrow(tmp_pts), ' points accumulated'))
    }
    # Reproject
    print('Projecting points')
    pts <- sf::st_as_sf(data.frame(tmp_pts), coords = c("tmp_pts1.xLocation", "tmp_pts1.yLocation", "tmp_pts1.surveyPointElev"), crs = this_crs)
    pts_proj <- sf::st_transform(pts, sf::st_crs(5070))
    pts_proj_geometry <- sf::st_coordinates(pts_proj) %>% as.data.frame()
    pts_proj$X <- pts_proj_geometry$X
    pts_proj$Y <- pts_proj_geometry$Y
    pts_proj$Z <- pts_proj_geometry$Z
    sp_pts <- as(pts_proj, "Spatial")

    # Generate interp grid
    print('Generating raster interpolation')
    grid_template <- pts_proj %>% 
      sf::st_bbox() %>% 
      sf::st_as_sfc() %>% 
      sf::st_make_grid(cellsize = c(5, 5),what = "centers") %>%
      sf::st_as_sf() %>%
      cbind(., st_coordinates(.)) %>% 
      sf::st_drop_geometry() %>% 
      mutate(Z = 0)
    grd_template_raster <- grid_template %>% 
      raster::rasterFromXYZ(
        crs = sp_pts@proj4string
      )
    fit_IDW <- gstat::gstat( # The setup here is quite similar to NN
      formula = Z ~ 1,
      data = as(pts_proj, "Spatial"),
      nmax = 10, nmin = 3,
      set = list(idp = 0.5) # inverse distance power
    )
    
    interp_IDW <- raster::interpolate(grd_template_raster, fit_IDW)
    
    survey_mask <- AOI_tiles_with_paths[AOI_tiles_with_paths$sourcedata==distinct_surveys[[2]],] %>% 
      sf::st_union() %>% 
      sf::st_as_sf()
    
    final_raster <- raster::raster(interp_IDW) %>% raster::mask(survey_mask)
    
  }
  if(length(HUC6_labels_toprocess)>1) {
    merged_HANDRasters <- do.call(raster::merge, all_HANDRasters)
  } else {
    merged_HANDRasters <- all_HANDRasters[[1]]
  }
  print("-- Welcome to FOSSFlood - cropping HAND data")
  raster::crop(x = merged_HANDRasters, y = raster::extent(user.aoi.call %>% sf::st_transform(sf::st_crs(raster::crs(merged_HANDRasters)))), snap="in", filename=paste0(tmpdir,"hand_crop.tif"))
  print("-- Welcome to FOSSFlood - projecting HAND data")
  gdalUtilities::gdalwarp(srcfile = paste0(tmpdir,"hand_crop.tif"), dstfile = paste0(basedir,"/AOI/",user.aoi.filepath,"/hand_",user.aoi.filepath,".tif"), t_srs = sf::st_crs(3857), r = "bilinear")   
  
  
  return(tile_fishnet)
}

mapview::mapview(xx$survey_tiles)

AOI_tiles_with_paths <- elemnts1
distinct_surveys <- unique(AOI_tiles_with_paths$sourcedata)
for (x in 1:length(distinct_surveys)) {
  # load in points
  if(tin) {  AOI_tiles_with_paths[AOI_tiles_with_paths$sourcedata==distinct_surveys[[5]],] %>% head(1) %>% pull(tin_path)
    load tin
    transform to xyz
  } else {
    tmp_pts = data.frame()
    pts <- AOI_tiles_with_paths[AOI_tiles_with_paths$sourcedata==distinct_surveys[[5]],] %>% head(1) %>% pull(survey_path)
    HDpts <- AOI_tiles_with_paths[AOI_tiles_with_paths$sourcedata==distinct_surveys[[5]],] %>% head(1) %>% pull(surveyHD_path)
    if(is.character(pts)) {
      tmp_pts1 <- rgdal::readOGR(dsn=pts,layer="SurveyPoint")
      this_crs <- tmp_pts1@proj4string 
      tmp_pts1 <- data.frame(tmp_pts1$xLocation,tmp_pts1$yLocation,tmp_pts1$surveyPointElev)
      tmp_pts <- rbind(tmp_pts, tmp_pts1)
    }
    if(is.character(HDpts)) {
      # NEED TO CHECK FIELD NAMES
      tmp_pts1 <- rgdal::readOGR(dsn=pts,layer="SurveyPointHD")
      this_crs <- tmp_pts1@proj4string 
      tmp_pts1 <- data.frame(tmp_pts1$xLocation,tmp_pts1$yLocation,tmp_pts1$surveyPointElev)
      tmp_pts <- rbind(tmp_pts, tmp_pts1)
    }
  }





f<-"/your/path/irregular_points.xyz"
pts <- read.table(f, header=FALSE, col.names=c("x", "y", "z")) # change accordingly - use read.csv for a csv!

survey_mask <- elemnts1[elemnts1$sourcedata==distinct_surveys[[180]],]
rast <- raster::raster(ext=raster::extent(firstPoints_proj), resolution=5)
rasOut <- raster::rasterize(firstPoints_proj, rast, firstPoints_proj$geometry$, fun = mean)

firstPoints_proj$z

# create a SpatialPointsDataFrame
coordinates(pts) = ~x+y 									   

# create an empty raster object to the extent of the points
rast <- raster(ext=extent(pts), resolution=250)

# rasterize your irregular points 
rasOut<-rasterize(pts, rast, pts$z, fun = mean) # we use a mean function here to regularly grid the irregular input points

#write it out as a geotiff
fout="my_raster.tif"


# we'd also test a reduce approach?
#reduce_bathy_surface <- function(AOI_tiles_with_paths,reducer = min) {
#)

##############################################################################################################################
mosaic_bathy_terrain_surface <- function(tile_id) {
  # reduce bathy and terrain to single tif
}


##############################################################################################################################
cut_cross_sections <- function(tile_id,cross_section_locations) {
  # Takes full terrain and cuts cross sections
  read_from_gpkg <- function(path, filetype) {
    gpkg_file <- paste0(tempfile(), ".gpkg")
    sf::gdal_utils(
      util = "vectortranslate",
      source = path, 
      destination = gpkg_file, 
      options = c("-f", "GPKG", filetype)
    )
    res <- sf::st_read(gpkg_file, quiet = TRUE)
    names(res)[which(names(res) == "geom")] <- "geometry"
    sf::st_geometry(res) <- "geometry"
    res
  }
  
  FEMA_cross_sections_avalible = FALSE
  if(FEMA_cross_sections_avalible) {
    # Load in files
  } else {
    # Flatten footprints to lines
    
    # Perpendicular line spacing
  }
  
  
}

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

mapview::mapview(xx$eHydro_survey_footprints)


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

# Merge rasters  ------------------------------------------------------------------
print("-- Welcome to FOSSFlood - merging HAND data")
openHANDRasters_func <- function(HUC6LabelsString) {return( raster::raster(paste0(basedir,'/AOI/',HUC6LabelsString,'/',HUC6LabelsString,'hand.tif')) )}
all_HANDRasters <- lapply(HUC6_labels_toprocess,openHANDRasters_func)
if(length(HUC6_labels_toprocess)>1) {
  merged_HANDRasters <- do.call(raster::merge, all_HANDRasters)
} else {
  merged_HANDRasters <- all_HANDRasters[[1]]
}
print("-- Welcome to FOSSFlood - cropping HAND data")
raster::crop(x = merged_HANDRasters, y = raster::extent(user.aoi.call %>% sf::st_transform(sf::st_crs(raster::crs(merged_HANDRasters)))), snap="in", filename=paste0(tmpdir,"hand_crop.tif"))
print("-- Welcome to FOSSFlood - projecting HAND data")
gdalUtilities::gdalwarp(srcfile = paste0(tmpdir,"hand_crop.tif"), dstfile = paste0(basedir,"/AOI/",user.aoi.filepath,"/hand_",user.aoi.filepath,".tif"), t_srs = sf::st_crs(3857), r = "bilinear")   

if(user.output.hardclip) {
  print("-- Cropping to AOI --")
  
  xx$catch.grid <- xx$catch.grid %>% raster::mask(xx$aoi.shp_d)
  xx$hand.grid <- xx$hand.grid %>% raster::mask(xx$aoi.shp_d)
  xx$flood.grid <- suppressWarnings(suppressMessages(xx$flood.grid %>% raster::mask(xx$aoi.shp_d)))
  
  xx$mapindex.poly <- suppressWarnings(suppressMessages(xx$mapindex.poly[xx$aoi.shp_d, ]))
  xx$road.line <- suppressWarnings(suppressMessages(sf::st_intersection(xx$road.line, xx$aoi.shp_d))) %>% sf::st_collection_extract("LINESTRING")


quiet(gc())
##############################################################################################################################
############################################################################################################################## 
aggregate_eHydro_footprints_within_AOI <- function(tile_feature,tile_feature_subset = NULL) {
  # Input parsing
  if(is.data.frame(tile_feature)) {
    tile_feature <- tile_feature
  } else {
    tile_feature <- xx$survey_tiles[xx$survey_tiles$tile_id==tile_feature,]
  }
  
  # Term for potential lines (should improve performance)
  if(is.null(tile_feature_subset)) { tile_feature_subset <- tile_feature}
  
  # Subset footprints and generate spatial index 
  print('Generating tiles')
  intersecting_footprints <- xx$eHydro_survey_footprints[tile_feature,]
  tile_fishnet <- sf::st_make_grid(intersecting_footprints, cellsize = 10, what = "polygons", square = TRUE) %>%
    sf::st_as_sf() %>% 
    sf::st_filter(intersecting_footprints) %>%
    sf::st_filter(tile_feature_subset)
  
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
  out_df <- do.call(rbind.data.frame, out)
  tile_fishnet$objectid <- out_df$objectid 
  tile_fishnet$surveydate <- out_df$surveydate
  tile_fishnet$sourcedata <- out_df$sourcedata
  tile_fishnet$sourceproj <- out_df$sourceproj
  tile_fishnet$plotsheetl <- out_df$plotsheetl
  return(tile_fishnet)
}

# we'd also test an average?
#crop_eHydro_footprints_within_AOI <- function(tile_feature,tile_feature_subset = NULL) {
#)
elemnts <- aggregate_eHydro_footprints_within_AOI(xx$survey_tiles[180,])
elemnts

tile_feature <- xx$survey_tiles[6,]
tile_feature_subset <- NULL
# Input parsing
if(is.data.frame(tile_feature)) {
  tile_feature <- tile_feature
} else {
  tile_feature <- xx$survey_tiles[xx$survey_tiles$tile_id==tile_feature,]
}

# Term for potential lines (should improve performance)
if(is.null(tile_feature_subset)) { tile_feature_subset <- tile_feature}

# Subset footprints and generate spatial index 
print('Generating tiles')
intersecting_footprints <- xx$eHydro_survey_footprints[tile_feature,]
tile_fishnet <- sf::st_make_grid(intersecting_footprints, cellsize = 10, what = "polygons", square = TRUE) %>%
  sf::st_as_sf() %>% 
  sf::st_filter(intersecting_footprints) %>%
  sf::st_filter(tile_feature_subset)

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
out_df <- do.call(rbind.data.frame, out)
tile_fishnet$objectid <- out_df$objectid 
tile_fishnet$surveydate <- out_df$surveydate
tile_fishnet$sourcedata <- out_df$sourcedata
tile_fishnet$sourceproj <- out_df$sourceproj
tile_fishnet$plotsheetl <- out_df$plotsheetl

append_eHydro_files_to_AOI <- function(AOI_tiles) {
  # clean up scratch
  #if(dir_size(scratchdir)/10**9>scratch_limit) { do.call(file.remove, list(list.files(scratchdir, recursive = TRUE,  full.names = TRUE))) }
  
  # add extra filepath fields
  AOI_tiles <- AOI_tiles %>%
    add_column(tin_path = NA) %>%
    add_column(survey_path = NA) %>%
    add_column(surveyHD_path = NA) %>%
    add_column(plot_path = NA) %>%
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
  return(AOI_tiles)
}

elemnts1 <- append_eHydro_files_to_AOI(elemnts)