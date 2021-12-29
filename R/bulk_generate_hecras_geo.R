#' Appends all viable hec ras 1D cross sections into single database
#' @description Point at a directory to establish (create) the folder and base data needed for eHydro to run
#' @param my_location Path on local system
#' @return list object
#' @export
#' @importFrom  httr GET write_disk
#' @importFrom  sf st_as_sf st_set_crs st_as_sfc st_bbox read_sf st_make_valid st_transform st_crs st_filter st_make_grid st_write
#' Blends point clouds together as vertial bins (replace part of one image with another
#' @description Point at a directory to establish (create) the folder and base data needed for eHydro to run
#' @param my_location Path on local system
#' @return list object
#' @export
#' @importFrom  httr GET write_disk
#' @importFrom  sf st_as_sf st_set_crs st_as_sfc st_bbox read_sf st_make_valid st_transform st_crs st_filter st_make_grid st_write
#' @importFrom  rhdf5 
#' @importFrom  stringi stri_sub
#' rhdf5
#' BiocManager

bulk_generate_hecras_geo(survey_location, HEC_path) {
  
  source_crosswalk <- 1
  
  HEC_PATH <- NULL
  
  # hec_geo_files <- list()
  hec_geo_files <- data.frame(g=c('<NA>'),geo=c('<NA>'),proj=c('<NA>'))
  i <- 1
  
  print('Processing HECRAS surveys')
  if(is.null(HEC_PATH)) {
    HEC_PATH <- survey_location
  }
  
  
  files <- list.files(path = HEC_PATH,full.names = T,recursive = T)
  for(file in files) {
    # file <- files[190]
    
    # .g base
    if(stringr::str_detect(stringi::stri_sub(file,-3,-3), "(?i)g")) {
      i <- i+1
      hec_geo_files[i,]$g <- file
      
      if(file.exists(paste0(file,'.hdf'))) {
        
        hec_geo_files[i,]$geo <- paste0(file,'.hdf')
        proj_files <- list.files(path = dirname(file),pattern = '*\\.prj',full.names = T,recursive = T)
        
        for(proj_file in proj_files){
          if(!stringi::stri_detect_fixed(stringi::stri_sub(readLines(file(proj_file,"r"),n=1),0,15),"=")) {
            hec_geo_files[i,]$proj <- proj_file
          }
        }
      }
      
    }
  }
  hec_geo_files <- hec_geo_files[-1,]
  rownames(hec_geo_files) <- 1:nrow(hec_geo_files)
  print(paste("Number of g** (geometry) files found:", nrow(hec_geo_files)))
  
  hec_geo_files <- tidyr::drop_na(hec_geo_files,geo)
  print(paste("Number of g**.hdf (geometry) files found:", nrow(hec_geo_files)))
  
  hec_geo_files <- hec_geo_files[complete.cases(hec_geo_files), ]
  rownames(hec_geo_files) <- 1:nrow(hec_geo_files)
  print(paste("Number of g**.hdf (geometry) files with a projection:", nrow(hec_geo_files)))
  
  print(hec_geo_files)
  # sink('G:/Dropbox/root/tools/myip.txt')
  # print(hec_geo_files)
  # sink()
  
  for(hec_geo in hec_geo_files) {
    geo_file_path <- hec_geo_files[1,]$geo
    # geo_file_path <- hec_geo$geo
    # hf <- rhdf5::H5Fopen(hec_geo$geo)
    
    n1 <- rhdf5::h5read(geo_file_path,'Geometry/Cross Sections/Polyline Points')
    n2 <- rhdf5::h5read(geo_file_path,'Geometry/Cross Sections/Polyline Parts')
    n3 <- rhdf5::h5read(geo_file_path,'Geometry/Cross Sections/Attributes')
    n4 <- rhdf5::h5read(geo_file_path,'Geometry/Cross Sections/River Names')
    n5 <- rhdf5::h5read(geo_file_path,'Geometry/Cross Sections/Reach Names')
    n6 <- rhdf5::h5read(geo_file_path,'Geometry/Cross Sections/River Stations')
    n7 <- rhdf5::h5read(geo_file_path,"Geometry/Cross Sections/Manning's n Info")
    n8 <- rhdf5::h5read(geo_file_path,"Geometry/Cross Sections/Manning's n Values")
    n9 <- rhdf5::h5read(geo_file_path,"Geometry/Cross Sections/Station Elevation Info")
    n10 <- rhdf5::h5read(geo_file_path,"Geometry/Cross Sections/Station Elevation Values")
    # Create a list of  number of points per each stream line
    
    list_points_per_cross_section_line <- n2[2,]
    
    if(nrow(n3) > 0){
      list_river_name <- n3$River
      list_reach_name <- n3$Reach
      list_station <- n3$Name
    } else {
      list_river_name <- n4
      list_reach_name <- n5
      list_station <- n6
    }
    
    ####################################
    ####################################
    ####################################
    cross_section_lines <- data.frame(matrix(ncol=6,nrow=0, dimnames=list(NULL, c("geometry", "xid","stream_stn", "river","reach","ras_path"))))
    
    # Loop through the cross section lines and create GeoDataFrame
    int_startPoint <- 1
    
    for(j in 1:length(list_points_per_cross_section_line)) {
      int_endPoint <- int_startPoint + list_points_per_cross_section_line[j]-1
      #for(k in 1:nlist(t(n1[,int_startPoint:(int_startPoint+int_numPnts)])) )
      cross_section_lines[j,]$geometry <- list(t(n1[,int_startPoint:int_endPoint]))
      cross_section_lines[j,]$xid <- j
      cross_section_lines[j,]$stream_stn <- list_station[j]
      cross_section_lines[j,]$river <- list_river_name[j]
      cross_section_lines[j,]$reach <- list_reach_name[j]
      cross_section_lines[j,]$ras_path <- geo_file_path
      
      int_startPoint <- int_endPoint + 1
    }
    # cross_section_lines
    
    # line string planer form of geometry
    ls_geo_extract <- function(x) {
      x_coords <- x$geometry[[1]][,1]
      y_coords <- x$geometry[[1]][,2]
      len <- length(x_coords)
      xid <- rep(x$xid,length.out=len)
      stream_stn <- rep(x$stream_stn,length.out=len)
      river <- rep(x$river,length.out=len)
      reach <- rep(x$reach,length.out=len)
      ras_path <- rep(x$ras_path,length.out=len)
      
      sf_return <- sfheaders::sf_linestring(
        obj = data.frame(x_coords,y_coords,xid,stream_stn,river,reach,ras_path)
        , x = "x_coords"
        , y = "y_coords"
        , linestring_id = "xid"
        , keep = TRUE
      ) %>% sf::st_sf() %>%
        sf::st_cast()
      return(sf_return)
    }
    sf_cross_section_lines <- c()
    for(h in 1:nrow(cross_section_lines)) {
      sf_cross_section_lines <- rbind(sf_cross_section_lines,ls_geo_extract(cross_section_lines[h,]))
    }
    
    # normalize projection?
    # write projectionless shapefile
    # move proj file to new location
    # rename file
    # load cross sections
    # project to new proj sf::st_transform(xx$survey_tiles, sf::st_crs(6349))
    # write out as new shapefile
    
    # sf_cross_section_lines
    # mapview::mapview(sf_cross_section_lines)
    
    ####################################
    ####################################
    ####################################
    
    point_database <- c()
    
    for(t in 1:ncol(n2)) {
      print(paste("processing",t))
      str_current_xs <- sf_cross_section_lines[t,]$reach
      geom_xs_linestring = sf_cross_section_lines[t,]$geometry
      
      int_prof_xs_start_pnt = n9[1,t]
      int_prof_pnts_in_xs = n9[2,t]
      int_prof_xs_end_pnt = int_prof_xs_start_pnt + int_prof_pnts_in_xs
      list_xs_station = n10[1,int_prof_xs_start_pnt:int_prof_xs_end_pnt]
      list_xs_elevation = n10[2,int_prof_xs_start_pnt:int_prof_xs_end_pnt]
      
      int_prof_xs_n_start_pnt = n7[1,t]
      int_prof_n_pnts_in_xs = n7[2,t]
      int_prof_xs_n_end_pnt = int_prof_xs_n_start_pnt + int_prof_n_pnts_in_xs
      list_xs_n_station = n8[1,int_prof_xs_n_start_pnt:int_prof_xs_n_end_pnt]
      list_xs_n = n8[2,int_prof_xs_n_start_pnt:int_prof_xs_n_end_pnt]
      
      station_elevation_data <- data.frame(xid_d=unlist(list_xs_station),z=unlist(list_xs_elevation))
      station_n_data <- data.frame(xid_d=unlist(list_xs_n_station),n=unlist(list_xs_n))
      
      xs_point_data <- merge(x=station_elevation_data,y=station_n_data,by="xid_d",all.x=TRUE)
      xs_point_data <- xs_point_data %>% tidyr::fill("n", .direction = "down")
      
      pt_xid_length <- sf::st_length(geom_xs_linestring)
      for(point_index in 1:nrow(xs_point_data)) {
        stn <- xs_point_data[point_index,1]
        ratio <- stn / pt_xid_length
        pt <- lwgeom::st_linesubstring(geom_xs_linestring, from = 0, to = ratio) %>% lwgeom::st_endpoint()
        pt_x <- pt[[1]][1]
        pt_y <- pt[[1]][2]
        pt_z <- xs_point_data[point_index,2]
        pt_n <- xs_point_data[point_index,3]
        pt_b <- "test"
        point_database <- rbind(point_database,
                                data.frame(xid=t,
                                           xid_length=pt_xid_length,
                                           xid_d=pt_x,
                                           x=pt_x,
                                           y=pt_y,
                                           z=pt_z,
                                           n=pt_n,
                                           source=3))
      }
    }
    
    ls_geoz_extract <- function(x) {
      sf_return <- sfheaders::sf_linestring(
        obj = x
        , x = "x"
        , y = "y"
        , z = "z"
        , linestring_id = "xid"
        , keep = TRUE
      ) %>% sf::st_sf() %>%
        sf::st_cast()
      return(sf_return)
    }
    sf_cross_section_line_z <- c()
    for(k in 1:nrow(point_database)) {
      sf_cross_section_line_z <- rbind(sf_cross_section_line_z,ls_geox_extract(point_database[k,]))
    }
    sf::st_write(sf_cross_section_line_z, file.path(snapshot_dir,"_ehydro_.shp"))
  }
  
  list.files(pattern='*_ehydro_.shp', recursive=TRUE, full.names = TRUE)
  
  find all shapefiles and merge database
  dir.create()
  write all shapefiles
  write source crosswalk
}