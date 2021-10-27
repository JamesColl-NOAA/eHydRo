## ----setup, include = FALSE---------------------------------------------------
library(nhdplusTools)

local <- (Sys.getenv("BUILD_VIGNETTES") == "TRUE")
if(local) {
  cache_path <- file.path(nhdplusTools_data_dir(), "index_v")
} else {
  cache_path <- tempdir()
}

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=6, 
  fig.height=4,
  eval=local,
  cache=local,
  cache.path=(cache_path)
)

oldoption <- options(scipen = 9999,
                     "rgdal_show_exportToProj4_warnings"="none")

## ----nhdplus_path_setup, echo=FALSE, include=FALSE----------------------------
library(dplyr, warn.conflicts = FALSE)

work_dir <- file.path(nhdplusTools_data_dir(), "index_vignette")
dir.create(work_dir, recursive = TRUE)

source(system.file("extdata/sample_data.R", package = "nhdplusTools"))

file.copy(sample_data,
          file.path(work_dir, "natseamless.gpkg"))

## ----nhdplus_path, echo=TRUE--------------------------------------------------
library(nhdplusTools)

nhdplus_path(file.path(work_dir, "natseamless.gpkg"))

flowlines <- sf::read_sf(nhdplus_path(), "NHDFlowline_Network")
gages <- sf::read_sf(nhdplus_path(), "Gage")

## ----get_indexes--------------------------------------------------------------
indexes <- get_flowline_index(flowlines,
                              sf::st_geometry(gages), 
                              search_radius = 0.01, 
                              max_matches = 1)

indexes <- left_join(sf::st_sf(id = c(1:nrow(gages)), 
                               geom = sf::st_geometry(gages)), 
                     indexes, by = "id")

plot(sf::st_geometry(sf::st_zm(flowlines)))
plot(sf::st_geometry(indexes), add = TRUE)


## ----analyze_index------------------------------------------------------------
p_match <- 100 * length(which(indexes$COMID %in% gages$FLComID)) / nrow(gages)
paste0(round(p_match, digits = 1), 
       "% were found to match the COMID in the NHDPlus gages layer")

p_match <- 100 * length(which(indexes$REACHCODE %in% gages$REACHCODE)) / nrow(gages)
paste0(round(p_match, digits = 1), 
       "% were found to match the REACHCODE in the NHDPlus gages layer")

matched <- cbind(indexes, 
                 dplyr::select(sf::st_drop_geometry(gages), 
                               REACHCODE_ref = REACHCODE, 
                               COMID_ref = FLComID, 
                               REACH_meas_ref = Measure)) %>%
  dplyr::filter(REACHCODE == REACHCODE_ref) %>%
  dplyr::mutate(REACH_meas_diff = REACH_meas - REACH_meas_ref)

hist(matched$REACH_meas_diff, breaks = 100, 
     main = "Difference in measure for gages matched to the same reach.")

round(quantile(matched$REACH_meas_diff, 
               probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)), 
      digits = 2)

## ----get_indexes_precise------------------------------------------------------
indexes <- get_flowline_index(flowlines, 
                              sf::st_geometry(gages), 
                              search_radius = 0.1, 
                              precision = 10)

indexes <- left_join(data.frame(id = seq_len(nrow(gages))), indexes, by = "id")

## ----analyze_inde_precise-----------------------------------------------------
p_match <- 100 * length(which(indexes$COMID %in% gages$FLComID)) / nrow(gages)
paste0(round(p_match, digits = 1), 
       "% were found to match the COMID in the NHDPlus gages layer")

p_match <- 100 * length(which(indexes$REACHCODE %in% gages$REACHCODE)) / nrow(gages)
paste0(round(p_match, digits = 1), 
       "% were found to match the REACHCODE in the NHDPlus gages layer")

matched <- cbind(indexes, 
                 dplyr::select(sf::st_set_geometry(gages, NULL), 
                               REACHCODE_ref = REACHCODE, 
                               COMID_ref = FLComID, 
                               REACH_meas_ref = Measure)) %>%
  dplyr::filter(REACHCODE == REACHCODE_ref) %>%
  dplyr::mutate(REACH_meas_diff = REACH_meas - REACH_meas_ref)

hist(matched$REACH_meas_diff, breaks = 100, 
     main = "Difference in measure for gages matched to the same reach.")

round(quantile(matched$REACH_meas_diff, 
               probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)), digits = 2)

## ----multi--------------------------------------------------------------------
indexes <- get_flowline_index(flowlines,
                              sf::st_geometry(gages)[42], 
                              search_radius = 0.01, 
                              max_matches = 10)

indexes <- left_join(sf::st_sf(id = 1, 
                               geom = sf::st_geometry(gages)[42]), 
                     indexes, by = "id")

plot(sf::st_geometry(sf::st_buffer(indexes, 0.005)), border = NA)
plot(sf::st_geometry(indexes), add = TRUE)
plot(sf::st_geometry(sf::st_zm(flowlines)), col = "blue", add = TRUE)
indexes

## ----waterbodies--------------------------------------------------------------
waterbody <- sf::read_sf(nhdplus_path(), "NHDWaterbody")

gages <- sf::st_drop_geometry(gages) %>%
  dplyr::filter(!is.na(LonSite)) %>%
  sf::st_as_sf(coords = c("LonSite", "LatSite"), crs = 4326)

plot(sf::st_geometry(sf::st_zm(flowlines)))
plot(sf::st_geometry(waterbody), add = TRUE)
plot(sf::st_geometry(gages), add = TRUE)


## ----index_waterbodies--------------------------------------------------------

flowline_indexes <- left_join(data.frame(id = seq_len(nrow(gages))),
                              get_flowline_index(
                                sf::st_transform(flowlines, 5070), 
                                sf::st_geometry(sf::st_transform(gages, 5070)), 
                                search_radius = 200), by = "id")
                              
indexed_gages <- cbind(dplyr::select(gages, 
                                      orig_REACHCODE = REACHCODE, 
                                      orig_Measure = Measure, 
                                      FLComID, 
                                      STATION_NM), 
                        flowline_indexes,
                        get_waterbody_index(
                          st_transform(waterbody, 5070), 
                          st_transform(gages, 5070), 
                          st_drop_geometry(flowlines), 
                          search_radius = 200))

plot(sf::st_geometry(sf::st_zm(flowlines)))
plot(sf::st_geometry(waterbody), add = TRUE)
plot(sf::st_geometry(indexed_gages), add = TRUE)

dplyr::select(sf::st_drop_geometry(indexed_gages), near_wb_COMID, near_wb_dist, in_wb_COMID, outlet_fline_COMID)


## ----teardown, include=FALSE--------------------------------------------------
options(oldoption)
if(Sys.getenv("BUILD_VIGNETTES") != "TRUE") {
  unlink(work_dir, recursive = TRUE)
}

