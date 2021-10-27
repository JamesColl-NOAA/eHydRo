## ----setup, include = FALSE---------------------------------------------------
library(nhdplusTools)

local <- (Sys.getenv("BUILD_VIGNETTES") == "TRUE")
if(local) {
  cache_path <- file.path(nhdplusTools_data_dir(), "nhdpt_v_cache")
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


## ----tldr---------------------------------------------------------------------
# Uncomment to install!
# install.packages("nhdplusTools")

library(nhdplusTools)
library(sf)

start_point <- st_sfc(st_point(c(-89.362239, 43.090266)), crs = 4269)
start_comid <- discover_nhdplus_id(start_point)

flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 1000)

subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = flowline$UT$nhdplus_comid,
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

flowline <- subset$NHDFlowline_Network
catchment <- subset$CatchmentSP
waterbody <- subset$NHDWaterbody

## Or:

flowline <- sf::read_sf(subset_file, "NHDFlowline_Network")
catchment <- sf::read_sf(subset_file, "CatchmentSP")
waterbody <- sf::read_sf(subset_file, "NHDWaterbody")

plot(sf::st_geometry(flowline), col = "blue")
plot(start_point, cex = 1.5, lwd = 2, col = "red", add = TRUE)
plot(sf::st_geometry(catchment), add = TRUE)
plot(sf::st_geometry(waterbody), col = rgb(0, 0, 1, alpha = 0.5), add = TRUE)

## ----install, echo = TRUE, eval = FALSE---------------------------------------
#  # install.packages("devtools")
#  # devtools::install_github("usgs-r/nhdplusTools")

## ----load---------------------------------------------------------------------
library(nhdplusTools)

## ----nhdplus_path_setup, echo=FALSE, include=FALSE----------------------------
work_dir <- file.path(nhdplusTools_data_dir(), "nhdpt_v_cache")

dir.create(work_dir, showWarnings = FALSE, recursive = TRUE)

source(system.file("extdata/sample_data.R", package = "nhdplusTools"))

file.copy(sample_data,
          file.path(work_dir, "natseamless.gpkg"))

## ----nhdplus_path, echo=TRUE--------------------------------------------------
nhdplus_path(file.path(work_dir, "natseamless.gpkg"))

nhdplus_path()

## ----stage_national_data------------------------------------------------------
staged_data <- stage_national_data(output_path = tempdir())

str(staged_data)

## ----staged_data--------------------------------------------------------------
flowline <- readRDS(staged_data$flowline)
names(flowline)[1:10]

library(sf)
plot(sf::st_geometry(flowline))

## ----nhdplushr_secret, echo=FALSE, include=FALSE------------------------------
source(system.file("extdata/nhdplushr_data.R", package = "nhdplusTools"))

## ----nhdplus_hr---------------------------------------------------------------
download_nhdplushr(nhd_dir = "download_dir", 
                   hu_list = c("0101"), # can mix hu02 and hu04 codes.
                   download_files = FALSE) # TRUE will download files.

out_gpkg <- file.path(work_dir, "nhd_hr.gpkg")
hr_data <- get_nhdplushr(work_dir, 
                         out_gpkg = out_gpkg)
(layers <- st_layers(out_gpkg))
names(hr_data)
unlink(out_gpkg)

hr_data <- get_nhdplushr(work_dir, 
                         out_gpkg = out_gpkg, 
                         layers = NULL)
(layers <- st_layers(out_gpkg))
names(hr_data)

## ----point--------------------------------------------------------------------
lon <- -89.362239
lat <- 43.090266

start_point <- sf::st_sfc(sf::st_point(c(lon, lat)),
                          crs = 4269)

plot(sf::st_geometry(flowline))
plot(start_point, cex = 1.5, lwd = 2, col = "red", add = TRUE)

## ----discover_nhdplus_id------------------------------------------------------
start_comid <- discover_nhdplus_id(start_point)
start_comid

## ----discover_nldi------------------------------------------------------------
dataRetrieval::get_nldi_sources()$source

nldi_feature <- list(featureSource = "comid", featureID = start_comid)

get_nldi_feature(nldi_feature)

## ----navigate_nldi------------------------------------------------------------

flowline_nldi <- navigate_nldi(nldi_feature, 
                               mode = "upstreamTributaries", 
                               distance_km = 1000)

plot(sf::st_geometry(flowline), lwd = 3, col = "black")
plot(sf::st_geometry(flowline_nldi$origin), lwd = 3, col = "red", add = TRUE)
plot(sf::st_geometry(flowline_nldi$UT), lwd = 1, col = "red", add = TRUE)

## ----subset_nhdplus_download--------------------------------------------------
output_file_download <- file.path(work_dir, "subset_download.gpkg")

output_file_download <-subset_nhdplus(comids = flowline_nldi$UT$nhdplus_comid,
                                      output_file = output_file_download,
                                      nhdplus_data = "download", return_data = FALSE,
                                      overwrite = TRUE)

sf::st_layers(output_file_download)

flowline_download <- sf::read_sf(file.path(work_dir, "subset_download.gpkg"), 
                                 "NHDFlowline_Network")

plot(sf::st_geometry(dplyr::filter(flowline_download, 
                                   streamorde > 2)), 
     lwd = 7, col = "darkgrey")
plot(sf::st_geometry(flowline_nldi$UT), 
     lwd = 3, col = "red", add = TRUE)

## ----nldi_nwissite------------------------------------------------------------
nldi_feature <- list(featureSource = "nwissite", featureID = "USGS-05428500")

flowline_nldi <- navigate_nldi(nldi_feature, 
                               mode = "upstreamTributaries", 
                               distance_km = 1000)

output_file_nwis <- file.path(work_dir, "subset_download_nwis.gpkg")

output_file_nwis <-subset_nhdplus(comids = flowline_nldi$UT$nhdplus_comid,
                                  output_file = output_file_nwis,
                                  nhdplus_data = "download",
                                  return_data = FALSE, overwrite = TRUE)

sf::st_layers(output_file_download)

flowline_nwis <- sf::read_sf(output_file_nwis, 
                                 "NHDFlowline_Network")

upstream_nwis <- navigate_nldi(nldi_feature,
                               mode = "upstreamTributaries",
                               data_source = "nwissite", 
                               distance_km = 1000)

plot(sf::st_geometry(flowline_nwis), 
     lwd = 3, col = "blue")
plot(sf::st_geometry(upstream_nwis$UT_nwissite), 
     cex = 1, lwd = 2, col = "red", add = TRUE)

## ----get_UT-------------------------------------------------------------------
UT_comids <- get_UT(flowline, start_comid)
UT_comids

## ----plot_fline_subset--------------------------------------------------------
plot(sf::st_geometry(flowline))
plot(start_point, cex = 1.5, lwd = 2, col = "red", add = TRUE)
plot(sf::st_geometry(dplyr::filter(flowline, COMID %in% UT_comids)),
     add=TRUE, col = "red", lwd = 2)

## ----subset_nhdplus-----------------------------------------------------------
output_file <- file.path(work_dir, "subset.gpkg")

output_file <-subset_nhdplus(comids = UT_comids,
                             output_file = output_file,
                             nhdplus_data = nhdplus_path(), 
                             return_data = FALSE, overwrite = TRUE)

sf::st_layers(output_file)

## ----plot_result--------------------------------------------------------------
catchment <- sf::read_sf(output_file, "CatchmentSP")
waterbody <- sf::read_sf(output_file, "NHDWaterbody")

plot(sf::st_geometry(flowline))
plot(start_point, cex = 1.5, lwd = 2, col = "red", add = TRUE)
plot(sf::st_geometry(dplyr::filter(flowline, COMID %in% UT_comids)),
     add=TRUE, col = "red", lwd = 2)
plot(sf::st_geometry(catchment), add = TRUE)
plot(sf::st_geometry(waterbody), col = rgb(0, 0, 1, alpha = 0.5), add = TRUE)

## ----indexing-----------------------------------------------------------------
get_flowline_index(flowline, start_point)

## ----index_list---------------------------------------------------------------
gage <- sf::read_sf(output_file, "Gage")

get_flowline_index(flowline, sf::st_geometry(gage), precision = 10)

## ----teardown, include=FALSE--------------------------------------------------
options(oldoption)

if(Sys.getenv("BUILD_VIGNETTES") != "TRUE") {
  unlink(work_dir, recursive = TRUE)
}

