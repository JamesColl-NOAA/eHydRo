## ----setup, include = FALSE---------------------------------------------------
library(nhdplusTools)

local <- (Sys.getenv("BUILD_VIGNETTES") == "TRUE")
if(local) {
  cache_path <- file.path(nhdplusTools_data_dir(), "plot_v_cache")
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


## ----data_dir_setup, echo=FALSE, include=FALSE--------------------------------
work_dir <- file.path(nhdplusTools_data_dir(), "plot_v_cache")
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
library(rosm)
set_default_cachedir(work_dir)
library(nhdplusTools)

## ----nwis_simple1, message=FALSE----------------------------------------------
plot_nhdplus("05428500")

## ----nwis_simple2, message=FALSE----------------------------------------------
plot_nhdplus(list(list("nwissite", "USGS-05428500"),
                  list("huc12pp", "070900020602")))

## ----two_outlets, message=FALSE-----------------------------------------------
plot_nhdplus(list(list("nwissite", "USGS-05428500"),
                  list("huc12pp", "070900020602")))

## ----point_location, message=FALSE--------------------------------------------
start_point <- sf::st_as_sf(data.frame(x = -89.36, y = 43.09), 
                            coords = c("x", "y"), crs = 4326)

plot_nhdplus(start_point)

## ----plot_styles, message=FALSE-----------------------------------------------

source(system.file("extdata/sample_data.R", package = "nhdplusTools"))

plot_nhdplus(list(list("comid", "13293970"),
                  list("nwissite", "USGS-05428500"),
                  list("huc12pp", "070900020603"),
                  list("huc12pp", "070900020602")),
             streamorder = 2,
             nhdplus_data = sample_data,
             plot_config = list(basin = list(lwd = 2),
                                outlets = list(huc12pp = list(cex = 1.5),
                                               comid = list(col = "green"))),
             stoponlargerequest = FALSE)

## ----bbox_plotting, message=FALSE---------------------------------------------
bbox <- sf::st_bbox(c(xmin = -89.56684, ymin = 42.99816, xmax = -89.24681, ymax = 43.17192),
                    crs = "+proj=longlat +datum=WGS84 +no_defs")
plot_nhdplus(bbox = bbox)

## ----get data-----------------------------------------------------------------
library(sf)
library(nhdplusTools)
nwissite <- list(featureSource = "nwissite", 
                     featureID = "USGS-05428500")

flowline <- navigate_nldi(nwissite, 
                          mode = "upstreamTributaries", 
                          data_source = "flowlines")

nhdplus <- subset_nhdplus(comids = flowline$UT$nhdplus_comid,
                          output_file = file.path(work_dir, "nhdplus.gpkg"),
                          nhdplus_data = "download",
                          overwrite = TRUE, return_data = FALSE)

flowline <- read_sf(nhdplus, "NHDFlowline_Network")

upstream_nwis <- navigate_nldi(nwissite,
                               mode = "upstreamTributaries",
                               data_source = "nwissite")

basin <- get_nldi_basin(nwissite)

## ----introspect---------------------------------------------------------------
st_layers(nhdplus)
names(flowline)
names(upstream_nwis)
names(basin)
class(st_geometry(flowline))
class(st_geometry(upstream_nwis$UT_nwissite))
class(st_geometry(basin))

## ----bbox, message=FALSE------------------------------------------------------
library(sp)

sf_bbox <- st_bbox(basin)
sf_bbox
class(sf_bbox)

sp_bbox <- sp::bbox(sf::as_Spatial(basin))
sp_bbox
class(sp_bbox)

# Or without the sp::bbox
sp_bbox <- matrix(sf_bbox, 
                  byrow = FALSE, 
                  ncol = 2, 
                  dimnames = list(c("x", "y"), 
                                  c("min", "max")))
sp_bbox

ggmap_bbox <- setNames(sf_bbox, c("left", "bottom", "right", "top"))
ggmap_bbox

## ----plot---------------------------------------------------------------------
prep_layer <- function(x) st_geometry(st_transform(x, 3857))

prettymapr::prettymap({
  rosm::osm.plot(sp_bbox, type = "cartolight", quiet = TRUE, 
                 progress = "none", cachedir = work_dir)
  
  plot(prep_layer(basin), 
       lwd = 2, add = TRUE)
  
  plot(prep_layer(flowline), 
       lwd = 1.5, col = "deepskyblue", add = TRUE)
  
  plot(prep_layer(dplyr::filter(flowline, streamorde > 2)), 
       lwd = 3, col = "darkblue", add = TRUE)
  
  us_nwis_layer <- prep_layer(upstream_nwis)
  
  plot(us_nwis_layer, 
       pch = 17, cex = 1.5, col = "yellow", add = TRUE)
  
  label_pos <- st_coordinates(us_nwis_layer)
  
  text(label_pos[,1],label_pos[,2], 
       upstream_nwis$identifier, 
       adj = c(-0.2, 0.5), cex = 0.7)
  
}, drawarrow = TRUE)

## ----ggmap, message=FALSE, warning=FALSE--------------------------------------
library(ggmap)
library(ggplot2)

upstream_nwis <- dplyr::bind_cols(upstream_nwis$UT_nwissite,
                           dplyr::rename(dplyr::as_tibble(sf::st_coordinates(upstream_nwis$UT_nwissite)), 
                                         lat = Y, lon = X))

basemap_toner <- get_map(source = "stamen", maptype = "toner", 
                         location = ggmap_bbox, zoom = 11, messaging = FALSE)
basemap_terrain <- get_map(source = "stamen", maptype = "terrain-lines", 
                           location = ggmap_bbox, zoom = 11, messaging = FALSE)
toner_map <- ggmap(basemap_toner)
terrain_map <- ggmap(basemap_terrain)

toner_map

terrain_map + geom_sf(data = basin,
                        inherit.aes = FALSE,
                        color = "black", fill = NA) + 
  geom_sf(data = flowline,
          inherit.aes = FALSE,
          color = "deepskyblue") +
  geom_sf(data = dplyr::filter(flowline, streamorde > 2),
          inherit.aes = FALSE,
          color = "darkblue") +
  geom_sf(data = upstream_nwis, inherit.aes = FALSE, color = "red") + 
  geom_text(data = upstream_nwis, aes(label = identifier, x = lon, y = lat),
            hjust = 0, size=2.5, nudge_x = 0.02, col = "black")

## ----teardown, include=FALSE--------------------------------------------------
options(oldoption)

if(Sys.getenv("BUILD_VIGNETTES") != "TRUE") {
  unlink(work_dir, recursive = TRUE)
}

