## ----setup, include = FALSE---------------------------------------------------
library(nhdplusTools)

local <- (Sys.getenv("BUILD_VIGNETTES") == "TRUE")
if(local) {
  cache_path <- file.path(nhdplusTools_data_dir(), "hr_v_cache")
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
library(nhdplusTools)
library(sf)

work_dir <- file.path(nhdplusTools_data_dir(), "hr_v_cache")

source(system.file("extdata/sample_data.R", package = "nhdplusTools"))

hr_gpkg <- file.path(work_dir, "hr_data.gpkg")

# Make a plot and get some background NHDPlusV2 data.
plot_data <- plot_nhdplus(list("nwissite", "USGS-05428500"), streamorder = 3,
                          nhdplus_data = sample_data,
                          stoponlargerequest = FALSE)

# Find the HU04 we are interested in.
hu04 <- unique(substr(plot_data$flowline$REACHCODE, 1, 4))

# Download some NHDPlusHR Data
hr_data_dir <- download_nhdplushr(work_dir, hu04)

# Projection and simplification for demo purposes.
hr <- get_nhdplushr(work_dir, out_gpkg = hr_gpkg,
                    proj = 3857)

(start_index <- get_flowline_index(st_transform(hr$NHDFlowline, 5070),
                                   st_transform(plot_data$outlets, 5070),
                                   search_radius = 200)) # meters albers eq area

ids <- get_UT(hr$NHDFlowline, start_index$COMID)

hr_subset <- subset_nhdplus(ids, nhdplus_data = hr_gpkg)

## ----plot---------------------------------------------------------------------
plot_nhdplus(list("nwissite", "USGS-05428500"), streamorder = 2, 
             nhdplus_data = sample_data, overwrite = TRUE,
             plot_config = list(flowline = list(lwd = 2.5),
                                basin = list(lwd = 3)),
             stoponlargerequest = FALSE)

plot(st_geometry(hr$NHDPlusCatchment), lwd = 0.25, add = TRUE)
plot(st_geometry(hr$NHDFlowline), col = "blue", lwd = 0.5, add = TRUE)

plot(st_geometry(st_transform(hr_subset$NHDFlowline, 3857)),
     col = "cyan", lwd = 1, add = TRUE)

## ----downloadhr---------------------------------------------------------------
(hr_urls <- download_nhdplushr(work_dir, "06", download_files = FALSE))

# already downloaded:
list.files(hr_data_dir)

## ----get_nhdplushr------------------------------------------------------------
hr <- get_nhdplushr(hr_data_dir)
sapply(hr, class)
plot(st_geometry(hr$NHDFlowline), lwd = (hr$NHDFlowline$StreamOrde / 6))

## ----get_nhdplushr2-----------------------------------------------------------
hr <- get_nhdplushr(hr_data_dir, layers = c("NHDFlowline", "NHDWaterbody", "NHDArea"))
sapply(hr, class)
sapply(hr, nrow)
plot(st_geometry(hr$NHDFlowline), lwd = (hr$NHDFlowline$StreamOrde / 6), col = "blue")

plot(c(st_geometry(hr$NHDWaterbody), st_geometry(hr$NHDArea)), 
     col = "cyan", border = "cyan", lwd = 0.25, add = TRUE)

## ----get_nhdplushr3-----------------------------------------------------------
demo_gpkg <- file.path(work_dir, "demo.gpkg")
hr <- get_nhdplushr(hr_data_dir, out_gpkg = demo_gpkg)
st_layers(demo_gpkg)

## ----get_nhdplushr4-----------------------------------------------------------
demo <- get_nhdplushr(hr_data_dir, layers = "NHDFlowline", 
                               min_size_sqkm = 50)
plot(st_geometry(demo$NHDFlowline), 
     lwd = demo$NHDFlowline$StreamOrde/4, col = "blue")

## ----get_nhdplushr5-----------------------------------------------------------
demo <- get_nhdplushr(hr_data_dir, layers = "NHDFlowline", 
                      min_size_sqkm = 100, 
                      proj = "+init=epsg:5070", simp = 200,
                      keep_cols = c("COMID", "StreamOrde"))
names(demo$NHDFlowline)
plot(st_geometry(demo$NHDFlowline), 
     lwd = demo$NHDFlowline$StreamOrde/4, col = "blue")

## ----make_standalone----------------------------------------------------------
demo <- get_nhdplushr(hr_data_dir, layers = "NHDFlowline", 
                    min_size_sqkm = 100, check_terminals = FALSE)

# Create a standalone basin with the results for comparison.
standalone_demo <- make_standalone(demo$NHDFlowline)

demo_outlet <- dplyr::filter(demo$NHDFlowline, TotDASqKM == max(TotDASqKM)) 

standalone_demo_outlet <- dplyr::filter(standalone_demo, TotDASqKM == max(TotDASqKM))

broken_outlet <- dplyr::select(st_drop_geometry(demo_outlet), 
                               Hydroseq, TerminalPa, TerminalFl, LevelPathI)
fixed_outlet <- dplyr::select(st_drop_geometry(standalone_demo_outlet), 
                              Hydroseq, TerminalPa, TerminalFl, LevelPathI)

print(data.frame(broken_outlet))
print(data.frame(fixed_outlet))

(broken <- dplyr::filter(demo$NHDFlowline, TerminalPa == demo_outlet$Hydroseq))
(standalone <- dplyr::filter(standalone_demo, TerminalPa == standalone_demo_outlet$Hydroseq))

plot(st_geometry(standalone))

## ----teardown, include=FALSE--------------------------------------------------
options(oldoption)

if(Sys.getenv("BUILD_VIGNETTES") != "TRUE") {
  unlink(work_dir, recursive = TRUE)
}

