## ----setup, include=FALSE, echo=FALSE-----------------------------------------
################################################################################
#Load packages
################################################################################
library("sp")
library("raster")
library("knitr")
library("elevatr")
library("httr")
library("sf")
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(purl = NOT_CRAN, 
                      eval = NOT_CRAN,
                      fig.width = 4, 
                      fig.height = 4, 
                      tidy = TRUE,
                      dpi = 100)

