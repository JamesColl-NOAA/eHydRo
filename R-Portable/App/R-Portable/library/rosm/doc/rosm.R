## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 4,
  fig.height = 4
)

## ---- message=FALSE------------------------------------------------------
library(rosm)
library(prettymapr)

## ---- message=FALSE------------------------------------------------------
altalake <- searchbbox("alta lake, BC")
# or
altalake <- makebbox(50.1232, -122.9574, 50.1035, -123.0042)

## ---- message=FALSE, results='hide', fig.show='hold'---------------------
osm.plot(altalake)

## ------------------------------------------------------------------------
osm.types()

## ---- message=FALSE, results='hide', fig.show='hold'---------------------
osm.plot(altalake, type="stamenbw")

## ------------------------------------------------------------------------
bmaps.types()

## ---- message=FALSE, results='hide', fig.show='hold'---------------------
bmaps.plot(altalake, type="AerialWithLabels")

## ---- message=FALSE, results='hide', fig.show='hold'---------------------
bmaps.plot(altalake, zoomin=1) # res=300 will also work

## ---- message=FALSE, results='hide', fig.show='hold'---------------------
osm.plot(altalake)
osm.points(c(-122.9841, -122.9812), c(50.11055, 50.11765), 
           pch=15, cex=0.6)
osm.text(c(-122.9841, -122.9812), c(50.11055, 50.11765), 
         labels=c("GC6", "GC2"), adj=c(-0.2, 0.5), cex=0.5)

## ---- message=FALSE, warning=FALSE, results='hide', fig.show='hold'------
altalake <- makebbox(50.1232, -122.9574, 50.1035, -123.0042)
prettymap({
  bmaps.plot(altalake)
  osm.points(c(-122.9841, -122.9812), c(50.11055, 50.11765), 
             pch=15, cex=0.6, col="white")
  osm.text(c(-122.9841, -122.9812), c(50.11055, 50.11765), 
           labels=c("GC6", "GC2"), adj=c(-0.2, 0.5), cex=0.7, col="white")
}, scale.label.col = "white")

