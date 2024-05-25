# Purpose: Simplify and union the FACTS polygons for KML export in a form that is not too complex it
# will bog down Avenza

## SETUP

# Load packages

library(terra)
library(sf)
library(dplyr)
library(ggplot2)

# Specify the root of the data directory
DATADIR = "/Users/anjumgujral/Box Sync/yuba-phys_data"
#DATADIR = "/home/derek/Documents/repo-data-local/yuba-phys_data/"

## LOAD DATA (including some reprojection and initial visualization)

# FACTS past treatment data (created by script "01_facts-data-carpentry.R")
facts = st_read(file.path(DATADIR, "facts", "processed", "facts_thinning_focal.gpkg"))
planned = st_read(file.path(DATADIR, "future-treatment-polys", "TNC_DataRequest_Mar15.2024.gdb"))

planned = st_layers(file.path(DATADIR, "future-treatment-polys", "TNC_DataRequest_Mar15.2024.gdb"))



# Simplyfy the FACTS polygons as much as possible
f1 = st_transform(planned, 3310)
f2 = st_buffer(f1, dist = 30)
f3 = st_union(f2)
f4 = st_simplify(f3, dTolerance = 10)
st_write(f4, dsn = file.path(DATADIR, "tmp", "planned_simplified.kml"), delete_dsn = TRUE)   
