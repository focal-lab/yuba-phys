# Purpose: Compile USFS treatment data from FACTS database (search for "FACTS" at: https://data.fs.usda.gov/geodata/edw/datasets.php), and filter to focal treatment types, focal project area, and focal date range

## SETUP

# Load packages

library(terra)
library(sf)
library(dplyr)
library(ggplot2)

# Specify the root of the data directory

DATADIR = "/home/derek/Documents/repo-data-local/yuba-phys_data/"

## LOAD DATA

# FACTS data on past treatments

facts_fuel = st_read(file.path(DATADIR, "facts", "raw", "S_USA.Activity_HazFuelTrt_PL", "S_USA.Activity_HazFuelTrt_PL.shp"))
facts_silv = st_read(file.path(DATADIR, "facts", "raw", "S_USA.Activity_SilvTSI", "S_USA.Activity_SilvTSI.shp"))
facts_harv = st_read(file.path(DATADIR, "facts", "raw", "S_USA.Activity_TimberHarvest", "S_USA.Activity_TimberHarvest.shp"))

# Project boundary

bound = sf::st_read(file.path(DATADIR, "boundary", "north_yuba_area.kml")) |>
  select(Name) |>
  sf::st_transform(3310)
# Buffer out by 10 km to include vicinity
bound_buf = sf::st_buffer(bound, dist = 5000)


## PROCESSING

# Thin to Tahoe and Plumas NFs and select relevant data columns, standardizing names to be consistent
facts_fuel2 = facts_fuel |>
  filter(PROC_REGIO == "05", PROC_FORES %in% c("17", "11")) |>
  select(ACTIVITY, LAND_SUITA, PRODUCTIVI, DATE_PLANN, DATE_AWARD, DATE_COMPL, METHOD, METHOD_COD, TREATMENT_)
facts_silv2 = facts_silv |>
  filter(REGION_COD == "05", ADMIN_FORE %in% c("17", "11")) |>
  select(ACTIVITY = ACTIVITY_N, LAND_SUITA, PRODUCTIVI, DATE_PLANN, DATE_AWARD, DATE_COMPL, METHOD = METHOD_DES, METHOD_COD, TREATMENT_)
facts_harv2 = facts_harv |>
  filter(ADMIN_REGI == "05", ADMIN_FORE %in% c("17", "11")) |>
  select(ACTIVITY = ACTIVITY_N, LAND_SUITA, PRODUCTIVI, DATE_PLANN, DATE_AWARD, DATE_COMPL, METHOD = METHOD_DES, METHOD_COD, TREATMENT_)

# Combine into a single (spatial) data frame
facts = bind_rows(facts_fuel2,
                  facts_silv2,
                  facts_harv2)

# Display the treatment types
table(facts$ACTIVITY) |> sort()

# Constrain to recent treatments
facts = facts |>
  filter(DATE_COMPL > "2014-01-01")

# Clip to N Yuba (including specified buffer)
facts_foc = st_make_valid(facts)
facts_foc = sf::st_intersection(facts_foc, bound_buf |> st_transform(st_crs(facts)))

# Display the activities
table(facts_foc$ACTIVITY) |> sort(decreasing = TRUE)

# Thin to focal activities
activ_foc = c("Commercial Thin",
              "Precommercial Thin",
              "Thinning for Hazardous Fuels Reduction",
              "Group Selection Cut (UA/RH/FH)",
              "Underburn - Low Intensity (Majority of Unit)",
              "Salvage Cut (intermediate treatment, not regeneration)",
              "Broadcast Burning - Covers a majority of the unit",
              "Fuel Break",
              "Wildlife Habitat Precommercial thinning"
)

# Activity types that could potentially involve density reduction but I decided to exclude -- could revisit
# "Yarding - Removal of Fuels by Carrying or Dragging"
# "Single-tree Selection Cut (UA/RH/FH)"
# "Sanitation Cut"
# "Fuel Break"

facts_foc = facts_foc |>
  filter(ACTIVITY %in% activ_foc)

# Write out the file
st_write(facts_foc, file.path(DATADIR, "facts", "processed", "facts_thinning_focal.gpkg"), delete_dsn = TRUE)
