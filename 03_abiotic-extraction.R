DATADIR = "/Users/anjumgujral/Box Sync/yuba-phys_data"

library(sf)
library(terra)
library(dplyr)

dem = terra::rast(file.path(DATADIR, "dem", "dem.tif"))

candidate = sf::st_read(file.path(DATADIR, "candidate-field-sites" ,"candidate_poly.gpkg"))
plot(dem)
plot(candidate)

# extract elevation for poloygons 

candidate_transf =  st_transform(candidate, st_crs(dem))
elev_extracted = terra::extract(dem, candidate_transf)

summ = elev_extracted |>
  group_by(ID) |>
  summarize(min_elev = min(dem),
            max_elev = max(dem),
            mean_elev = mean(dem))

# candidate_cent = st_centroid(candidate_transf)
# elev_extracted = terra::extract(candidate_cent, dem)

candidate_transf$elev_mean = summ$mean_elev
plot(candidate_transf)

candidate_transf


#prism vpd max and vpd (30 year normals, maximum annual vpd)
#vpd should be highly correlated with elevation 





