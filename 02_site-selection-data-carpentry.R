# Purpose: Aggregate and summarize geospatial data into a format that is convenient for defining plot selection.
# Input data includes past treatments, planned future treatments, topography (elev, SRI, etc), project area bounds

## SETUP

# Load packages

library(terra)
library(sf)
library(dplyr)
library(ggplot2)

# Specify the root of the data directory

DATADIR = "/Users/anjumgujral/Box Sync/yuba-phys_data"

## LOAD DATA (including some reprojection and initial visualization)

# Digital elevation model
dem = terra::rast(file.path(DATADIR, "dem", "dem.tif"))
plot(dem)

# Solar radiation index
sri = terra::rast(file.path(DATADIR, "dem", "sri.tif"))
names(sri) = "sri"
plot(sri)

# Focal area boundary
bound = sf::st_read(file.path(DATADIR, "boundary", "north_yuba_area.kml")) |>
  select(Name)
bound = sf::st_transform(bound, sf::st_crs(dem))
plot(bound, add = TRUE)

# USFS Ownership
own = sf::st_read(file.path(DATADIR, "ownership", "S_USA.PADUS_Fee.shp"))
own = sf::st_transform(own, sf::st_crs(bound))

# FACTS past treatment data (created by script "01_facts-data-carpentry.R")
facts = st_read(file.path(DATADIR, "facts", "processed", "facts_thinning_focal.gpkg"))


# Planned treatment polygons
trt1 = sf::st_read(file.path(DATADIR, "future-treatment-polys", "Mtn_House_Proposal_SC", "Mtn_House_Proposal_SC.shp")) |>
  dplyr::mutate(project = "Mtn_House") |>
  sf::st_transform(3310)

trt2 = sf::st_read(file.path(DATADIR, "future-treatment-polys", "Trapper_PAC_STS_Final", "Trapper_PAC_STS_Layout.shp")) |>
  dplyr::mutate(project = "Trapper_PAC") |>
  sf::st_transform(3310)

# TODO: add more planned treatment datasets provided by TNC/USFS

trt3 = sf::st_read(file.path(DATADIR, "future-treatment-polys", "Trapper_STS_Units_Final", "Trapper_STS_Final.shp")) |>
  dplyr::mutate(project = "Trapper_STS") |>
  sf::st_transform(3310)

trt4 = sf::st_read(file.path(DATADIR, "future-treatment-polys", "Trapper_Sales", "AlaskaPeakIRTC_Official.shp")) |>
  dplyr::mutate(project = "Trapper_Sales_AlaskaPeak") |>
  sf::st_transform(3310)

trt5 = sf::st_read(file.path(DATADIR, "future-treatment-polys", "Trapper_Sales", "GraveyardSale_Draft20220607.shp")) |>
  dplyr::mutate(project = "Trapper_Sales_GraveyardSale") |>
  sf::st_transform(3310)

trt6 = sf::st_read(file.path(DATADIR, "future-treatment-polys", "Trapper_Sales", "SleighvilleTimberSale.shp")) |>
  dplyr::mutate(project = "Trapper_Sales_SleighvilleTimberSale.shp") |>
  sf::st_transform(3310)

trt7 = sf::st_read(file.path(DATADIR, "future-treatment-polys", "Unit_F11b", "Unit_F11b.shp")) |>
  dplyr::mutate(project = "Unit_F11b") |>
  sf::st_transform(3310)

## PROCESSING

# Buffer out the focal area boundary by 5 km to include potentially relevant sites just outside the official area, and add to the DEM plot
bound_buf = sf::st_buffer(bound, dist = 5000)
plot(bound_buf, add = TRUE)

# Optional: crop the DEM to the focal area
# dem = terra::crop(dem, bound)
# dem = terra::mask(dem, bound)

# Crop the USFS ownership layer to the focal area
own = sf::st_intersection(own, bound_buf)
plot(own)

# Select only the necessary ownership columns (others were creating conflicts with downstream processing)
own = own |>
  dplyr::select(UNIT_NM)

# Write out the cropped ownership layer to inspect in QGIS
sf::st_write(own, dsn = file.path(DATADIR, "tmp", "own.gpkg"), delete_dsn = TRUE)


# Combine all the treatment layers that were loaded in the LOAD DATA step into one geospatial data frame
trt = dplyr::bind_rows(trt1, trt2, trt3, trt4, trt5, trt6, trt7)

# Buffer all polygons out by 10 m to remove the tiny gaps between some treatment polygons due to imprecise mapping
trt = st_buffer(trt, dist = 10)

# Merge them into a single polygon, including flattening overlapping polygons into a single polygon
trt_union = sf::st_union(trt)

# Buffer out by 500 m to identify an area around the treatments where we could look for comparable "untreated" units
trt_union_buf = sf::st_buffer(trt_union, dist = 500)

# Remove the area with planned treatment from this buffered region, to create a zone of "no treatment planned" that is near planned treatments
untrt = sf::st_difference(trt_union_buf, trt_union)

# Visualize this "no treatment" region
plot(untrt, col = "blue")

# Exclude non-FS land from the untreated region
own_proj = st_transform(own, st_crs(untrt))
untrt_fs = sf::st_intersection(untrt, own_proj)
plot(untrt_fs, col = "blue")

# Create a "treatment planned" region that includes only USFS land
trt_fs = sf::st_intersection(trt_union, own_proj)
plot(trt_fs, col = "blue")

# Add a "treated" and "untreated" status column to these two datasets
untrt_fs = sf::st_as_sf(untrt_fs)
untrt_fs$status = "no trt planned"

trt_fs = sf::st_as_sf(trt_fs)
trt_fs$status = "trt planned"

# Combine them into a single data frame
focal_area = dplyr::bind_rows(untrt_fs, trt_fs)
plot(focal_area)


# Make a grid to sample candidate plots from
grid = sf::st_make_grid(focal_area, cellsize = 300, what = "centers")
grid = sf::st_as_sf(grid)
plot(grid)
# The above makes a grid within the bounding box. Clip to focal units only and extract the "treated" or "untreated" status
grid = sf::st_intersection(grid, focal_area)
plot(grid)

# Aggregate the DEM and SRI layers to a coarser resolution because we care more about broad-scale topography effects
dem = terra::aggregate(dem, fact = 3) # 30 m orig * 3 = 90 m new resolution
sri = terra::aggregate(sri, fact = 3)

# Extract the elevation at each point
elev = terra::extract(dem, grid)
grid$elev = elev[,2]
plot(grid)



# Visualize the range of elevation for each treatment status
ggplot(grid, aes(x=status, y = elev)) +
  geom_point()

# Write out some of the layers to a temp dir to visually inspect them in QGIS
st_write(grid, dsn = file.path(DATADIR, "tmp", "grid.gpkg"), delete_dsn = TRUE)
st_write(trt_fs, dsn = file.path(DATADIR, "tmp", "trt_fs.gpkg"), delete_dsn = TRUE)

# Classify the DEM into 3 elevation classes (order of vals is lowerbound, upperbound, newvalue)
class_matrix = matrix(c(0, 1000, 1,
                        1000, 2000, 2,
                        2000, 3000, 3),
                      ncol = 3, byrow = TRUE)

dem_class = classify(dem, class_matrix)
plot(dem_class)

# Convert to polygon layer
dem_class_poly = terra::as.polygons(dem_class, values = TRUE)
plot(dem_class_poly)

# Convert from terra's SpatVector format to sf format, which we're more familiar with
dem_class_poly_sf = st_as_sf(dem_class_poly)
plot(dem_class_poly_sf)
dem_class_poly_sf = st_simplify(dem_class_poly_sf, dTolerance = 100)

# Write it to a temp dir for inpsection in QGIS
sf::st_write(dem_class_poly_sf, file.path(DATADIR, "tmp", "elev_poly.gpkg"), delete_dsn = TRUE)


# Classify the SRI layer
# Define 3 SRI classes (order of vals is lowerbound, upperbound, newvalue)
class_matrix = matrix(c(0, 110000, 1,
                        110000, 129000, 2,
                        129000, 200000, 3),
                      ncol = 3, byrow = TRUE)

sri_class = classify(sri, class_matrix)
plot(sri_class)

# Convert to polygon layer
sri_class_poly = terra::as.polygons(sri_class, values = TRUE)
plot(sri_class_poly)

# Convert from terra's SpatVector format to sf format, which we're more familiar with
sri_class_poly_sf = st_as_sf(sri_class_poly)

# Simplify the geometry so there are fewer vertices
sri_class_poly_sf = st_simplify(sri_class_poly_sf, dTolerance = 100)
plot(sri_class_poly_sf)

# Write it to a temp dir for inpsection in QGIS
sf::st_write(sri_class_poly_sf, file.path(DATADIR, "tmp", "sri_poly.gpkg"), delete_dsn = TRUE)


# Create an intersection of the SRI and Elev polygon layers, so we can look at factorial combinations of the two
sri_dem = st_intersection(sri_class_poly_sf, dem_class_poly_sf)

# Create a column that combines the SRI levels (1-3) and DEM levels (1-3)
sri_dem = sri_dem |>
  mutate(sri_dem = paste0("sri-", sri, "_dem-", dem)) |>
  select(sri_dem)

# Extract the SRI X DEM classes for each candidate plot point
grid = st_intersection(grid, sri_dem)

# Visualize
ggplot(grid, aes(x = status, y = sri_dem)) +
  geom_jitter(width = 0.1, height = 0.1)
# We can see that there are more options in SRI-2, so maybe we constrain the plots to SRI class 2 and then just sample along elevation variation within that SRI class, in order to keep as many other variables as possible constant

## Some potential next steps

# Pull in all planned treatment layers from TNC, consider which are focal areas to put plots near, and which are lower-intensity or anomalous treatments we want to avoid. (E.g., the skinny serpentine treatment unit in the example above is clearly a roadside treatment and not a useful treatment for this project, so avoid it.) To exclude the low-intensity or unusual planned treatments, you can use 'st_difference'
# Determine a candidate plot selection strategy. The point-based approach above may not be the best strategy. Another option is to divide the landscape into facets (e.g. low-high elev X low-high SRI) and find areas where these span treatment boundaries. Identify a few candidates of each and go check them out in the field. Potentially put one plot on either side of the planned boundary. Ensure in the field that env conditions seem comparable on both sides.
# Determine the best way to subset the landscape into our focal facets. For example, we know that we have at least two important axes of SRI and elevation. Maybe we hold SRI relatively constant (e.g. moderate north-ish slopes), only focus on those types of sites, and try to find as much elevation variation within that facet.
# Just like we're identifying candidate sites here around planned treatments, do a similar site selection around *previous* treatments. Ideally you would find candidate sites in trios of (1) recently treated, (2) planned treated, and (3) neither that are otherwise comparable.
# Revisit the filtering we are doing to the past treatment ("FACTS") data. Is < 10 y old a good cutoff? Are certain treatment types too low-intensity or unusual to include? This may require some iteration after going to see some candidate sites in the field.

