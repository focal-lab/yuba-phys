# yuba-phys

Tree physiology & functional traits along climatic and forest management gradients in the North Yuba landscape

## Data management approach for this repo

The data are stored in a [Box folder](https://ucdavis.box.com/s/8gnqihv8xhteapc9i7uc3jk9afyniarn). Contact Derek or Anjum for access. Copy this folder to your computer (or sync it using Box Drive), and at the top of each script, set the envrionment variable `DATADIR` to the location of this folder on your computer.

## Analysis approach

**01_facts-data-carpentry.R:** Compile USFS treatment data from FACTS database (search for "FACTS" at: <https://data.fs.usda.gov/geodata/edw/datasets.php>), and filter to focal treatment types, focal project area, and focal date range. Saves result to `{DATADIR}/facts/processed/facts_thinning_focal.gpkg`. Only needs to be run once, unless the input FACTS data from the USFS is updated.

**02_site-selection-data-carpentry.R:** Aggregate and summarize geospatial data into a format that is convenient for informing plot selection. Input data includes past treatments, planned future treatments, topography (elev, SRI, etc), USFS ownership, project area bounds.
