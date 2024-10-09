DATADIR = "/Users/anjumgujral/Box Sync/yuba-phys_data/traits"
trait_data <- read.csv("/Users/anjumgujral/Box Sync/yuba-phys_data/traits/yuba-phys-data.csv")

library('dplyr')
library('tidyverse')

## subset dataframe to include only seedlings and saplings for ABCO, PIPO, PILA, and ABMA
trait_data1 <- trait_data[trait_data$size_class %in% c('seedling', 'sapling'), ]
trait_data1 <- trait_data[trait_data$size_class == 'seedling', 'sapling',]
trait_data1 <- trait_data1[trait_data1$species %in% c('ABCO', 'ABMA','PIPO', 'PILA'), ]
  
## summary statistics of dataframe

## make a new column turning canopy closure into a categorical variable
trait_data1$microsite <- cut(
  trait_data1$percent_cover,
  breaks = c(0, 68, 85, 100),
  labels = c('sun', 'int', 'shade'),
  right = TRUE
)

## make a table for counts of seedlings and saplings per species
counts_table <- table(trait_data1$species[trait_data1$species %in% c('ABCO', 'ABMA','PIPO','PILA')],
                      trait_data1$size_class[trait_data1$size_class %in% c('seedling', 'sapling')],
                      trait_data1$elevation[trait_data1$elevation %in% c('low', 'high')])

## make a table for counts of seedlings and saplings per species by elevation and microsite
summary_table <- trait_data1 %>%
  filter(size_class %in% c('seedling', 'sapling'), 
         species %in% c('ABCO','ABMA','PIPO','PILA')) %>%
  group_by(species, size_class, elevation, microsite) %>%
  summarize(count = n(), .groups = 'drop')

## counts by elevation
low_elev_counts <- summary_table %>%
  filter(elevation %in% c('low'))

high_elev_counts <- summary_table %>%
  filter(elevation %in% c('high'))

## create a table to summarize missing data
missing_heights_by_species <- trait_data1 %>%
  group_by(species, elevation) %>%
  summarize(
    missing_heights = sum(is.na(seedling_height_cm) | seedling_height_cm == ""),
    .groups = 'drop'
  )

library(dplyr)

# counts by height intervals 
library(dplyr)

# Create height intervals and count heights by species and elevation
height_intervals <- trait_data1 %>%
  mutate(height_interval = cut(
    seedling_height_cm,
    breaks = seq(0, 200, by = 20),
    include.lowest = TRUE,
    right = FALSE,
    labels = paste0(seq(0, 180, by = 20), "-", seq(20, 200, by = 20))
  )) %>%
  group_by(species, elevation, height_interval) %>%
  summarize(
    count_heights = n(),  # Count all rows in each interval
    .groups = 'drop'
  )

hist(trait_data1$seedling_height_cm)

library(dplyr)

# Create DBH intervals and count DBH by species and elevation
DBH_intervals <- trait_data1 %>%
  mutate(DBH_interval = cut(
    DBH_cm,
    breaks = seq(0, 14, by = 2),
    include.lowest = TRUE,
    right = FALSE,
    labels = paste0(seq(0, 12, by = 2), "-", seq(2, 14, by = 2))
  )) %>%
  group_by(species, elevation, DBH_interval) %>%
  summarize(
    count_DBH = n(),  # Count all rows in each interval
    .groups = 'drop'
  )



