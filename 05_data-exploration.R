DATADIR = "/Users/anjumgujral/Box Sync/yuba-phys_data/traits"
trait_data <- read.csv("/Users/anjumgujral/Box Sync/yuba-phys_data/traits/yuba-phys-data.csv")

library('dplyr')
library('tidyverse')

## subset dataframe to include only seedlings and saplings for ABCO, PIPO, PILA, and ABMA
trait_data1 <- trait_data[trait_data$size_class %in% c('seedling', 'sapling'), ]
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

# create DBH intervals and count DBH by species and elevation
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

trait_data1$DBH <- as.numeric(as.character(trait_data1$DBH_cm))

hist(trait_data1$DBH_cm)

# make a table to visualize the representation of vulnerability curves across species, 
#size class, elevation and microsite

P50_microsite_table <- trait_data1 %>%
  filter(branch_processed == 'yes') %>%
  group_by(species, elevation, microsite, size_class) %>%
  summarize(count = n(), .groups = 'drop')

## P50 representation is low for AMBA high elev sun seedling, PIPO high and low elev shade seedling, 
# PILA low elev sun sapling

P50_elev_table <- trait_data1 %>%
  filter(branch_processed == 'yes') %>%
  group_by(species, elevation, size_class) %>%
  summarize(count = n(), .groups = 'drop')

plot(trait_data1$DBH~trait_data1$seedling_height_cm)
mod1 <- lm(trait_data1$DBH~trait_data1$seedling_height_cm)
summary(mod1)

# create a training model using seedlings with height <= 100cm 
complete_cases <- trait_data1 %>%
  filter(!is.na(DBH) & seedling_height_cm <= 100)

missing_cases <- trait_data1 %>% filter(is.na(DBH))
mod2 <- lm(DBH ~ seedling_height_cm, data = complete_cases)
summary(mod2) # r^2 decreased when filtering to <= 100cm


trait_data1$predicted_DBH <- NA

if (nrow(missing_cases) > 0) {
  predicted_values <- predict(mod2, newdata = missing_cases)
  trait_data1$predicted_DBH[is.na(trait_data1$DBH)] <- predicted_values
}

# create a new column that combines observed and predicted DBH values
trait_data1$DBH_complete <- ifelse(is.na(trait_data1$DBH), trait_data1$predicted_DBH, trait_data1$DBH)

library('ggplot2')
#compare trends between DBH and seedling height for actual and predicted data
ggplot(trait_data1, aes(x = seedling_height_cm, y = DBH)) +
  geom_point(color = "blue") +
  geom_point(aes(y = predicted_DBH), color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
  geom_smooth(aes(y = predicted_DBH), method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

# preliminary analyses

# do predawn and midday water potentials differ by microsite, species, and elevation?
ggplot(trait_data1, aes(x = microsite, y = midday_MPa, fill = species)) +
  geom_boxplot(position = position_dodge(0.8), outlier.shape = NA) +  
  facet_wrap(~ elevation) +
  labs(
       x = "Microsite",
       y = "Midday Water Potential (-MPa)",
       fill = "Species") +
  scale_y_reverse() +  # Reverse the y-axis
  theme_bw() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank())


ggplot(trait_data1, aes(x = microsite, y = predawn_MPa, fill = species)) +
  geom_boxplot(position = position_dodge(0.8), outlier.shape = NA) +  
  facet_wrap(~ elevation) +
  labs(
    x = "Microsite",
    y = "Predawn Water Potential (-MPa)",
    fill = "Species") +
  scale_y_reverse() +  # Reverse the y-axis
  theme_bw() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank())

# high elevation plots are oddly more dry, could this be because of stand basal area or 
# heat load index ?
plot(trait_data1$midday_MPa~trait_data1$stand_basal_area)

# what about differences between size classes?
ggplot(trait_data1, aes(x = size_class, y = predawn_MPa, fill = species)) +
  geom_boxplot(position = position_dodge(0.8), outlier.shape = NA) +  
  facet_wrap(~ elevation) +
  labs(
    x = "Size",
    y = "Predawn Water Potential (-MPa)",
    fill = "Species") +
  scale_y_reverse() +  # Reverse the y-axis
  theme_bw() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank())

ggplot(trait_data1, aes(x = size_class, y = midday_MPa, fill = species)) +
  geom_boxplot(position = position_dodge(0.8), outlier.shape = NA) +  
  facet_wrap(~ elevation) +
  labs(
    x = "Size",
    y = "Midday Water Potential (-MPa)",
    fill = "Species") +
  scale_y_reverse() +  # Reverse the y-axis
  theme_bw() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank())

# is midday WP different across size, species, microsite, and elevation
ggplot(trait_data1, aes(x = microsite, y = midday_MPa, fill = interaction(size_class, species))) +
  geom_boxplot(position = position_dodge(0.8), outlier.shape = NA) +  
  facet_wrap(~ elevation) +
  labs(
    x = "Microsite",
    y = "Midday Water Potential (-MPa)",
    fill = "Species and size") +
  scale_y_reverse() +  # Reverse the y-axis
  theme_bw() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank())

# run a pairwise wilcoxon rank sum test to determine whether WP is significantly different 
# between groups 

# generate a heat load index to test whether heat load index explains variation in WP 
# across elevations


