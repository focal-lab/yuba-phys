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

counts_table <- trait_data1 %>%
  filter(
    species %in% c('ABCO', 'ABMA', 'PIPO', 'PILA'),
    size_class %in% c('seedling', 'sapling')
  ) %>%
  select(species, size_class, plot) %>%
  table()


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

# counts for midday measurements 
midday_summary_table <- trait_data1 %>%
  filter(!is.na(midday_MPa)) %>%  # Only include rows where midday_MPa has data
  group_by(elevation, microsite, size_class, species) %>%
  summarise(count = n())

## create a table to summarize missing data
missing_heights_by_species <- trait_data1 %>%
  group_by(species, elevation) %>%
  summarize(
    missing_heights = sum(is.na(seedling_height_cm) | seedling_height_cm == ""),
    .groups = 'drop'
  )


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


# create DBH intervals and count DBH by species and elevation
DBH_intervals <- trait_data1 %>%
  mutate(DBH_interval = cut(
    diameter_cm,
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

trait_data1$diameter <- as.numeric(as.character(trait_data1$diameter_cm))

hist(trait_data1$diameter)

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

plot(trait_data1$diameter~trait_data1$seedling_height_cm)
mod1 <- lm(trait_data1$diameter~trait_data1$seedling_height_cm)
summary(mod1)

# create a training model using seedlings with height <= 100cm 
complete_cases <- trait_data1 %>%
  filter(!is.na(diameter) & seedling_height_cm <= 100)

missing_cases <- trait_data1 %>% filter(is.na(diameter))
mod2 <- lm(diameter ~ seedling_height_cm, data = complete_cases)
summary(mod2) # r^2 decreased when filtering to <= 100cm

trait_data1$predicted_diameter <- NA

if (nrow(missing_cases) > 0) {
  predicted_values <- predict(mod2, newdata = missing_cases)
  trait_data1$predicted_diameter[is.na(trait_data1$diameter)] <- predicted_values
}

# create a new column that combines observed and predicted DBH values
trait_data1$diameter_complete <- ifelse(is.na(trait_data1$diameter), trait_data1$predicted_diameter, 
                                        trait_data1$diameter)

library('ggplot2')
#compare trends between DBH and seedling height for actual and predicted data
ggplot(trait_data1, aes(x = seedling_height_cm, y = diameter)) +
  geom_point(color = "blue") +
  geom_point(aes(y = predicted_diameter), color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
  geom_smooth(aes(y = predicted_diameter), method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

# are predicted and observed DBH values statistically different?
t_test <- t.test(
  trait_data1$DBH[!is.na(trait_data1$diameter) & !is.na(trait_data1$predicted_diameter)],
  trait_data1$predicted_diameter[!is.na(trait_data1$diameter) & !is.na(trait_data1$predicted_diameter)],
  paired = TRUE
)
print(t_test)

# preliminary analyses
write.csv(trait_data1, "trait_data1.csv", row.names = FALSE)


trait_data1$P50_MPa <- as.numeric(as.character(trait_data1$P50_MPa))

# interpolate P50 values for individuals that P50 wasnt measure
# create a training dataset
P50_measured <- trait_data1 %>%
  filter(!is.na(P50_MPa))

P50_missing <- trait_data1 %>% filter(is.na(P50_MPa))

mod3 <- lm(P50_MPa ~ species + size + percent_cover + elev_indiv, data = P50_measured)
summary(mod3)

trait_data1$predicted_P50 <- NA

if (nrow(P50_missing) > 0) {
  P50_predicted_values <- predict(mod3, newdata = P50_missing)
  trait_data1$predicted_P50[is.na(trait_data1$P50_MPa)] <- P50_predicted_values
}

# create a new column that combines observed and predicted DBH values
trait_data1$P50_complete <- ifelse(is.na(trait_data1$P50_MPa), trait_data1$predicted_P50, 
                                        trait_data1$P50_MPa)

#compare trends between P50 measured and P50 predicted
# Base plot

# Plot both measured and predicted P50 values over size (or diameter)
ggplot() +
  # Measured P50 points
  geom_point(
    data = trait_data1 %>% filter(!is.na(P50_MPa)),
    aes(x = diameter_complete, y = P50_MPa, color = "Measured P50"),
    alpha = 0.7
  ) +
  # Predicted P50 points (generated on the fly)
  geom_point(
    data = trait_data1 %>% filter(is.na(P50_MPa)),
    aes(x = diameter_complete, y = predict(mod3, newdata = trait_data1 %>% filter(is.na(P50_MPa))), color = "Predicted P50"),
    alpha = 0.7
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Measured P50" = "steelblue", "Predicted P50" = "darkorange")
  ) +
  labs(
    title = "Measured and Predicted P50 Values vs. Size",
    x = "Size (e.g., Diameter)",
    y = "P50 (MPa)"
  ) +
  theme_minimal()


trait_data1$HSM_predawn_2024 <- trait_data1$P50_MPa - trait_data1$predawn_MPa_2024
trait_data1$HSM_midday_2024  <- trait_data1$P50_MPa - trait_data1$midday_MPa_2024

trait_data1$HSM_predawn_2025 <- trait_data1$P50_MPa - trait_data1$predawn_MPa_2025
trait_data1$HSM_midday_2025 <- trait_data1$P50_MPa - trait_data1$midday_MPa_2025

trait_data1 <- trait_data1 %>%
  mutate(size = ifelse(diameter_complete < 2.5, "seedling", "sapling"))

trait_data1 <- trait_data1[, !(names(trait_data1) %in% "HSM_MPa")]

## see PLC script to make sure P50 means are loaded 

trait_data1$HSM_predawn_P50_mean_2024 <- trait_data1$P50_mean - trait_data1$predawn_MPa_2024
trait_data1$HSM_midday_P50_mean_2024  <- trait_data1$P50_mean - trait_data1$midday_MPa_2024

trait_data1$HSM_predawn_P50_mean_2025 <- trait_data1$P50_mean - trait_data1$predawn_MPa_2025
trait_data1$HSM_midday_P50_mean_2025  <- trait_data1$P50_mean - trait_data1$midday_MPa_2025

ABCO <- trait_data1[trait_data1$species == 'ABCO',]
PILA <- trait_data1[trait_data1$species == 'PILA',]
PIPO <- trait_data1[trait_data1$species == 'PIPO',]
ABMA <- trait_data1[trait_data1$species == 'ABMA',]


trait_data2 <- trait_data1 %>%
  pivot_longer(
    cols = c(predawn_MPa_2025, midday_MPa_2025, P50_MPa, P50_mean, P50_complete, HSM_predawn_2025, HSM_midday_2025),
    names_to = "type",
    values_to = "water_potential"
  )




