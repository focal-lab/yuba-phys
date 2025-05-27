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

hist(trait_data1$DBH)

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

# are predicted and observed DBH values statistically different?
t_test <- t.test(
  trait_data1$DBH[!is.na(trait_data1$DBH) & !is.na(trait_data1$predicted_DBH)],
  trait_data1$predicted_DBH[!is.na(trait_data1$DBH) & !is.na(trait_data1$predicted_DBH)],
  paired = TRUE
)
print(t_test)

# preliminary analyses
write.csv(trait_data1, "trait_data1.csv", row.names = FALSE)

# merge P50 data with main trait dataframe 

## subset dataframe to include only seedlings and saplings for ABCO, PIPO, PILA, and ABMA
#yuba_phys_P50_2024 <- yuba_phys_P50[yuba_phys_P50$size_class %in% c('seedling', 'sapling'), ]

#yuba_phys_P50_2024 <- yuba_phys_P50_2024[yuba_phys_P50_2024$species %in% c('ABCO', 'ABMA','PIPO', 'PILA'), ]

trait_data1$P50_MPa <- as.numeric(as.character(trait_data1$P50_MPa))

#P50 <- yuba_phys_P50_2024[, c("tree_ID", "P50_MPa", "P50_branch_1", "P50_branch_2", "P50_branch_3")]

#trait_data1 <- trait_data1[, !(names(trait_data1) %in% "P50_MPa")]

#trait_data2 <- merge(trait_data1, P50, by = "tree_ID")

trait_data1$HSM_predawn <- trait_data1$P50_MPa - trait_data1$predawn_MPa
trait_data1$HSM_midday  <- trait_data1$P50_MPa - trait_data1$midday_MPa

trait_data1 <- trait_data1 %>%
  mutate(size = ifelse(DBH_complete < 2.5, "seedling", "sapling"))

trait_data1 <- trait_data1[, !(names(trait_data1) %in% "HSM_MPa")]

## see PLC script to make sure P50 means are loaded 

trait_data1$HSM_predawn_P50_mean <- trait_data1$P50_mean - trait_data1$predawn_MPa
trait_data1$HSM_midday_P50_mean  <- trait_data1$P50_mean - trait_data1$midday_MPa


ABCO <- trait_data1[trait_data1$species == 'ABCO',]
PILA <- trait_data1[trait_data1$species == 'PILA',]
PIPO <- trait_data1[trait_data1$species == 'PIPO',]
ABMA <- trait_data1[trait_data1$species == 'ABMA',]


trait_data2 <- trait_data1 %>%
  pivot_longer(
    cols = c(predawn_MPa, midday_MPa, P50_MPa, P50_mean, HSM_predawn, HSM_midday),
    names_to = "type",
    values_to = "water_potential"
  )




