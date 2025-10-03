DATADIR = "/Users/anjumgujral/Box Sync/yuba-phys_data/traits"
trait_data <- read.csv("/Users/anjumgujral/Box Sync/yuba-phys_data/traits/yuba-phys-data.csv")

library('dplyr')
library('tidyverse')
library('ggplot2')

# subset dataframe to include only seedlings and saplings for ABCO, PIPO, PILA, and ABMA
trait_data1 <- trait_data[trait_data$size_class %in% c('seedling', 'sapling'), ]
trait_data1 <- trait_data1[trait_data1$species %in% c('ABCO', 'ABMA','PIPO', 'PILA'), ]

# make size categorical
trait_data1 <- trait_data1 %>%
  mutate(size = ifelse(diameter_complete < 2.5, "seedling", "sapling"))

# make a new column turning canopy closure into a categorical variable
trait_data1$microsite <- cut(
  trait_data1$percent_cover,
  breaks = c(0, 68, 85, 100),
  labels = c('sun', 'int', 'shade'),
  right = TRUE
)

# create a table to summarize missing data
missing_heights_by_species <- trait_data1 %>%
  group_by(species, elevation) %>%
  summarize(
    missing_heights = sum(is.na(seedling_height_cm) | seedling_height_cm == ""),
    .groups = 'drop'
  )

# determine the relationship between diameter and height to infill missing data
trait_data1$diameter <- as.numeric(as.character(trait_data1$diameter_cm))
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

# create a new column that combines observed and predicted diameter values
trait_data1$diameter_complete <- ifelse(is.na(trait_data1$diameter), trait_data1$predicted_diameter, 
                                        trait_data1$diameter)

#compare trends between diameter and seedling height for actual and predicted data
ggplot(trait_data1, aes(x = seedling_height_cm, y = diameter)) +
  geom_point(color = "blue") +
  geom_point(aes(y = predicted_diameter), color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
  geom_smooth(aes(y = predicted_diameter), method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

# make sure P50 is numeric
trait_data1$P50_MPa_2024 <- as.numeric(as.character(trait_data1$P50_MPa_2024))
trait_data1$P50_MPa_2025 <- as.numeric(as.character(trait_data1$P50_MPa_2025))

# make a new dataframe that pools together 2024 and 2025 data into single trait variables
trait_data2 <- trait_data1 %>%
  pivot_longer(
    cols = c(P50_MPa_2024, P50_MPa_2025,
             HSM_predawn_2024, HSM_midday_2024,
             HSM_predawn_2025, HSM_midday_2025,
             predawn_MPa_2024, predawn_MPa_2025,
             midday_MPa_2024, midday_MPa_2025),
    names_to = c(".value", "year"),
    names_sep = "_(?=20)"  
  )

# interpolate P50 values for individuals that P50 wasn't measured
# create a training dataset
P50_measured <- trait_data2 %>%
  filter(!is.na(P50_MPa))

P50_missing <- trait_data2 %>% filter(is.na(P50_MPa))

mod3 <- lm(P50_MPa ~ species + size + percent_cover + elev_indiv + year, data = P50_measured)
summary(mod3)

trait_data2$predicted_P50 <- NA

if (nrow(P50_missing) > 0) {
  P50_predicted_values <- predict(mod3, newdata = P50_missing)
  trait_data2$predicted_P50[is.na(trait_data2$P50_MPa)] <- P50_predicted_values
}

# create a new column that combines observed and predicted P50 values
trait_data2$P50_complete <- ifelse(is.na(trait_data2$P50_MPa), trait_data2$predicted_P50, 
                                        trait_data2$P50_MPa)

#compare trends between P50 measured and P50 predicted
ggplot() +
  geom_point(
    data = trait_data2 %>% filter(!is.na(P50_MPa)),
    aes(x = diameter_complete, y = P50_MPa, color = species),
    shape = 16, size = 3, alpha = 0.8
  ) +
  geom_point(
    data = trait_data2 %>% filter(is.na(P50_MPa)),
    aes(x = diameter_complete, y = predicted_P50, color = species),
    shape = 17, size = 3, alpha = 0.8
  ) +
  geom_smooth(
    data = trait_data2 %>% filter(!is.na(P50_MPa)),
    aes(x = diameter_complete, y = P50_MPa, color = species, linetype = "Measured"),
    method = "lm", se = TRUE, size = 1, alpha = 0.3
  ) +
  geom_smooth(
    data = trait_data2 %>% filter(is.na(P50_MPa)),
    aes(x = diameter_complete, y = predicted_P50, color = species, linetype = "Predicted"),
    method = "lm", se = TRUE, size = 1, alpha = 0.3
  ) +
  scale_color_manual(
    values = c(
      "ABCO" = "mediumturquoise",
      "PIPO" = "lawngreen",
      "ABMA" = "maroon3",
      "PILA" = "lightslateblue"
    )
  ) +
  scale_linetype_manual(
    name = "P50 Type",
    values = c("Measured" = "solid", "Predicted" = "dashed")
  ) +
  labs(
    title = "Measured and Predicted P50 Trendlines by Species",
    x = "Diameter at Root Collar (cm)",
    y = "P50 (-MPa)",
    color = "Species"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  )

# with the exception of ABMA it looks like predicted and observed P50 values are similar enough

# calculate HSM using both predawn and midday with year specific WP and P50 data
trait_data1$HSM_predawn_2024 <- trait_data1$P50_MPa_2024 - trait_data1$predawn_MPa_2024
trait_data1$HSM_midday_2024  <- trait_data1$P50_MPa_2024 - trait_data1$midday_MPa_2024

trait_data1$HSM_predawn_2025 <- trait_data1$P50_MPa_2025 - trait_data1$predawn_MPa_2025
trait_data1$HSM_midday_2025 <- trait_data1$P50_MPa_2025 - trait_data1$midday_MPa_2025

# calculate HSM using P50_complete (measured and predicted P50 infilled for missing values)
trait_data2$HSM_predawn_P50_mean_2024 <- trait_data2$P50_complete - trait_data2$predawn_MPa_2024
trait_data2$HSM_midday_P50_mean_2024  <- trait_data2$P50_complete - trait_data2$midday_MPa_2024

trait_data2$HSM_predawn_P50_mean_2025 <- trait_data2$P50_complete - trait_data2$predawn_MPa_2025
trait_data2$HSM_midday_P50_mean_2025  <- trait_data2$P50_complete - trait_data2$midday_MPa_2025

# subset dataframe by species 
ABCO <- trait_data2[trait_data2$species == 'ABCO',]
PILA <- trait_data2[trait_data2$species == 'PILA',]
PIPO <- trait_data2[trait_data2$species == 'PIPO',]
ABMA <- trait_data2[trait_data2$species == 'ABMA',]

# remove outlier tree_ID == 94
trait_data1 <- trait_data1[trait_data1$tree_ID != 94, ]
trait_data2 <- trait_data2[trait_data2$tree_ID != 94, ]

mod3 <- lmer(predawn_MPa ~ diameter + percent_cover + year + elev_indiv + species 
             + (1|plot), data = trait_data2)
summary(mod3)

# determine whether WP and P50 values are different between years to determine how data is pooled
t.test(predawn_MPa ~ year, data = trait_data2, paired = FALSE)
# There is a statistically significant difference between 2024 and 2025 
# predawn and midday water potential means. 

# create a new variable that is predawn and midday for 2025 unless NA, if NA, pull 2024 
trait_data1 <- trait_data1 %>%
  mutate(predawn_MPa_combined = coalesce(predawn_MPa_2025, predawn_MPa_2024))

trait_data1 <- trait_data1 %>%
  mutate(midday_MPa_combined = coalesce(midday_MPa_2025, midday_MPa_2024))

# add P50_complete to trait_data1, its derived from both P50_MPa_2024 and P50_MPa_2025 
# in trait_data2
# and now we can recalculate HSM using P50 complete (observed and predicted values) and 2024-2025 
# combined WP data

# average P50 across years, create a vector of one P50 value per tree_ID
# P50 mean is the average of both years for measured and predicted observations
P50_mean <- trait_data2 %>%
  group_by(tree_ID) %>%
  summarise(P50_mean = mean(P50_complete, na.rm = TRUE), .groups = "drop")

# merge P50_mean with trait_data1
trait_data1 <- trait_data1 %>%
  left_join(P50_mean, by = "tree_ID")

# calculate HSM using the combined predawn and midday potentials
trait_data1 <- trait_data1 %>%
  mutate(
    HSM_predawn_P50_mean = P50_mean - predawn_MPa_combined,
    HSM_midday_P50_mean  = P50_mean - midday_MPa_combined
  )

# P50_MPa in trait_data2 is the pooled raw P50 data. lets aggregate that by averaging across years, and join
# that to trait_data1

# aggregate P50_MPa across years by tree_ID (mean per tree)
P50_combined <- trait_data2 %>%
  group_by(tree_ID) %>%
  summarise(P50_combined = mean(P50_MPa, na.rm = TRUE), .groups = "drop")

# Step 2: Join the average P50 back to trait_data1
trait_data1 <- trait_data1 %>%
  left_join(P50_combined, by = "tree_ID")

# P50_complete = 
# P50_combined = 
# P50_mean = 


