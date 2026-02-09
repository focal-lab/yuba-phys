DATADIR = "/Users/anjumgujral/Box Sync/yuba-phys_data/traits"
trait_data <- read.csv("/Users/anjumgujral/Box Sync/yuba-phys_data/traits/yuba-phys-raw-field-data-2024_2025.csv")

library('dplyr')
library('tidyverse')
library('ggplot2')
library('lme4')

# subset dataframe to include only seedlings and saplings for ABCO, PIPO, PILA, and ABMA
trait_data1 <- trait_data[trait_data$size_class %in% c('seedling', 'sapling'), ]
trait_data1 <- trait_data1[trait_data1$species %in% c('ABCO', 'ABMA','PIPO', 'PILA'), ]

# delete columns we don't need (size_class, height_m, PARmicromol, 
# branch_processed_2024, branch_processed_2025)

trait_data1 <- trait_data1 %>% select(-height_m, -PAR_μmol, -branch_processed_2024, -branch_processed_2025)


# make a new column turning canopy closure into a categorical variable
trait_data1$microsite <- cut(
  trait_data1$canopy_closure,
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
trait_data1$diameter_cm <- as.numeric(as.character(trait_data1$diameter_cm))
plot(trait_data1$diameter_cm~trait_data1$seedling_height_cm)
mod1 <- lm(trait_data1$diameter_cm~trait_data1$seedling_height_cm)
summary(mod1)

# create a training model using seedlings with height <= 100cm 
complete_cases <- trait_data1 %>%
  filter(!is.na(diameter_cm) & seedling_height_cm <= 100)

missing_cases <- trait_data1 %>% filter(is.na(diameter_cm))
mod2 <- lm(diameter_cm ~ seedling_height_cm, data = complete_cases)
summary(mod2) 

trait_data1$predicted_diameter <- NA

if (nrow(missing_cases) > 0) {
  predicted_values <- predict(mod2, newdata = missing_cases)
  trait_data1$predicted_diameter[is.na(trait_data1$diameter_cm)] <- predicted_values
}

# create a new column that combines observed and predicted diameter values
trait_data1$diameter_complete <- ifelse(is.na(trait_data1$diameter_cm), trait_data1$predicted_diameter, 
                                        trait_data1$diameter_cm)

# make size categorical
trait_data1 <- trait_data1 %>%
  mutate(size = ifelse(diameter_complete < 2.5, "seedling", "sapling"))

#compare trends between diameter and seedling height for actual and predicted data
ggplot(trait_data1, aes(x = seedling_height_cm, y = diameter_cm)) +
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

# combine P50 data from 2024-2025 and take the average for trees measured in both years
trait_data1 <- trait_data1 %>%
  mutate(P50_combined = rowMeans(cbind(P50_MPa_2024, P50_MPa_2025), na.rm = TRUE))

trait_data1$P50_combined <- as.numeric(as.character(trait_data1$P50_combined))


# interpolate P50 values for individuals that P50 wasn't measured
# create a training dataset
P50_measured <- trait_data1 %>%
  filter(!is.na(P50_combined))

P50_missing <- trait_data1 %>% 
  filter(is.na(P50_combined))

# we cant predict P50 for trees that have missing predictors
P50_missing <- P50_missing %>%
  filter(
    !is.na(species),
    !is.na(size),
    !is.na(canopy_closure),
    !is.na(elevation_ft),
    !is.na(plot)
  )

mod3 <- lmer(P50_combined ~ species + size + canopy_closure + elevation_ft + (1|plot), data = P50_measured)
summary(mod3)

trait_data1$predicted_P50 <- NA



# predict missing P50 data from mod3 and include random effects from plot when predicting
#if (nrow(P50_missing) > 0) {
 # P50_predicted_values <- predict(mod3, newdata = P50_missing, re.form = ~(1 | plot))
#  trait_data1$predicted_P50[is.na(trait_data1$P50_combined)] <- P50_predicted_values
#  trait_data1$predicted_P50[(trait_data1$tree_ID) %in% (P50_missing$tree_ID)] <- P50_predicted_values
#}

if (nrow(P50_missing) > 0) {
  P50_predicted_values <- predict(mod3, newdata = P50_missing, re.form = ~(1 | plot))
   trait_data1$predicted_P50[match(P50_missing$tree_ID, trait_data1$tree_ID)] <- P50_predicted_values
}

all.equal(trait_data1$tree_ID[which(trait_data1$tree_ID %in% P50_missing$tree_ID)], P50_missing$tree_ID)
# create a new column that combines observed and predicted P50 values (from both years)
trait_data1$P50_complete <- ifelse(is.na(trait_data1$P50_combined), trait_data1$predicted_P50, 
                                        trait_data1$P50_combined)

trait_data1 <- trait_data1 %>%
  mutate(P50_source = ifelse(is.na(P50_combined), "predicted", "measured"))

#compare trends between P50 measured and P50 predicted
ggplot() +
  geom_point(
    data = trait_data1 %>% filter(!is.na(P50_combined)),
    aes(x = diameter_complete, y = P50_combined, color = species),
    shape = 16, size = 3, alpha = 0.8
  ) +
  geom_point(
    data = trait_data1 %>% filter(is.na(P50_combined)),
    aes(x = diameter_complete, y = predicted_P50, color = species),
    shape = 17, size = 3, alpha = 0.8
  ) +
  geom_smooth(
    data = trait_data1 %>% filter(!is.na(P50_combined)),
    aes(x = diameter_complete, y = P50_combined, color = species, linetype = "Measured"),
    method = "lm", se = TRUE, size = 1, alpha = 0.3
  ) +
  geom_smooth(
    data = trait_data1 %>% filter(is.na(P50_combined)),
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
    x = "Basal diameter (cm)",
    y = "P50 (-MPa)",
    color = "Species"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  )


# average water potentials across years
trait_data1 <- trait_data1 %>%
  mutate(predawn_MPa_combined = rowMeans(cbind(predawn_MPa_2025, predawn_MPa_2024), na.rm = TRUE))

trait_data1 <- trait_data1 %>%
  mutate(midday_MPa_combined = rowMeans(cbind(midday_MPa_2025, predawn_MPa_2024), na.rm = TRUE))


# P50_combined == measured P50 from both years
# P50_complete == measured (and averaged from both years) and interpolated P50


# calculate HSM using both predawn and midday with year specific WP and P50 data
trait_data1$HSM_predawn_2024 <- trait_data1$P50_MPa_2024 - trait_data1$predawn_MPa_2024
trait_data1$HSM_midday_2024  <- trait_data1$P50_MPa_2024 - trait_data1$midday_MPa_2024

trait_data1$HSM_predawn_2025 <- trait_data1$P50_MPa_2025 - trait_data1$predawn_MPa_2025
trait_data1$HSM_midday_2025 <- trait_data1$P50_MPa_2025 - trait_data1$midday_MPa_2025

trait_data1$HSM_predawn_combined <- trait_data1$P50_combined - trait_data1$predawn_MPa_combined
trait_data1$HSM_midday_combined <- trait_data1$P50_combined - trait_data1$midday_MPa_combined

# and now calculate HSM using P50 complete (observed and predicted values) and 2024-2025 
# combined WP data
# calculate HSM using P50_complete (measured and predicted P50 infilled for missing values)
trait_data1$HSM_predawn_P50_mean <- trait_data1$P50_complete - trait_data1$predawn_MPa_combined
trait_data1$HSM_midday_P50_mean <- trait_data1$P50_complete - trait_data1$midday_MPa_combined

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


# lets convert to long format again but by WP type instead of year this time 
trait_data3 <- trait_data1 %>%
  pivot_longer(
    cols = c(
      predawn_MPa_combined,
      midday_MPa_combined,
      P50_combined,
      P50_complete
    ),
    names_to = "type",
    values_to = "water_potential"
  )

# remove outlier tree_ID == 94
trait_data1 <- trait_data1[trait_data1$tree_ID != 94, ]
trait_data2 <- trait_data2[trait_data2$tree_ID != 94, ]
trait_data3 <- trait_data3[trait_data3$tree_ID != 94, ]

# scale predictor variables
trait_data1$diameter_complete_scaled <- as.numeric(scale(trait_data1$diameter_complete))
trait_data1$canopy_closure_scaled <- as.numeric(scale(trait_data1$canopy_closure))
trait_data1$elevation_ft_scaled <- as.numeric(scale(trait_data1$elevation_ft))

# scale response variables 
trait_data1$predawn_scaled <- as.numeric(scale(trait_data1$predawn_MPa_combined))
trait_data1$P50_scaled <- as.numeric(scale(trait_data1$P50_combined))
trait_data1$P50_mean_scaled <- as.numeric(scale(trait_data1$P50_complete))
trait_data1$HSM_midday_P50_mean_scaled <- as.numeric(scale(trait_data1$HSM_midday_P50_mean))



# we want to include a random/fixed effect for year, so lets make a column for that, including 'both'
trait_data1 <- trait_data1 %>%
  mutate(year = case_when(
    !is.na(predawn_MPa_2024) & is.na(predawn_MPa_2025) ~ "2024",
    is.na(predawn_MPa_2024) & !is.na(predawn_MPa_2025) ~ "2025",
    !is.na(predawn_MPa_2024) & !is.na(predawn_MPa_2025) ~ "both",
    TRUE ~ NA_character_
  ))

# subset dataframe by species 
ABCO <- trait_data1[trait_data1$species == 'ABCO',]
PILA <- trait_data1[trait_data1$species == 'PILA',]
PIPO <- trait_data1[trait_data1$species == 'PIPO',]
ABMA <- trait_data1[trait_data1$species == 'ABMA',]

# are water potentials and P50s different between years
# compare trends between years for measured P50 values
ggplot() +
  geom_point(data = trait_data1, 
             aes(x = diameter_complete, y = P50_MPa_2024, color = species, shape = "2024"), 
             size = 3, alpha = 1) +
  geom_smooth(data = trait_data1, 
              aes(x = diameter_complete, y = P50_MPa_2024, color = species, linetype = "2024"), 
              method = "lm", se = TRUE, alpha = 0.3) +
  geom_point(data = trait_data1, 
             aes(x = diameter_complete, y = P50_MPa_2025, color = species, shape = "2025"), 
             size = 3, alpha = 1) +
  geom_smooth(data = trait_data1, 
              aes(x = diameter_complete, y = P50_MPa_2025, color = species, linetype = "2025"), 
              method = "lm", se = TRUE, alpha = 0.3) +
  scale_color_manual(values = c(
    "ABCO" = "mediumturquoise",
    "PIPO" = "lawngreen",
    "ABMA" = "maroon3",
    "PILA" = "lightslateblue"
  )) +
  scale_shape_manual(name = "Year", values = c("2024" = 16, "2025" = 17)) +
  scale_linetype_manual(name = "Year", values = c("2024" = "solid", "2025" = "dashed")) +
  labs(
    x = "Basal diameter (cm)",
    y = "P50 (-MPa)",
    color = "Species"
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "top"
  )

mod4 <- lmer(P50_MPa ~ diameter_complete + canopy_closure + year + elevation_ft 
             + (1|plot), data = )
summary(mod4)

# determine whether WP and P50 values are different between years to determine how data is pooled
t.test(P50_MPa ~ year, data = PILA_long, paired = FALSE)
# There is a statistically significant difference between 2024 and 2025 
# predawn and midday water potential means. 


P50_summary_table <- trait_data1 %>%
  filter(!is.na(P50_MPa_2024) | !is.na(P50_MPa_2025)) %>% 
  group_by(size, species, elevation, microsite) %>%
  summarise(n_individuals = n(), .groups = "drop")

WP_summary_table <- trait_data1 %>%
  filter(!is.na(predawn_MPa_2024) | !is.na(predawn_MPa_2025)) %>% 
  group_by(size, species, plot) %>%
  summarise(n_individuals = n(), .groups = "drop")

write.csv(trait_data1, file = "yuba-phys-prepped-field-data-2024_2025.csv", row.names = FALSE)
