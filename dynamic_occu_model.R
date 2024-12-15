# Final project - RNG557
# Author: Francisco Gonzalez
# Fall 2024 OSU

#### IMPORT LIBRARIES ####
library(dplyr)
library(lubridate)
library(tidyr)
library(unmarked)
library(ggplot2)
library(terra)
library(readxl)
library(stringr)
library(sf)
library(osmdata)
library(ggspatial)
library(viridis)

#### FUNCTIONS ####
get_env_data <- function(filename, sheet, min_date, max_date) {
  df <- read_excel(filename, sheet=sheet)
  df <- df %>% subset(., select = -c(1,2)) %>% t() %>% as.data.frame() %>% 
    mutate(date = as.Date(paste0(rownames(.), "-01"))) %>% 
    mutate(date = ceiling_date(date, "month") - days(1)) %>% 
    filter(date >= min_date & date - 30 <= max_date) %>% 
    separate(date, into = c("year","month", "day"), sep="-") %>% 
    rename(value = V1) %>% 
    select(-day)
  return(df)
}

#### DATA LOAD ####

# Occurrence data 
occu_data <- read.csv("data/occurrence_data.csv")
occu_data <- occu_data[, c("gbifID", "individualCount", "eventDate", "decimalLatitude", "decimalLongitude")]

str(occu_data)
summary(occu_data)

occu_data <- occu_data %>%
  # Assuming at least 1 detected if row is present but value is NA
  mutate(individualCount = ifelse(is.na(individualCount), 1, individualCount)) %>% 
  # Removing rows with empty date-time stamps
  filter(eventDate != "") %>% 
  # Adding new fields
  mutate(site_id = paste(round(decimalLatitude, 2), round(decimalLongitude, 2), sep = "_"),
         year= sapply(str_split(eventDate, "-"), `[`,1),
         month= sapply(str_split(eventDate, "-"), `[`,2),
         eventDate = as.Date(substr(eventDate, 1, 10))
         #pr = NA, tas = NA, cdd = NA
  )


# Only keeping data from 2016-2023
occu_data <- occu_data[occu_data$year >= 2016 & occu_data$year <= 2023, ]

# Getting time range of sightings
max_date <- max(occu_data[, "eventDate"])
min_date <- min(occu_data[, "eventDate"])
date_range <- str_sub(c(min_date, max_date), 1, 7)

# Load environmental data
cdd_monthly <- get_env_data("data/era5_timeseries.xlsx", "cdd_monthly", min_date, max_date)
pr_monthly <- get_env_data("data/era5_timeseries.xlsx", "pr_monthly", min_date, max_date)
tas_monthly <- get_env_data("data/era5_timeseries.xlsx", "tas_monthly", min_date, max_date)

# Load geospatial data
bbox <- c(min_lon = -83.9, min_lat = 8.3, max_lon = -83.1, max_lat = 8.9)

road_data <- opq(bbox=bbox) %>% 
  add_osm_feature(key="highway") %>% 
  osmdata_sf()

farm_data <- opq(bbox=bbox) %>% 
  add_osm_feature(key="landuse", value="farmland") %>% 
  osmdata_sf()

town_data <- opq(bbox=bbox) %>% 
  add_osm_features( features = list(
    "place" = "city",
    "place" = "town",
    "place" = "village"
  )) %>% 
  osmdata_sf()

water <- opq(bbox = bbox) %>%
  add_osm_features( features = list(
    "natural" = "wetland",
    "natural" = "water"
  )) %>%
  osmdata_sf()

forest <- opq(bbox = bbox) %>%
  add_osm_feature(key = "natural", value = "wood") %>%
  osmdata_sf()

grass <- opq(bbox = bbox) %>%
  add_osm_features( features = list(
    "natural" = "grassland",
    "natural" = "heath"
  )) %>% 
  osmdata_sf()

coastline <- opq(bbox=bbox) %>% 
  add_osm_feature(key="natural", value = "coastline") %>% osmdata_sf()

# Extract necessary layers
water_polygons <- water$osm_polygons
forest_polygons <- forest$osm_polygons
grass_polygons <- grass$osm_polygons
coast_lines <- coastline$osm_lines
road_lines <- road_data$osm_lines
town_points <- town_data$osm_points
farm_polygons <- farm_data$osm_polygons

#### DATA PREPARATION ####

# Combine environmental data into a single dataframe
env_data <- bind_rows(
  cdd_monthly %>% mutate(variable = "cdd"),
  pr_monthly %>% mutate(variable = "pr"),
  tas_monthly %>% mutate(variable = "tas")
) %>%
  pivot_wider(names_from = variable, values_from = value)

# Merging environmental data into occu_data by year and month
occu_data <- occu_data %>%
  left_join(env_data, by = c("year", "month"))

# Converting occu_data into geospatial dataframe
site_points <- occu_data %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# Ensuring these match CRS of site_points
road_lines <- st_transform(road_lines, crs = st_crs(site_points))
town_points <- st_transform(town_points, crs = st_crs(site_points))
farm_polygons <- st_transform(farm_polygons, crs = st_crs(site_points))
water_polygons <- st_transform(water_polygons, crs = st_crs(site_points))
forest_polygons <- st_transform(forest_polygons, crs = st_crs(site_points))
grass_polygons <- st_transform(grass_polygons, crs = st_crs(site_points))
coast_lines <- st_transform(coast_lines, crs = st_crs(site_points))

# Calculating distances from sightings to geospatial features
occu_data <- occu_data %>%
  mutate(
    road_dist = apply(st_distance(site_points, road_lines), 1, min),
    town_dist = apply(st_distance(site_points, town_points), 1, min),
    farm_dist = apply(st_distance(site_points, farm_polygons), 1, min),
    water_dist = apply(st_distance(site_points, water_polygons), 1, min),
    forest_dist = apply(st_distance(site_points, forest_polygons), 1, min),
    grass_dist = apply(st_distance(site_points, grass_polygons), 1, min),
    coast_dist = apply(st_distance(site_points, coast_lines), 1, min)
  )

# Determining if occurrences fall within specific land-use types
occu_data <- occu_data %>%
  mutate(
    inside_farmland = st_intersects(site_points, farm_polygons, sparse = FALSE) %>% apply(1, any) %>% ifelse("y", "n"),
    inside_grassland = st_intersects(site_points, grass_polygons, sparse = FALSE) %>% apply(1, any) %>% ifelse("y", "n"),
    inside_forest = st_intersects(site_points, forest_polygons, sparse = FALSE) %>% apply(1, any) %>% ifelse("y", "n")
  )

#### EXPLORATORY DATA ANALYSIS ####

# Histogram of group size
ggplot(occu_data, aes(x=individualCount)) +
  geom_histogram(binwidth = 1, fill="skyblue", color="black",alpha=0.7) +
  labs(title="Distribution of Sightings by Group Size",
       x = "Group Size (number of individuals)") + 
  theme_minimal()

# Histogram of yearly sightings
ggplot(occu_data, aes(x=year)) +
  geom_histogram(stat="count", fill="skyblue", color="black",alpha=0.7) +
  labs(title="Distribution of Sightings by Year",
       x = "Year") + 
  theme_minimal()

# Monthly sightings
ggplot(occu_data, aes(x=month)) +
  geom_histogram(stat="count", fill="skyblue", color="black",alpha=0.7) +
  labs(title="Distribution of Sightings by Month",
       x = "Month") + 
  theme_minimal()

# Get total counts by year
total_counts_per_yr <- occu_data %>% group_by(year) %>%
  summarize(total_count = sum(individualCount))

# Total individuals observed each year
ggplot(total_counts_per_yr, aes(x = as.numeric(year), y = total_count)) +
  geom_line(color = "black", size = 1) +
  geom_point(color = "orange", size = 2) +
  scale_x_continuous(breaks = seq(min(total_counts_per_yr$year), max(total_counts_per_yr$year), by = 1)) +
  labs(
    title = "Total Individual Count Over Time",
    x = "Year",
    y = "Total Individual Count"
  ) +
  theme_minimal()

# Mapping sightings with geospatial features
ggplot() +
  geom_sf(data = forest_polygons, fill = "darkgreen", alpha = 0.3, color = NA) +
  geom_sf(data = farm_polygons, fill = "yellow2", alpha = 0.5, color = NA) +
  geom_sf(data = coast_lines, color = "black", size = 0.3, alpha = 0.7) +
  geom_sf(data = town_points, aes(color = "Towns"), size = 2, shape = 21, fill = "black") +
  geom_sf(data = grass_polygons, fill = "lightgreen", alpha = 0.5, color = NA) +
  geom_sf(data = road_lines, color = "grey40", size = 0.3) +
  geom_sf(data = water_polygons, fill = "lightblue", alpha = 0.4, color = NA) +
  geom_sf(data = site_points, aes(color = "Sightings", size=individualCount)) +  # Sightings
  scale_color_manual(values = c("Sightings" = "orange", "Towns" = "black"), name = "Legend") +
  labs(
    title = "Collared Peccary Sightings with Geospatial Context",
    subtitle = "Osa Peninsula, Costa Rica",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

# Examine relationships across data
ggplot(occu_data, aes(x = inside_forest, y = individualCount)) +
  geom_violin() +
  labs(title = "Group Size vs Forest Occupancy",
       x = "Inside Forest (y/n)",
       y = "Group Size") +
  theme_minimal()

env_data_long <- env_data %>%
  pivot_longer(cols = c("cdd", "tas", "pr"), names_to = "variable", values_to = "value")

#Plot seasonal variation
ggplot(env_data_long, aes(x = month, y = value, group = year, color = factor(year))) +
  geom_line(alpha = 0.7) +
  facet_wrap(~ variable, scales = "free_y", labeller = labeller(variable = c(
    cdd = "Consecutive Dry Days (CDD)",
    tas = "Temperature (°C)",
    pr = "Precipitation (mm)"
  ))) +
  scale_color_viridis_d(name = "Year") +
  labs(
    title = "Seasonal Variation in Environmental Data",
    x = "Month",
    y = "Value"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot for seasonal variation
ggplot(env_data_long, aes(x = month, y = value, fill = variable)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  facet_wrap(~ variable, scales = "free_y", labeller = labeller(variable = c(
    cdd = "Consecutive Dry Days (CDD)",
    tas = "Temperature (°C)",
    pr = "Precipitation (mm)"
  ))) +
  scale_fill_viridis_d(name = "Variable", option = "C") +
  labs(
    title = "Monthly Environmental Patterns Across All Years",
    x = "Month",
    y = "Value"
  ) +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

# Aggregate occurrence data by month and year
monthly_sightings <- occu_data %>%
  group_by(year, month) %>%
  summarize(
    sightings = n(),  # Total sightings (number of occurrences)
    total_count = sum(individualCount, na.rm = TRUE),  # Total individual count
    .groups = "drop"
  ) %>%
  mutate(month = factor(month, levels = sprintf("%02d", 1:12), labels = month.name))

# Reshape data to long format for plotting
monthly_sightings_long <- monthly_sightings %>%
  pivot_longer(cols = c("sightings", "total_count"), names_to = "metric", values_to = "value")

# Plot sightings and total counts by month for each year
ggplot(monthly_sightings_long, aes(x = month, y = value, group = year, color = factor(year))) +
  geom_line(alpha = 0.7, size = 1) +
  facet_wrap(~ metric, scales = "free_y", labeller = labeller(metric = c(
    sightings = "Monthly Sightings",
    total_count = "Total Individual Count"
  ))) +
  scale_color_viridis_d(name = "Year") +
  labs(
    title = "Monthly Variation in Sightings and Individual Count",
    x = "Month",
    y = "Value"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

#### DETECTION HISTORY AND COVARIATES ####

library(tibble)

# Create detection matrix
detection_matrix <- occu_data %>%
  group_by(site_id, year, month) %>%
  summarize(detected = ifelse(sum(individualCount) > 0, 1, 0), .groups = "drop") %>%
  complete(site_id, year, month = sprintf("%02d", 1:12), fill = list(detected = 0)) %>%
  arrange(site_id, year, month) %>% 
  pivot_wider(names_from = c(year, month), values_from = detected, values_fill = 0) %>%
  column_to_rownames("site_id") %>%
  as.matrix()

dim(detection_matrix)

# Static covariates -- the geospatial features basically
site_covs <- occu_data %>% 
  group_by(site_id) %>% 
  summarize(
    mean_coast_dist = mean(coast_dist, na.rm = TRUE),
    mean_road_dist = mean(road_dist, na.rm = TRUE),
    mean_town_dist = mean(town_dist, na.rm = TRUE),
    mean_farm_dist = mean(farm_dist, na.rm = TRUE),
    mean_water_dist = mean(water_dist, na.rm = TRUE),
    mean_grass_dist = mean(grass_dist, na.rm = TRUE),
    mean_forest_dist = mean(forest_dist, na.rm = TRUE),
    inside_forest = as.factor(max(as.integer(inside_forest == "y"))),  # Convert to binary
    .groups = "drop"
  )

n_sites <- nrow(detection_matrix)       # sites
n_primary <- 8                          # years
n_secondary <- 12                       # months

# Define row and column names
row_names <- unique(occu_data$site_id)
col_names <- paste(rep(2016:2023, each = n_secondary), sprintf("%02d", 1:12), sep = "_")

env_data$year <- as.numeric(env_data$year)
env_data$month <- as.numeric(env_data$month)

obs_covs_list <- list(
  tas = matrix(t(tas_monthly$value), nrow(detection_matrix), 96, byrow=TRUE),
  cdd = matrix(t(cdd_monthly$value), nrow(detection_matrix), 96, byrow=TRUE),
  pr = matrix(t(pr_monthly$value), nrow(detection_matrix), 96, byrow=TRUE) 
)

# Adding yearly env data
yearly_env_data <- env_data %>%
  group_by(year) %>%
  summarize(
    yearly_tas = mean(tas, na.rm = TRUE),
    yearly_pr = mean(pr, na.rm = TRUE),
    yearly_cdd = mean(cdd, na.rm = TRUE),
    .groups = "drop"
  )

yearly_site_covs <- list(
  year_tas = matrix(t(yearly_env_data$yearly_tas), nrow(detection_matrix), 8, byrow=TRUE),
  year_cdd = matrix(t(yearly_env_data$yearly_cdd), nrow(detection_matrix), 8, byrow=TRUE),
  year_pr = matrix(t(yearly_env_data$yearly_pr), nrow(detection_matrix), 8, byrow=TRUE) 
)
  
#### MODEL BUILDING ####

# Prepare unmarked frame object
umf <- unmarkedMultFrame(
  y = detection_matrix,    # Detection matrix
  siteCovs = site_covs,    # Static site-level covariates
  yearlySiteCovs = yearly_site_covs,
  obsCovs = obs_covs_list, # Time-varying covariates (month)
  numPrimary = 8          # Number of primary periods (years)
)

summary(umf)

# Geospatial Features - Hypothesis 1
mod1 <- colext(
  psiformula = ~ mean_farm_dist + mean_town_dist + mean_road_dist,  # Initial occupancy
  gammaformula = ~ mean_farm_dist+ mean_town_dist,                 # Colonization
  epsilonformula = ~ mean_town_dist + mean_road_dist,               # Extinction
  pformula = ~ 1,                                                   # Detection probability
  data = umf
)

summary(mod1)

# Geospatial and Env combined - Hypothesis 1
mod2 <- colext(
  psiformula = ~ mean_forest_dist + mean_water_dist,    # Initial occupancy
  gammaformula = ~ year_tas + year_pr,                  # Colonization
  epsilonformula = ~ year_cdd + mean_forest_dist,       # Extinction
  pformula = ~ tas*cdd + pr,                            # Detection probability
  data = umf
)

summary(mod2)

mod3 <-  colext(
  psiformula = ~ mean_farm_dist + mean_forest_dist + mean_coast_dist, # Initial occupancy
  gammaformula = ~ year_tas + year_pr + mean_farm_dist,              # Colonization
  epsilonformula = ~ year_cdd + mean_town_dist + mean_road_dist,     # Extinction
  pformula = ~ tas*cdd + pr,                                         # Detection probability
  data = umf
)

summary(mod3)

#### INTERPRET RESULTS ####

# Predict occupancy probabilities
predictions <- predict(
  mod2,                       # Replace with your fitted model
  type = "psi",             # State corresponds to occupancy probabilities
  newdata = site_covs         # Use the site-level covariates for prediction
)

# Prepare spatial data
predicted_sites <- site_covs %>%
  mutate(occupancy_prob = predictions$Predicted) %>%  # Add occupancy probabilities
  left_join(occu_data %>% select(site_id, decimalLatitude, decimalLongitude) %>% distinct(), by = "site_id") %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)  # Convert to spatial data

# Ensure all layers are in the same CRS
contextual_layers <- list(
  roads = st_transform(road_lines, st_crs(predicted_sites)),
  towns = st_transform(town_points, st_crs(predicted_sites)),
  forests = st_transform(forest_polygons, st_crs(predicted_sites)),
  water = st_transform(water_polygons, st_crs(predicted_sites)),
  coastline = st_transform(coast_lines, st_crs(predicted_sites))
)

ggplot() +
  geom_sf(data = contextual_layers$forests, fill = "darkgreen", alpha = 0.3, color = NA) +
  geom_sf(data = contextual_layers$water, fill = "blue", alpha = 0.3, color = NA) +
  geom_sf(data = contextual_layers$coastline, fill = "black", alpha=0.3) +
  geom_sf(data = contextual_layers$roads, color = "gray", size = 0.5, linetype = "dashed") +
  geom_sf(data = predicted_sites, aes(color = occupancy_prob), size = 2) +
  scale_color_viridis_c(name = "Occupancy Probability", option = "plasma") +
  labs(
    title = "Predicted Occupancy Probabilities for Collared Peccaries",
    subtitle = "Osa Peninsula, Costa Rica",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14)
  )

# Create a new dataset to represent seasonal covariates
seasonal_covs <- expand.grid(
  tas = seq(min(env_data$tas), max(env_data$tas), length.out = 12),  # Monthly temperature
  cdd = mean(env_data$cdd, na.rm = TRUE),  # Keep other covariates constant
  pr = mean(env_data$pr, na.rm = TRUE)     # Keep other covariates constant
)

# Predict detection probabilities for each month
seasonal_predictions <- predict(
  mod2,
  type = "det",
  newdata = seasonal_covs
)

# Add month labels
seasonal_predictions$month <- factor(1:12, labels = month.name)

# Plot detection probability across months
ggplot(seasonal_predictions, aes(x = month, y = Predicted, group = 1)) +
  geom_line(color = "#0073C2FF", size = 1.5) +  # Use consistent and aesthetic color
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "#56B4E9") +  # Add a subtle ribbon
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # Convert y-axis to percentage
  labs(
    title = "Seasonal Variation in Detection Probability",
    subtitle = "Detection probability by month with 95% confidence intervals",
    x = "Month",
    y = "Detection Probability",
    caption = "Error bars show 95% confidence intervals."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Arial"),  # Use a clean, professional font
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14),
    legend.position = "none",  # Remove legend if not needed
    plot.caption = element_text(size = 10, hjust = 0)
  )
