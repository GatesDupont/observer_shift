# Gates Dupont
# gatesdupont@princeton.edu

library(tidyverse)
library(vroom)
library(RSQLite)
library(dbscan)
library(leaflet)
library(maps)
library(sf)

source("scripts/00_Functions.R")


# ---- Load the SQL database to df ---

db_to_load <- "Data/PFW/Downloaded PFW data/2023-08-21_03-17-30-PM.sqlite"
conn <- dbConnect(SQLite(), db_to_load)

pfw_all <- dbGetQuery(conn, 
                      "SELECT DISTINCT 
  LOC_ID, PROJ_PERIOD_ID, LATITUDE, LONGITUDE, SUBNATIONAL1_CODE 
  FROM combined_data")

dbDisconnect(conn)


# ---- Select first year data ----

# New object
bird_data_sf_aea <- pfw_all %>%
  mutate(winter = substr(PROJ_PERIOD_ID, 5, 8)) %>%
  select(-PROJ_PERIOD_ID, -SUBNATIONAL1_CODE) %>%
  na.omit() %>%
  filter(winter == 1989) %>%
  st_as_sf(., coords = c("LONGITUDE","LATITUDE"), crs = 4326) %>%
  st_transform(2163)


# ---- ID centroids for study areas ----

# Fixed-radius neighborhood search (FRNS)
# Calculate the number of points within a 15-mile radius for each point
bird_data_sf_aea$neighbors_count <- lengths(
  st_is_within_distance(bird_data_sf_aea, bird_data_sf_aea, dist = 24140))

# Extract PFW locs with at least n neighbors
potential_centers <- bird_data_sf_aea %>% 
  filter(neighbors_count >= 30)

# Run DBSCAN; eps is set to a value reflecting the max distance for points 
# to be considered part of the same local cluster
dbscan_clusters <- dbscan(st_coordinates(potential_centers), 
                          eps = 24140, minPts = 1)

# Add cluster IDs to potential_centers
potential_centers$local_cluster <- dbscan_clusters$cluster

# Select 1 point with most neighbors per cluster
representative_points <- potential_centers %>%
  group_by(local_cluster) %>%  # Group by the local cluster ID
  arrange(desc(neighbors_count)) %>%
  slice(1) %>%
  ungroup()


# ---- Associate city names to study area centroids ----

# Load city data and filter to major cities
major_cities_sf <- maps::world.cities %>%
  filter(country.etc %in% c("USA", "Canada")) %>%
  filter(name %in% c(
    "Boston","Montreal","Toronto","New York","Philadelphia",
    "Washington","Pittsburgh","Cleveland","Chicago","Atlanta",
    "Minneapolis","Seattle","San Francisco")) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(2163)

# Find which city is closest to each centroid
closest_cities <- st_nearest_feature(representative_points, major_cities_sf)

# Assign city names to centroids
representative_points$city_name <- major_cities_sf$name[closest_cities]


# ---- Select points in study area of 15mi radius ----

# All PFW locs, transform to 2163
all_pfw_locs <- pfw_all %>%
  mutate(winter = substr(PROJ_PERIOD_ID, 5, 8)) %>%
  select(-PROJ_PERIOD_ID, -SUBNATIONAL1_CODE) %>%
  na.omit() %>%
  st_as_sf(., coords = c("LONGITUDE","LATITUDE"), crs = 4326) %>%
  st_transform(2163)

# Create a buffer of 15 miles (in meters) around each representative point
city_buffers <- st_buffer(representative_points, dist = 15 * 1609.34)  # Convert miles to meters

# Select sites in buffers
locs_in_cities <- st_join(all_pfw_locs, city_buffers, join = st_within) %>%
  filter(!is.na(city_name)) %>%
  select(LOC_ID = LOC_ID.x, winter = winter.x, city = city_name, geometry)

# Final result
result <- locs_in_cities %>% 
  as_tibble() %>%
  select(LOC_ID, winter, city) %>%
  distinct() %>%
  mutate(loc_winter = paste0(LOC_ID, "_", winter))


# ---- Plot sites in cities ----

# Plot in leaflet
locs_in_cities %>%
  select(-winter) %>%
  distinct() %>%
  st_transform(4326) %>%
  st_as_sf() %>%
  leaflet(data = .) %>%
  addTiles() %>%  # This will add the default OpenStreetMap tiles
  addCircleMarkers(
    # ~lon, ~lat, 
    popup = ~city,  # This will show the 'name' when you click on a marker
    radius = 5,
    color = "red",
    fill = TRUE,
    fillOpacity = 0.6)


# ---- OUTPUT ----

db_to_load <- "Data/PFW/Downloaded PFW data/2023-08-21_03-17-30-PM.sqlite"
conn <- dbConnect(RSQLite::SQLite(), db_to_load)
dbWriteTable(conn, "city_locs", result, overwrite = T)
dbDisconnect(conn)

