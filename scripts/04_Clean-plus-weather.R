library(tidyverse)
library(RSQLite)
library(DBI)
library(lubridate)
library(daymetr)
library(stringr)


# ---- Load data ----

db_to_load <- "Data/PFW/Downloaded PFW data/2023-08-21_03-17-30-PM.sqlite"
conn <- dbConnect(SQLite(), db_to_load)
pfw <- dbGetQuery(conn, "SELECT * FROM zf_data")
listlength_dat <- dbGetQuery(conn, "SELECT * FROM listlength_tab")
sites_in_cities <- dbGetQuery(conn, "SELECT LOC_ID, city FROM city_locs")
coords <- dbGetQuery(conn, "SELECT * FROM coords_tab")
dbDisconnect(conn)


# ---- Get city avg coords ----

# Average the city of interest coords
focal_city_coords <- sites_in_cities %>%
  distinct() %>%
  left_join(coords, by = "LOC_ID") %>%
  group_by(city) %>%
  summarise(mean_latitude = mean(LATITUDE),
            mean_longitude = mean(LONGITUDE)) %>%
  ungroup() %>%
  select(name = city, latitude = mean_latitude, longitude = mean_longitude)

# Number of cities
n_cities = focal_city_coords %>% n_distinct(.$name)


# ---- Get dates ----

city_dates <- pfw %>%
  select(city, Month, Day, Year) %>%
  distinct()


# ---- Connect to db ----

# Connect to the SQLite database
db_to_load <- "Data/PFW/Downloaded PFW data/2023-08-21_03-17-30-PM.sqlite"
conn <- dbConnect(SQLite(), db_to_load)

# Loop through each city
for(i in 1:n_cities) {
  
  # Download daymet data for each city
  city_data <- download_daymet(
    lat = focal_city_coords$latitude[i], 
    lon = focal_city_coords$longitude[i], 
    start = 1988, 
    end = 2021, 
    internal = TRUE,
    simplify = FALSE)
  
  # Clean and organize data
  weather <- city_data$data %>%
    mutate(date = as.Date(paste(year, yday, sep = "-"), "%Y-%j")) %>%
    mutate(date = ymd(date)) %>%
    mutate(year = year(date),
           month = month(date),
           day = day(date),
           yday = yday(date)) %>%
    select(Year = year, Month = month, Day = day,
           tmax = tmax..deg.c., tmin = tmin..deg.c.,
           prcp = prcp..mm.day., swe = swe..kg.m.2.) %>%
    as_tibble() %>%
    mutate(city = focal_city_coords$name[i]) 
  
  # Keep only the necessary dates
  weather_dates <- city_dates %>%
    filter(city == focal_city_coords$name[i]) %>%
    inner_join(weather, by = c("city", "Month", "Day", "Year"))
  
  # Quick check for overwriting and appending
  should_overwrite = if_else(i == 1, T, F)
  should_append = if_else(should_overwrite == T, F, T)
  
  # Append the data to your SQL database
  dbWriteTable(conn, "weather_for_dates_tab", weather_dates, 
               overwrite = should_overwrite,
               append = should_append,
               row.names = FALSE)
}


# --- Now bring it all together ----

# Get weather data
weather_dates <- dbGetQuery(conn, "SELECT * FROM weather_for_dates_tab")

# Combine
pfw_final <- pfw %>%
  as_tibble() %>%
  left_join(weather_dates, by = join_by(city, Month, Day, Year)) %>%
  left_join(listlength_dat, by = c("sub_id" = "SUB_ID")) %>%
  select(city, site = loc_id, 
         year = winter, visit = week, 
         species, count = how_many,
         list_length = num_species, 
         tmax, tmin, prcp, swe) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(year = 1+(year - min(year))) %>%
  group_by(site) %>%
  mutate(first_reported_year = min(year)) %>%
  ungroup() %>%
  mutate(fye = ifelse(year == first_reported_year, 1, 0)) %>%
  select(-first_reported_year) %>%
  mutate(present = if_else(count > 0, 1, 0)) %>%
  arrange(site) %>%
  arrange(visit) %>%
  arrange(year) %>%
  mutate(site = as.factor(site)) %>%
  select(city, year, site, visit, count, species, 
         list_length, tmax, tmin, prcp, swe, 
         fye, present)


# ---- Output ----

# Append the data to your SQL database
dbWriteTable(conn, "pfw_final", pfw_final, 
             overwrite = T, row.names = FALSE)
dbDisconnect(conn)
