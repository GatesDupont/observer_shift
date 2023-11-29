# Gates Dupont
# gatesdupont@princeton.edu

library(tidyverse)
library(lubridate)
library(RSQLite)
library(DBI)
library(stringr)
source("scripts/00_Functions.R")

# COI: city of interest


# ---- Load the SQL database ----

db_to_load <- "Data/PFW/Downloaded PFW data/2023-08-21_03-17-30-PM.sqlite"
conn <- dbConnect(SQLite(), db_to_load)
pfw_all <- dbGetQuery(conn, "SELECT * FROM combined_data")
sites_in_cities <- dbGetQuery(conn, "SELECT * FROM city_locs")
spp_fullnames <- dbGetQuery(conn, "SELECT * FROM spp_names")
dbDisconnect(conn)

# Species to remove
rm_sp <- c("baleag", "ambduc", "blackb", "buffle", "comgol", "commer", 
           "doccor", "gbbgul", "hergul", "hoomer", "rufgro", "turvul", "wooduc", 
           "amewig", "gadwal", "rosfin", "snogoo", "y00326", "y00033", "x00004")


# ---- Select city sites, clean up species names ----

pfw_cities <- pfw_all %>%
  as_tibble() %>%
  # Join by loc-year pairs, keep only obs from COIs
  mutate(winter = substr(PROJ_PERIOD_ID, 5, 8)) %>%
  mutate(loc_winter = paste0(LOC_ID, "_", winter)) %>%
  left_join(sites_in_cities, by = "loc_winter") %>%
  mutate(LOC_ID = LOC_ID.x, winter = winter.y) %>%
  filter(!is.na(city)) %>%
  # Remove non-feeder species
  filter(!(SPECIES_CODE %in% rm_sp)) %>%
  # Add full names
  left_join(spp_fullnames, by = c("SPECIES_CODE" = "Abbrev")) %>%
  mutate(species = Full) %>%
  filter(VALID == 1) %>% 
  # Keep only what's required
  select(species, HOW_MANY, LOC_ID, SUB_ID, 
         Month = MONTH, Day = DAY, Year = YEAR, winter, city) %>%
  # Remove NA cells
  na.omit() %>%
  # Edit some species
  # need to combine BCCH and CACH in DC
  mutate(species = ifelse(city == "Washington" & species == "Black-capped Chickadee", 
                          "Carolina Chickadee", 
                          species)) %>%
  # need to combine Aphelocoma sp with California Scrub-Jay in SF
  mutate(species = ifelse(city == "San Francisco" & species == "Aphelocoma sp.", 
                          "California Scrub-Jay", 
                          species)) %>%
  # need to remove hummingbirds
  filter(!grepl("hummingbird", species, ignore.case = TRUE)) %>%
  # Combine subspecies (by removing parentheticals)
  mutate(species = str_extract(species, "^[^(]+")) %>%
  mutate(species = str_trim(species)) %>%
  group_by(SUB_ID, species) %>%
  mutate(HOW_MANY = sum(HOW_MANY)) %>%
  ungroup() %>%
  distinct()


#---- Reformat the dates ----

# Julian weeks to PFW weeks
to_pfw_weeks <- data.frame(
  real_week = c(46:52, 1:17),
  pfw_week = 1:24)

# Convert data to date format, join, clean 
pfw_cities_formatdate <- pfw_cities %>%
  mutate(date = mdy(paste(Month, Day, Year, "/"))) %>%
  mutate(week = week(date)) %>%
  filter(week != 45) %>%
  filter(week != 53) %>%
  left_join(to_pfw_weeks, by = c("week" = "real_week")) %>%
  mutate(week = pfw_week) %>%
  select(species, how_many = HOW_MANY, loc_id = LOC_ID, 
         sub_id = SUB_ID, week, winter, Month, Day, Year, city) %>%
  na.omit()


# ---- Thin to weekly checklists for each loc_id ----

# Thin to 1 loc-week list, randomly
pfw_cities_formatdate_thinned <- pfw_cities_formatdate %>%
  select(loc_id, week, winter, sub_id,  Month, Day, Year) %>%
  group_by(loc_id, week, winter) %>%
  slice_sample(n = 1) %>%
  ungroup() %>%
  select(sub_id) %>%
  left_join(pfw_cities_formatdate, by = "sub_id") %>%
  select(species, how_many, loc_id, sub_id, week, winter, city, Month, Day, Year)


# --- Filter Data for Winter Months and 20 checklists at least 2/year ---

pfw_cities_formatdate_thinned_winter <- pfw_cities_formatdate_thinned %>%
  filter(winter %in% c(1989:2021),
         week %in% c(1:19)) %>%
  group_by(loc_id, winter, city) %>%
  filter(n_distinct(sub_id) >= 2) %>%
  ungroup() %>%
  group_by(loc_id, city) %>%
  filter(n_distinct(sub_id) >= 20) %>%
  ungroup()

# Plot to check
pfw_cities_formatdate_thinned_winter %>% 
  group_by(city, winter) %>% 
  summarise(n = n_distinct(loc_id)) %>% 
  ungroup() %>%
  mutate(winter = as.numeric(as.character(winter))) %>%
  ggplot(., aes(x = winter, y = n)) + 
  facet_wrap(~city, scales = "free") +
  geom_hline(yintercept = 20) +
  geom_point() + 
  geom_line() +
  ylim(0,NA)


# --- Species selection by prevalence ---

# Total number of cites per city
city_nsites <- pfw_cities_formatdate_thinned_winter %>%
  group_by(city) %>%
  summarise(total_sites = n_distinct(loc_id)) %>%
  ungroup() %>%
  arrange(desc(total_sites))

# Total number of sites at which each species is observed
city_species_nsites <- pfw_cities_formatdate_thinned_winter %>%
  group_by(city, species) %>%
  summarise(species_sites = n_distinct(loc_id)) %>%
  ungroup() %>%
  arrange(desc(species_sites))

# Select species based on site prevalence in each city
top_species_in_city <- city_species_nsites %>%
  left_join(city_nsites, by = "city") %>%
  filter(species_sites / total_sites >= 0.4) %>%
  select(city, species) %>%
  arrange(species) %>%
  arrange(city)

# top_species_in_city %>%
#   group_by(city) %>%
#   summarise(n = n_distinct(species)) %>%
#   ungroup() %>%
#   arrange(desc(n))

# Use semijoin to keep only the top species
pfw_cities_formatdate_thinned_winter_topspp <- pfw_cities_formatdate_thinned_winter %>%
  semi_join(top_species_in_city, by = c("city", "species")) %>%
  select(species, how_many, loc_id, sub_id, week, winter, city,  Month, Day, Year)


# ---- Zero-fill the data ----

# Zero-fill the df
pfw_extracted <- pfw_cities_formatdate_thinned_winter_topspp %>%
  group_by(city) %>%
  complete(species = species,
           nesting(loc_id, week, winter, sub_id, Month, Day, Year),
           fill = list(how_many = 0,
                       sub_id = "none")) %>%
  ungroup()


# ---- Check ZF ----

analytical_solution <- pfw_cities_formatdate_thinned_winter_topspp %>%
  select(city, sub_id, species) %>%
  group_by(city) %>%
  summarise(n_checklists = n_distinct(sub_id),
            n_species = n_distinct(species)) %>%
  ungroup() %>%
  mutate(total = n_checklists * n_species) %>%
  pull(total) %>%
  sum()

observed_n <- nrow(pfw_extracted)

observed_n == analytical_solution


# ---- Export! ----

db_to_load <- "Data/PFW/Downloaded PFW data/2023-08-21_03-17-30-PM.sqlite"
conn <- dbConnect(RSQLite::SQLite(), db_to_load)
dbWriteTable(conn, "zf_data", pfw_extracted, overwrite = T)
dbDisconnect(conn)
