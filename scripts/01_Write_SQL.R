library(dplyr)
library(vroom)
library(RSQLite)
library(DBI)


# ---- Create SQL database ----

# TABLE FROM PFW DATA

# Load csv filenames 
# (NOTE: USER WILL HAVE TO PROPOGATE THIS FOLDER WITH LARGE CSV FILES FROM THE URL BELOW)
# https://feederwatch.org/explore/raw-dataset-requests/
files <- list.files(path = "Data/PFW/Downloaded PFW data/", 
                    pattern="*.csv", full.names = T)

# Create unique db name
db_name <- paste0("Data/PFW/Downloaded PFW data/", format(Sys.time(), "%Y-%m-%d_%I-%M-%S-%p"), ".sqlite")

# Create and connect to new db
conn <- dbConnect(SQLite(), db_name)

# Loop through files and append them to the db
for (file in files) {
  
  vroom(file) %>%
    rename_all(toupper) %>%
    select(LOC_ID, PROJ_PERIOD_ID, LATITUDE, LONGITUDE, SUBNATIONAL1_CODE, ENTRY_TECHNIQUE,
           SPECIES_CODE, HOW_MANY, SUB_ID, MONTH, DAY, YEAR, VALID) %>%
    dbWriteTable(conn, "combined_data", ., append=TRUE, row.names=FALSE)
  
  
}

# Add another table for checklist length
ListLength_query <- "SELECT SUB_ID, COUNT(SPECIES_CODE) as num_species
                      FROM combined_data
                      GROUP BY SUB_ID"
ListLength_tab <- dbGetQuery(conn, ListLength_query)
dbWriteTable(conn, "listlength_tab", ListLength_tab)

# Add another table for loc_ids with coords
coords_tab <- dbGetQuery(
  conn, "SELECT DISTINCT LOC_ID, LATITUDE, LONGITUDE FROM combined_data")
dbWriteTable(conn, "coords_tab", coords_tab)

# Add another table for species codes to names translation
spp_names_path <- "~/Data/PFW/PFW supplementary data/PFW_spp_translation_table_May2023.csv"
spp_names_tab <- vroom(spp_names_path) %>%
  select(Abbrev = species_code, Full = american_english_name)
dbWriteTable(conn, "spp_names", spp_names_tab)

# Disconnect from the db
dbDisconnect(conn)
