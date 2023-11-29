require(tidyverse)

# Gates Dupont
# gatesdupont@princeton.edu


# ---- Functions ----

# Function to prepare raw PFW data for modeling
prep_PFW_data <- function(df){
  
  # Prep the data
  result <- df %>%
    # Turn sites and species to factors
    mutate(site = as.factor(site)) %>%
    mutate(species = as.factor(species)) %>%
    # Make first year effect variable
    group_by(site) %>%
    mutate(total_years = n_distinct(year)) %>%
    filter(total_years > 1) %>%
    mutate(first_year = min(year)) %>%
    mutate(n_years = year - first_year) %>%
    mutate(fye = if_else(n_years > 0, 1, 0)) %>%
    ungroup(site) %>%
    select(-n_years) %>%
    mutate(present = if_else(count > 0, 1, 0)) %>%
    mutate(species_observed = as.factor(present)) %>%
    select(-total_years, -first_year)
  
  return(result)
  
}