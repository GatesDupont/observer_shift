#########################################################################################################
#########################################################################################################

city <-  "Washington"

#########################################################################################################
#########################################################################################################

library(tidyverse)
library(mgcv)
library(RSQLite)
library(DBI)
library(rcartocolor)


# ---- Load data ----

db_to_load <- "Data/PFW/Downloaded PFW data/2023-08-21_03-17-30-PM.sqlite"
conn <- dbConnect(SQLite(), db_to_load)

pfw_spp <- dbGetQuery(conn, 
                      "SELECT SUB_ID, LOC_ID, PROJ_PERIOD_ID, SUBNATIONAL1_CODE,
  COUNT(DISTINCT SPECIES_CODE) AS num_species
  FROM combined_data 
  WHERE SUBNATIONAL1_CODE IN ('US-NY', 'US-NJ', 'US-PA')
  GROUP BY SUB_ID, year")

ll_df <- pfw_spp %>%
  mutate(season = substr(PROJ_PERIOD_ID, 5,8)) %>%
  mutate(season = as.numeric(season)) %>%
  group_by(LOC_ID) %>%
  mutate(first_year = min(season)) %>%
  ungroup()


# ---- Model checklist length ----

# Modeling list length
m <- bam(num_species ~ ti(first_year,k=3) + ti(season) + ti(first_year, season, k = 3),
         data = ll_df, family = nb, discrete = T, gamma = 1.4, 
         control = gam.control(trace = TRUE))

pred_df <- expand.grid(first_year = seq(1989,2021,length = 500),
                       season = 2021)

fit <- predict(m,pred_df,type = "link", se.fit = T, exclude = c("ti(season)","ti(first_year,season)"))

pred_df$mean <- exp(fit$fit)
pred_df$lwr <- exp(fit$fit - 1.96 * fit$se.fit)
pred_df$upr <- exp(fit$fit + 1.96 * fit$se.fit)


# ---- Predict checklist length ----

# Figure
p3c <- rcartocolor::carto_pal(n = 7, "ag_Sunset")[1]
ggplot(pred_df, aes(x = first_year, y = mean)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.35, fill = p3c) +
  geom_line(color = p3c, linewidth = 1) +
  labs(x = "Year that observer began submitting data", y = "Predicted spcies count") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  theme_minimal(12) +
  theme(panel.grid.minor = element_blank(),
        text = element_text(family = "Lato"),
        axis.title = element_text(size = 10),
        axis.text = element_text(color = "gray50"),
        plot.title = element_text(hjust = 0.5, size = 12))
