#########################################################################################################
#########################################################################################################

city <-  "Washington"

#########################################################################################################
#########################################################################################################


library(tidyverse)
library(mgcv)
library(RSQLite)
library(DBI)
slice <- dplyr::slice


# ---- Load data ----

db_to_load <- "Data/PFW/Downloaded PFW data/2023-08-21_03-17-30-PM.sqlite"
conn <- dbConnect(SQLite(), db_to_load)
query <- paste0("SELECT * FROM pfw_final WHERE city = '", city, "'")
pfw_cleaned <- dbGetQuery(conn, query) %>%
  mutate(site = as.factor(site)) %>%
  mutate(n_periods = DAY1_AM + DAY1_PM + DAY2_AM + DAY2_PM) %>%
  select(-DAY1_AM, -DAY1_PM, -DAY2_AM, -DAY2_PM) %>%
  mutate(site = as.factor(site)) %>%
  mutate(visit = as.double(visit)) %>%
  mutate(species = as.factor(species)) %>%
  mutate(list_length = as.double(list_length)) %>%
  mutate(n_periods = as.numeric(n_periods))
dbDisconnect(conn)

# Minimize to rows as distinct checklists
ll_df <- pfw_cleaned %>%
  select(year, site, fye, visit, tmin, swe, list_length, n_periods) %>%
  distinct()


# ---- Modeling observed species richness (species count) ----

# Fill model
c <- bam(list_length ~ s(site, bs = "re") +
           fye + n_periods +
           ti(tmin, k = 3, fx = T) + ti(visit, k = 3, fx = T) + ti(year, k = 3, fx = T),
         data = ll_df, 
         family = poisson, 
         gamma = 1.4, 
         discrete = T,
         control = gam.control(trace = TRUE))

# Intercept only
c0 <- bam(list_length ~ placeholder, data = ll_df %>% mutate(placeholder = 1), family = poisson)

# All univariate models
c1 <- bam(list_length ~ s(site, bs = "re"),
          data = ll_df, 
          family = poisson, 
          gamma = 1.4, 
          discrete = T,
          control = gam.control(trace = TRUE))

c2 <- bam(list_length ~ 
            fye,
          data = ll_df, 
          family = poisson, 
          gamma = 1.4, 
          # discrete = T,
          control = gam.control(trace = TRUE))

c3 <- bam(list_length ~ n_periods,
          data = ll_df, 
          family = poisson, 
          gamma = 1.4, 
          # discrete = T,
          control = gam.control(trace = TRUE))

c4 <- bam(list_length ~
            ti(tmin, k = 3, fx = T),
          data = ll_df, 
          family = poisson, 
          gamma = 1.4, 
          # discrete = T,
          control = gam.control(trace = TRUE))

c5 <- bam(list_length ~ 
            ti(visit, k = 3, fx = T),
          data = ll_df, 
          family = poisson, 
          gamma = 1.4, 
          # discrete = T,
          control = gam.control(trace = TRUE))

c6 <- bam(list_length ~ ti(year, k = 3, fx = T),
          data = ll_df, 
          family = poisson, 
          gamma = 1.4, 
          # discrete = T,
          control = gam.control(trace = TRUE))


# ---- Record R-sq values into data frame -----

fortab4 <- data.frame(
  term = c("Observer", "First year effect","Number of observation periods",
           "Daily minimum temperature","Week of year","Year"),
  ve = c(
    summary(c1)$r.sq,
    summary(c2)$r.sq,
    summary(c3)$r.sq,
    summary(c4)$r.sq,
    summary(c5)$r.sq,
    summary(c6)$r.sq
  )
) %>% arrange(desc(ve)) %>%
  mutate(ve = round(ve,2))


# ---- Figure ----

ggplot(fortab4, aes(x = reorder(term, ve), y = ve)) +
  geom_hline(yintercept = sum(fortab4$ve), linetype = "dashed", linewidth = 0.25) +
  geom_hline(yintercept = summary(c)$r.sq, linetype = "dashed", linewidth = 0.25) +
  geom_col(fill = "navy") +
  coord_flip() +
  ggtext::geom_richtext(angle = -90, y = sum(fortab4$ve), x = 3.5, size = 3, label.size = 0,
                        label = "Sum total", color = "white") +
  ggtext::geom_richtext(angle = -90, y = summary(c)$r.sq, x = 3.5, size = 3, label.size = 0,
                        label = "Amount from combined model", color = "white") +
  
  geom_text(angle = -90, y = sum(fortab4$ve), x = 3.5, size = 3, 
            label = "Sum total", check_overlap = T) +
  geom_text(angle = -90, y = summary(c)$r.sq, x = 3.5, size = 3,
            label = "Amount from combined model", check_overlap = T) +
  
  scale_y_continuous(labels = scales::label_percent(scale = 100)) +
  labs(y = "Amount of variation explained (univariate)", x = NULL) +
  theme_minimal(13) +
  theme(aspect.ratio = 1/2,
        axis.text.y = element_text(color = "black"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

