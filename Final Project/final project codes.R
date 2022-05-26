
library(tidycensus)
library(tidyverse)
library(tigris)
options(tigris_use_cache = TRUE)

census_api_key("7540e4d61b8467521425225cbe8f44f7c1667f9a")
net_migration <- get_estimates(geography = "county", state = "CA",
                               variables = "RNETMIG",
                               year = 2019,
                               geometry = TRUE,
                               resolution = "20m") %>%
  shift_geometry()

net_migration

lon = c(118)
lat = c(34)
point_LA = as.data.frame(cbind(lon,lat))

order = c("-15 and below", "-15 to -5", "-5 to +5", "+5 to +15", "+15 and up")

net_migration <- net_migration %>%
  mutate(groups = case_when(
    value > 15 ~ "+15 and up",
    value > 5 ~ "+5 to +15",
    value > -5 ~ "-5 to +5",
    value > -15 ~ "-15 to -5",
    TRUE ~ "-15 and below"
  )) %>%
  mutate(groups = factor(groups, levels = order))

state_overlay <- states(
  cb = TRUE,
  resolution = "20m"
) %>%
  filter(GEOID != "72") %>%
  shift_geometry()

ggplot() +
  geom_sf(data = net_migration, aes(fill = groups, color = groups), size = 0.1) + 
  scale_fill_brewer(palette = "PuOr", direction = -1) +
  scale_color_brewer(palette = "PuOr", direction = -1, guide = FALSE)  +
  labs(title = "Net migration per 1000 residents in CA",
       subtitle = "US Census Bureau 2019 Population Estimates",
       fill = "Rate") +
  theme_minimal(base_family = "Roboto")





theme_minimal(base_family = "Roboto")
  coord_sf(datum = NA)
#+
# geom_sf(data = state_overlay, fill = NA, color = "black", size = 0.1) +
  
  
  
c = "1990/2/1"
c.data = as.Date.character(c)
c.data
class(c.data)
