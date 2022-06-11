# start with Feb since we delete the first row of unprocessed data
processed = ts(unprocessed[,-1],frequency = 12, start = c(1990,2)) 

# plot the LA unemployment Rate 1990-2022
autoplot.zoo(processed[,"unemploy_rate_la"])+
  ggtitle("Unemployment Rate in LA County") +
  xlab("Year") +
  ylab("Percentage%")

# observe the seasoality
ggseasonplot(processed[,"unemploy_rate_la"]) +
  ggtitle("Seasonal Plot of Unemployment Rate in LA County") +
  xlab("Year") +
  ylab("Percentage%")


ggsubseriesplot(processed[,"unemploy_rate_la"]) +
  ylab("% percentage") +
  ggtitle("Seasonal subseries plot: LA Unemployment Rate")



#------------------------------------------------------------------------
# A try of Spatial Analysis 

census_api_key("7540e4d61b8467521425225cbe8f44f7c1667f9a")
net_migration <- get_estimates(geography = "county", state = "CA",
                               variables = "RNETMIG",
                               year = 2015,
                               geometry = TRUE,
                               resolution = "20m") %>%
  shift_geometry()

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

# plot the net migration of LA county (purple part with islands)

ggplot() +
  geom_sf(data = net_migration, aes(fill = groups, color = groups), size = 0.1) + 
  scale_fill_brewer(palette = "PuOr", direction = -1) +
  scale_color_brewer(palette = "PuOr", direction = -1, guide = FALSE)  +
  labs(title = "Net migration per 1000 residents in CA",
       subtitle = "US Census Bureau 2015 Population Estimates",
       fill = "Rate") +
  theme_minimal(base_family = "Roboto")

#median age
med_age <- get_acs(state = "CA", county = "Los Angeles", geography = "tract", 
                   variables = "B01002_001", geometry = TRUE)
med_age %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma")


# correlation graph 
train_2016 %>% select(contains("la")) %>%
  cor() %>%
  corrplot(type = "upper", tl.pos = "td",
           method = "circle", tl.cex = 0.5, tl.col = 'black',
           order = "hclust", diag = FALSE)


# ACF&PACF Diagnostics 
data %>% 
  plot_acf_diagnostics(DATA, unemploy_rate_la, .lags = 100, .interactive = F)


