# Objective 1
library(leaflet)
library(dplyr)

# Create data frame with country total confirmed cases and mean long/lat
global_conf_totals <- confirmed_global %>% 
  mutate(conf = rowSums(across(5:ncol(confirmed_global)))) %>% 
  group_by(`Country/Region`) %>% 
  summarize(mean_lat = mean(Lat, na.rm=TRUE),
            mean_long = mean(Long, na.rm=TRUE),
            total_conf = sum(conf, na.rm=TRUE))

# Find levels
levels_conf <- quantile(global_conf_totals$total_conf,
                           prob=c(.2,.4,.6,.8),
                           type=1)

# Set rank levels
rank_levels <- c('very low', 'low', 'mid', 'high', 'very high')

# Add rank levels to the data frame
global_conf_rank <- global_conf_totals %>% 
  mutate(rank = factor(case_when(
    total_conf <= levels_conf[1] ~ 'very low',
    total_conf <= levels_conf[2] ~ 'low',
    total_conf <= levels_conf[3] ~ 'mid',
    total_conf <= levels_conf[4] ~ 'high',
    TRUE ~ 'very high'),
    levels = rank_levels))

# Create data frame with death totals and mean long/lat
global_death_totals <- deaths_global %>% 
  mutate(deaths = rowSums(across(5:ncol(deaths_global)))) %>% 
  group_by(`Country/Region`) %>% 
  summarize(mean_lat = mean(Lat, na.rm=TRUE),
            mean_long = mean(Long, na.rm=TRUE),
            total_deaths = sum(deaths, na.rm=TRUE))

# Find levels
levels_death <- quantile(global_death_totals$total_deaths,
                            prob=c(.2,.4,.6,.8),
                            type=1)

# Add rank levels to the data frame
global_deaths_rank <- global_death_totals %>% 
  mutate(rank = factor(case_when(
    total_deaths <= levels_death[1] ~ 'very low',
    total_deaths <= levels_death[2] ~ 'low',
    total_deaths <= levels_death[3] ~ 'mid',
    total_deaths <= levels_death[4] ~ 'high',
    TRUE ~ 'very high'),
    levels = rank_levels))

# Map colors to levels
cols <- colorFactor(palette = c('darkblue', 'lightblue', 'grey', 'orange', 'red'),
                    levels = c('very low', 'low', 'mid', 'high', 'very high'))

# Create leaflet map visualization
leaflet() %>% 
  addProviderTiles('CartoDB.Positron') %>% 
  setView(lat = 15, lng = 0, zoom = 1.5) %>% 
  addCircleMarkers(data = global_conf_rank,
                   lng = ~mean_long,
                   lat = ~mean_lat,
                   color = ~cols(rank),
                   radius = 4,
                   opacity = 0.75,
                   label = ~`Country/Region`,
                   group = 'Confirmed') %>% 
  addCircleMarkers(data = global_deaths_rank,
                   lng = ~mean_long,
                   lat = ~mean_lat,
                   color = ~cols(rank),
                   radius = 4,
                   opacity = 0.75,
                   label = ~`Country/Region`,
                   group = 'Deaths') %>% 
  addLayersControl(overlayGroups = c('Confirmed', 'Deaths'),
                   options = layersControlOptions(collapsed = FALSE)) %>% 
  addLegend(position = "bottomleft",
            pal = cols,
            values = global_conf_rank$rank,
            title = 'COVID-19 Severity',
            opacity = 1)


