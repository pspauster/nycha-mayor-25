library(sf)
library(tidyverse)
library(leaflet)

nycha_development_sf <- read_sf("nycha_pact_developments_shapefile.geojson") %>% 
  mutate(dev_area = st_area(.))

eds_21 <- read_sf("2021_eds_planning") %>% 
  st_transform(crs = st_crs(nycha_development_sf)) %>% 
  st_make_valid()

eds_25 <- read_sf("2025_eds_planning/nyed_25b") %>% 
  st_transform(crs = st_crs(nycha_development_sf))


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = eds_21,
              color = "blue", weight = 1,
              fillOpacity = 0.4,
              label = ~paste("Layer1 ID:", ElectDist)) %>%  # change "some_id" to your column
  addPolygons(data = eds_25,
              color = "red", weight = 2,
              fillOpacity = 0.2,
              label = ~paste("Layer2 Info:", ElectDist)) %>%   # same here
  addPolygons(data = nycha_development_sf,
              color = "pink", weight = 2,
              fillOpacity = 0.2,
              label = ~paste("Layer3 Info:", DEVELOPMEN))

intersections_25 <- st_intersection(nycha_development_sf, eds_25) %>% 
  mutate(intersection_area = st_area(.))

intersections_21 <- st_intersection(nycha_development_sf, eds_21) %>% 
  mutate(intersection_area = st_area(.))


