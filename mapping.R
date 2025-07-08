library(sf)
library(tidyverse)
library(leaflet)

nycha_development_sf <- read_sf("nycha_pact_developments_shapefile.geojson") %>% 
  mutate(dev_area = st_area(.)) %>% 
  st_make_valid()

eds_21 <- read_sf("2021_eds_planning_clipped/nyed_21b") %>% 
  st_transform(crs = st_crs(nycha_development_sf)) %>% 
  st_make_valid() %>% 
  mutate(ed_area = st_area(.))

eds_25 <- read_sf("2025_eds_planning/nyed_25b") %>% 
  st_transform(crs = st_crs(nycha_development_sf)) %>% 
  mutate(ed_area = st_area(.))

#map all layers
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

#intersect developments and Eds
intersections_25 <- st_intersection(nycha_development_sf, eds_25) %>% 
  mutate(intersection_area = st_area(.),
         per_ed_covered = intersection_area/ed_area)

intersections_25_flat <- intersections_25 %>%
  as.data.frame() %>% 
  group_by(ElectDist) %>% 
  summarize(intersection_area_sum = sum(intersection_area),
            per_ed_covered = intersection_area_sum/ed_area)

#get coverage score by ED for filtering
big_nycha_25 <- nycha_development_sf %>% 
  summarize(geometry = st_union(geometry)) %>% 
  st_intersection(eds_25) %>% 
  mutate(intersection_area = st_area(.),
         per_ed_covered = intersection_area/ed_area)

eds_list_25 <- big_nycha_25 %>% 
  filter(as.numeric(per_ed_covered) > 0.5) %>% 
  pull(ElectDist)

# repeat for 21
intersections_21 <- st_intersection(nycha_development_sf, eds_21) %>% 
  mutate(intersection_area = st_area(.),
         per_ed_covered = intersection_area/ed_area)

intersections_21_flat <- intersections_21 %>% 
  as.data.frame() %>% 
  group_by(ElectDist) %>% 
  summarize(intersection_area = sum(intersection_area),
            per_ed_covered = intersection_area/ed_area)

big_nycha_21 <- nycha_development_sf %>% 
  summarize(geometry = st_union(geometry)) %>% 
  st_intersection(eds_21) %>% 
  mutate(intersection_area = st_area(.),
         per_ed_covered = intersection_area/ed_area)

eds_list_21 <- big_nycha_21 %>% 
  filter(as.numeric(per_ed_covered) > 0.5) %>% 
  pull(ElectDist)


# map based on individual property map
pal <- colorNumeric(palette = "YlOrRd", domain = as.numeric(intersections_25$per_ed_covered))

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>%

  addPolygons(data = eds_25,
              color = "blue", weight = 2,
              fillOpacity = 0,
              label = ~paste("Layer2 Info:", ElectDist)) %>% 
  addPolygons(data = intersections_25,
              color = "black", weight = 2,
              fillOpacity = 0.8,
              fillColor = ~pal(as.numeric(per_ed_covered)),
              label = ~paste("Layer2 Info:", ElectDist, per_ed_covered)) 



