library(sf)
library(tidyverse)
library(leaflet)
library(geojsonio)

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

eds_25 %>% st_simplify(0.001) %>% st_write(dsn = "2025_simplified.geojson", append = FALSE, delete_dsn = TRUE)

ed_result_25 <- read_csv("2025_results.csv")

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


map_25_results <- full_join(eds_25, ed_result_25, by = c("ElectDist" = "districtid")) %>% 
  rowwise() %>% 
  mutate(
    winner = c("zohran25", "cuomo25", "lander25")[which.max(c_across(zohran25:lander25))],
    # 2a. Keep the winning value handy
    win_value = case_when(
      winner == "val1" ~ zohran25,
      winner == "val2" ~ cuomo25,
      TRUE             ~ lander25
    )
  ) %>% 
  ungroup()



#intersect developments and Eds
# intersections_25 <- st_intersection(nycha_development_sf, eds_25) # %>% 
#   mutate(intersection_area = st_area(.),
#          per_ed_covered = intersection_area/ed_area)
# 
# intersections_25_flat <- intersections_25 %>%
#   as.data.frame() %>% 
#   group_by(ElectDist) %>% 
#   summarize(intersection_area_sum = sum(intersection_area),
#             per_ed_covered = intersection_area_sum/ed_area)

#get coverage score by ED for filtering
big_nycha_25 <- nycha_development_sf %>% 
  summarize(geometry = st_union(geometry)) %>% 
  st_intersection(eds_25) %>% 
  mutate(intersection_area = st_area(.),
         per_ed_covered = intersection_area/ed_area)

eds_list_25 <- big_nycha_25 %>% 
  filter(as.numeric(per_ed_covered) > 0.25) %>% 
  pull(ElectDist)

intersections_25 <- st_intersection(nycha_development_sf, eds_25) %>% 
  mutate(in_list = ElectDist %in% eds_list_25)

intersections_25_xwalk <- intersections_25 %>% 
  as.data.frame() %>% 
  mutate(districtid = as.character(ElectDist)) %>% 
  select(DEVELOPMEN, districtid) %>% 
  distinct()

saveRDS(intersections_25_xwalk, "xwalk_25.rds")

eds_25_flagged <- eds_25 %>% 
  mutate(in_list = ElectDist %in% eds_list_25)

saveRDS(eds_list_25, "public_housing_eds_25.rds")

# repeat for 21
# intersections_21 <- st_intersection(nycha_development_sf, eds_21) %>% 
#   mutate(intersection_area = st_area(.),
#          per_ed_covered = intersection_area/ed_area)
# 
# intersections_21_flat <- intersections_21 %>% 
#   as.data.frame() %>% 
#   group_by(ElectDist) %>% 
#   summarize(intersection_area = sum(intersection_area),
#             per_ed_covered = intersection_area/ed_area)

big_nycha_21 <- nycha_development_sf %>% 
  summarize(geometry = st_union(geometry)) %>% 
  st_intersection(eds_21) %>% 
  mutate(intersection_area = st_area(.),
         per_ed_covered = intersection_area/ed_area)

eds_list_21 <- big_nycha_21 %>% 
  filter(as.numeric(per_ed_covered) > 0.25) %>% 
  pull(ElectDist)

intersections_21 <- st_intersection(nycha_development_sf, eds_21) %>% 
  mutate(in_list = ElectDist %in% eds_list_21)

intersections_21_xwalk <- intersections_21 %>% 
  as.data.frame() %>% 
  mutate(districtid = as.character(ElectDist)) %>% 
  select(DEVELOPMEN, districtid) %>% 
  distinct()

saveRDS(intersections_21_xwalk, "xwalk_21.rds")

eds_21_flagged <- eds_21 %>% 
  mutate(in_list = ElectDist %in% eds_list_21)

saveRDS(eds_list_21, "public_housing_eds_21.rds")

# map based on individual property map
pal <- colorFactor(palette = c("gray", "red"), domain =c(TRUE, FALSE))
pal2 <- colorFactor(palette = c("gray", "blue"), domain =c(TRUE, FALSE))

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>%

  addPolygons(data = eds_25_flagged,
              fillColor = ~pal(in_list), weight = 2,
              color='gray',
              fillOpacity = .7,
              label = ~paste("Layer1 Info:", ElectDist)) %>% 
  addPolygons(data = eds_21_flagged,
              fillColor = ~pal2(in_list), weight = 2,
              color = "gray",
              fillOpacity = .7,
              label = ~paste("Layer1 Info:", ElectDist)) %>% 
  addPolygons(data = nycha_development_sf,
              color = "black", weight = 2,
              fillOpacity = 0,
              #fillColor = ~pal(as.numeric(per_ed_covered)),
              label = ~paste("Layer2 Info:", DEVELOPMEN)) 





