library(tidyverse)
library(getarc)
library(sf)

nycha_dev_layer <- query_layer("https://services1.arcgis.com/0mhlGDIUKwQF6tnJ/arcgis/rest/services/NYCHA_developments_public2017/FeatureServer/0") %>% 
                                 mutate(layer = "NYCHA")

pact_dev_layer <- query_layer("https://services1.arcgis.com/0mhlGDIUKwQF6tnJ/arcgis/rest/services/20200212_RADPACT_Layer/FeatureServer/1") %>% 
  mutate(layer = "PACT",
         TOTAL_UNIT = TOTAL_DU)

nycha_pact_sf <- bind_rows(nycha_dev_layer, pact_dev_layer)

write_sf(nycha_pact_sf, "nycha_pact_developments_shapefile.geojson", append = F)
