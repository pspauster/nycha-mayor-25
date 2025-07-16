library(tidyverse)
library(jsonlite)
library(googlesheets4)
library(sf)


eds_list_21 <- readRDS("public_housing_eds_21.rds")
eds_list_25 <- readRDS("public_housing_eds_25.rds")

biggest_devs <- read_sheet("1WNwOznjEWxT-CJtkt1nlzOUaCbZbkKk9bShrK7EMSi4", sheet = 2) %>% 
  mutate(districtid = str_remove(Precinct, "-"))

selected_dev_codes_25 <- biggest_devs %>% filter(Year == 2025) %>% pull(districtid)

cols <- c("cuomo_share", "mamdani_share", "lander_share")

votes_ed_25 <- read_csv("ed_mayor_2025.csv") %>% 
  group_by(ED_num, AD_num) %>% 
  summarize(zohran25 = sum(votes[candidate == "Zohran Kwame Mamdani"], na.rm=T),
            cuomo25 = sum(votes[candidate == "Andrew M. Cuomo"], na.rm=T),
            lander25 = sum(votes[candidate == "Brad Lander"], na.rm = T),
            total25 = sum(votes, na.rm=T)) %>% 
  mutate(districtid = paste0(str_sub(AD_num, start = 4, end = 5),str_pad(str_extract(ED_num, "\\d+"), 3, "left", pad = "0"))) %>% 
  filter(ED_num != "Total") %>% 
  mutate(public_housing = districtid %in% eds_list_25,
         selected_devs = districtid %in% selected_dev_codes_25,
         cuomo_share = cuomo25/total25,
         mamdani_share = zohran25/total25,
         lander_share = lander25/total25,
         leader_name = case_when(
           zohran25 > cuomo25 & zohran25 > lander25 ~ "Mamdani",
           cuomo25 > zohran25 & cuomo25 > lander25 ~ "Cuomo",
           lander25 > cuomo25 & lander25 > cuomo25 ~ "Lander",
           total25 > 0 ~ "Tie"
         ),
         category = case_when(
           # Mamdani
           leader_name == "Mamdani" & mamdani_share >= 0   & mamdani_share < 0.4 ~ "Mamdani 0‑40%",
           leader_name == "Mamdani" & mamdani_share >= 0.4 & mamdani_share < 0.6 ~ "Mamdani 40‑60%",
           leader_name == "Mamdani" & mamdani_share >= 0.6 & mamdani_share < 0.8 ~ "Mamdani 60‑80%",
           leader_name == "Mamdani" & mamdani_share >= 0.8                        ~ "Mamdani 80%+",
           
           # Cuomo
           leader_name == "Cuomo"   & cuomo_share   >= 0   & cuomo_share   < 0.4 ~ "Cuomo 0‑40%",
           leader_name == "Cuomo"   & cuomo_share   >= 0.4 & cuomo_share   < 0.6 ~ "Cuomo 40‑60%",
           leader_name == "Cuomo"   & cuomo_share   >= 0.6 & cuomo_share   < 0.8 ~ "Cuomo 60‑80%",
           leader_name == "Cuomo"   & cuomo_share   >= 0.8                        ~ "Cuomo 80%+",
           
           # Lander
           leader_name == "Lander"  & lander_share  >= 0   & lander_share  < 0.4 ~ "Lander 0‑40%",
           leader_name == "Lander"  & lander_share  >= 0.4 & lander_share  < 0.6 ~ "Lander 40‑60%",
           leader_name == "Lander"  & lander_share  >= 0.6 & lander_share  < 0.8 ~ "Lander 60‑80%",
           leader_name == "Lander"  & lander_share  >= 0.8                        ~ "Lander 80%+"         ,
           
           leader_name == "Tie" ~ "Tie")
  )

write_csv(votes_ed_25, "2025_results.csv")

write_csv(votes_ed_25 %>% filter(selected_devs==T), "2025_selected_results.csv")

nycha_development_sf <- read_sf("nycha_pact_developments_shapefile.geojson") %>% 

    mutate(dev_area = st_area(.)) %>% 
  st_make_valid()

eds_25 <- read_sf("2025_eds_planning/nyed_25b") %>% 
  st_transform(crs = st_crs(nycha_development_sf)) %>% 
  mutate(districtid = as.character(ElectDist))

geo_results <- left_join(eds_25, votes_ed_25, by = "districtid")

write_sf(geo_results, "geo_25_results.geojson", append = F, delete_layer = T)

votes_ed_21 <- fromJSON("2021_eds.json") %>% select(districtid, adams1, total1) %>% 
  mutate(public_housing = districtid %in% eds_list_21)

votes_ed_25 %>%
  ungroup() %>% 
  group_by(public_housing) %>% 
  summarize(cuomo = sum(cuomo25, na.rm = T),
                                      mamdani = sum(zohran25, na.rm = T),
                                      total = sum(total25, na.rm = T)) %>% 
  mutate(cuomo_share = cuomo/total,
         mamdani_share = mamdani/total)


votes_ed_21 %>%
  ungroup() %>% 
  group_by(public_housing) %>% 
  summarize(adams = sum(as.integer(adams1), na.rm = T),
            total = sum(as.integer(total1), na.rm = T))%>% 
  mutate(adams_share = adams/total)

xwalk_25 <- readRDS("xwalk_25.rds")
xwalk_21 <- readRDS("xwalk_21.rds")

development_25 <- votes_ed_25 %>%
  filter(public_housing == TRUE) %>% 
  left_join(xwalk_25, by = "districtid") %>% 
  ungroup() %>% 
  group_by(DEVELOPMEN) %>% 
  summarize(cuomo = sum(cuomo25, na.rm = T),
            mamdani = sum(zohran25, na.rm = T),
            total = sum(total25, na.rm = T)) %>% 
  mutate(cuomo_share = cuomo/total,
         mamdani_share = mamdani/total)
  
development_21 <- votes_ed_21 %>%
  filter(public_housing == TRUE) %>% 
  left_join(xwalk_25, by = "districtid") %>% 
  ungroup() %>% 
  group_by(DEVELOPMEN) %>% 
  summarize(adams = sum(as.integer(adams1), na.rm = T),
            total = sum(as.integer(total1), na.rm = T))%>% 
  mutate(adams_share = adams/total)

joined <- full_join(development_21, development_25, by = "DEVELOPMEN") %>% 
  mutate(adams_cuomo_swing = adams_share - cuomo_share)
  
