library(tidyverse)
library(jsonlite)

eds_list_21 <- readRDS("public_housing_eds_21.rds")
eds_list_25 <- readRDS("public_housing_eds_25.rds")

votes_ed_25 <- read_csv("ed_mayor_2025.csv") %>% 
  group_by(ED_num, AD_num) %>% 
  summarize(zohran25 = sum(votes[candidate == "Zohran Kwame Mamdani"], na.rm=T),
            cuomo25 = sum(votes[candidate == "Andrew M. Cuomo"], na.rm=T),
            total25 = sum(votes, na.rm=T)) %>% 
  mutate(districtid = paste0(str_sub(AD_num, start = 4, end = 5),str_pad(str_extract(ED_num, "\\d+"), 3, "left", pad = "0"))) %>% 
  filter(ED_num != "Total") %>% 
  mutate(public_housing = districtid %in% eds_list_25)

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
  
