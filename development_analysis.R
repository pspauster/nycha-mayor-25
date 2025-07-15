library(tidyverse)
library(jsonlite)
library(googlesheets4)

eds_list_21 <- readRDS("public_housing_eds_21.rds")
eds_list_25 <- readRDS("public_housing_eds_25.rds")

votes_ed_25 <- read_csv("ed_mayor_2025.csv") %>% 
  group_by(ED_num, AD_num) %>% 
  summarize(zohran25 = sum(votes[candidate == "Zohran Kwame Mamdani"], na.rm=T),
            cuomo25 = sum(votes[candidate == "Andrew M. Cuomo"], na.rm=T),
            total25 = sum(votes, na.rm=T)) %>% 
  mutate(districtid = paste0(str_sub(AD_num, start = 4, end = 5),str_pad(str_extract(ED_num, "\\d+"), 3, "left", pad = "0"))) %>% 
  filter(ED_num != "Total") %>% 
  mutate(public_housing = districtid %in% eds_list_25,
         Year = 2025)

raw21 <- fromJSON("2021_eds.json")

votes_ed_21 <- raw21 %>% 
  select(districtid, adams1, garcia1, wiley1, total1, adams8, total8) %>% 
  mutate(public_housing = districtid %in% eds_list_21,
         Year = 2021)

biggest_devs <- read_sheet("1WNwOznjEWxT-CJtkt1nlzOUaCbZbkKk9bShrK7EMSi4", sheet = 2) %>% 
  mutate(districtid = str_remove(Precinct, "-"))

development_merged_25 <- biggest_devs %>% 
  filter(Year == 2025) %>% 
  left_join(votes_ed_25, by = c("Year", "districtid"))

dev_sum_25 <- development_merged_25 %>% 
  group_by(Development) %>% 
  summarize(cuomo = sum(cuomo25, na.rm = T),
            mamdani = sum(zohran25, na.rm = T),
            total = sum(total25, na.rm = T)) %>% 
  mutate(cuomo_share = cuomo/total,
         mamdani_share = mamdani/total)

development_merged_21 <- biggest_devs %>% 
  filter(Year == 2021) %>% 
  left_join(votes_ed_21, by = c("Year", "districtid"))

dev_sum_21 <- development_merged_21 %>% 
  group_by(Development) %>% 
  summarize(adams = sum(as.integer(adams1), na.rm = T),
            garcia = sum(as.integer(garcia1), na.rm = T),
            wiley = sum(as.integer(wiley1), na.rm = T),
            total = sum(as.integer(total1), na.rm = T),
            total8 = sum(as.integer(total8), na.rm = T),
            adams8 = sum(as.integer(adams8), na.rm = T))%>% 
  mutate(adams_share = adams/total,
         garcia_wiley_share = (garcia + wiley)/total,
         adams_final_share = adams8/total8)

devs_compare <- left_join(dev_sum_21, dev_sum_25, by = "Development") %>% 
  mutate(mod_difference = adams_share - cuomo_share,
         prog_difference = garcia_wiley_share - mamdani_share,
         final_to_first = adams_final_share - cuomo_share,
         vote_diff = total.x-total.y)

write_csv(devs_compare, "output-devs.csv")



