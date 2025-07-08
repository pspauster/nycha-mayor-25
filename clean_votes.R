library(tidyverse)
library(jsonlite)

votes_ed_25 <- read_csv("ed_mayor_2025.csv") %>% 
  group_by(ED_num, AD_num) %>% 
  summarize(zohran25 = sum(votes[candidate == "Zohran Kwame Mamdani"], na.rm=T),
            cuomo25 = sum(votes[candidate == "Andrew M. Cuomo"], na.rm=T),
            total25 = sum(votes, na.rm=T)) %>% 
  mutate(districtid = paste0(str_sub(AD_num, start = 4, end = 5),str_pad(str_extract(ED_num, "\\d+"), 3, "left", pad = "0"))) %>% 
  filter(ED_num != "Total")


votes_ed_21 <- fromJSON("2021_eds.json") %>% select(districtid, adams1, total1) %>% 
  mutate(public_housing = districtid %in% eds_list_21)

eds_list_21 <- readRDS("public_housing_eds_21.rds")
eds_list_25 <- readRDS("public_housing_eds_25.rds")
