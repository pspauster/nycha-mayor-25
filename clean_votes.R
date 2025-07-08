library(tidyverse)
library(jsonlite)

votes_ed_25 <- read_csv("ed_mayor_2025.csv") %>% 
  group_by(ED_num, AD_num) %>% 
  summarize(zohran = sum(votes[candidate == "Zohran Kwame Mamdani"], na.rm=T),
            cuomo = sum(votes[candidate == "Andrew M. Cuomo"], na.rm=T),
            total = sum(votes, na.rm=T))


votes_ed_21 <- fromJSON("2021_eds.json") %>% select(districtid, adams1, total1)
