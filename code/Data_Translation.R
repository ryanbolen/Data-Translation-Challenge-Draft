library(tidyverse)
library(haven)
library(dplyr)

#Data frame to show the number of people (household individuals) per area:

sec1 <- read_dta("sec1.dta")

num_people <- sec1 %>% 
  group_by(clust) %>% 
  summarise(count = n())

#Gather education to get average years of education by cluster number. Helps to show which areas have more educated individuals. 

education <- read_dta("sec2a.dta")

education <- dfsec2a %>%
  select(clust,s2aq2) %>%
  group_by(clust) %>%
  summarise(avg_educ = mean(s2aq2,na.rm = TRUE)) %>% 
  rename("area" = clust)

ggplot(data = gathered_education) + 
  geom_smooth(mapping = aes(x = clust, y = avg_educ))

#Group agricultural equipment value if sold into cluster number to get the total value of agricultural equipment currently in each area.

ag_equip <- read_dta("sec8a3.dta")

ag_equip <- ag_equip %>% 
  select(clust,s8aq35) %>% 
  mutate(land_no_na = ifelse(is.na(s8aq35), 0, s8aq35)) %>% 
  group_by(clust) %>% 
  summarise(total_eq_val = sum(land_no_na)) %>% 
  rename("area" = clust)


#Group total land by enumeration area to get the amount of land available in each area. 

dfsec8b <- read_dta("sec8b.dta")



# 1 pole  = 0.25 acres
# 1 rope (online it says square rod) = 0.0625 acres
# Drop observations where unit of measure = 4/other so we can convert everything to acres. 

dfsec8a1 <- read_dta("sec8a1.dta")

land_stats_by_area <- dfsec8a1 %>% 
  select(clust,s8aq4,s8aq16) %>% 
  group_by(clust) %>% 
  summarise(total_land = sum(s8aq4, na.rm = TRUE), total_share_crop = sum(s8aq16, na.rm = TRUE)) %>% 
  mutate(perc_share_crop = total_share_crop/total_land) %>% 
  mutate(perc_share_crop = ifelse(is.na(perc_share_crop), 0, perc_share_crop))

#Total land converted to acres (unit of measure = 4/other observations dropped)

land <- read_dta("sec8b.dta")

land <- land %>% 
  select(clust,s8bq4b,s8bq4a)

acres <- land  %>% 
  filter(s8bq4b == 1) %>% 
  mutate(land_acres = s8bq4a * 1) %>% 
  select(clust,land_acres)

poles <- land  %>% 
  filter(s8bq4b == 2) %>% 
  mutate(land_acres = s8bq4a/4) %>% 
  select(clust,land_acres)

ropes <- land  %>% 
  filter(s8bq4b == 3) %>% 
  mutate(land_acres = s8bq4a * 0.0625) %>% 
  select(clust,land_acres)


total_land <- bind_rows(acres,poles,ropes) %>% 
  group_by(clust) %>% 
  summarise(total_land = sum(land_acres)) %>% 
  rename("area" = clust)

# Group total value of all harvest by area number to understand which areas have the highest yield based on value. 

dfsec8c1 <- read_dta("sec8c1.dta")

sales <- dfsec8c1 %>% 
  select(clust,s8cq13) %>% 
  group_by(clust) %>% 
  summarise(harvest_value = sum(s8cq13, na.rm = TRUE)) %>% 
  rename("area" = clust)


newdf2 <- sales %>% 
  full_join(total_land) %>% 
  full_join(ag_equip) %>% 
  left_join(gathered_education)
  


dfinc10 <- read_dta("inc10.dta")
dfinc11 <- read_dta("inc11.dta")

#Get these 3 tables joined to get total agricultural revenue (cash crops main, cash crops other, roots/fruits & other ag-revenue)

dfsubagg13 <- read_dta("subagg13.dta")
dfsubagg14 <- read_dta("subagg14.dta")
dfsubagg15 <- read_dta("subagg15.dta")

joined_incomes <- dfsubagg13 %>% 
  inner_join(dfsubagg14) %>% 
  inner_join(dfsubagg15) %>% 
  mutate(total_ag_inc = crpinc1 + crpinc2 + rootinc + incothag)

grouped_total_income <- joined_incomes %>% 
  select(clust,total_ag_inc) %>% 
  group_by(clust) %>% 
  summarise(total_income = sum(total_ag_inc))

new_df <- grouped_total_income %>% 
  inner_join(total_land) %>% 
  mutate(inc_per_acre = total_income/total_land) %>% 
  inner_join(gathered_ag_equip)





