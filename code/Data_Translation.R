library(tidyverse)
library(haven)
library(dplyr)

#Data frame to show the number of people (household individuals) per area:

num_people <- read_dta("sec1.dta")

num_people <- num_people %>% 
  group_by(clust) %>% 
  summarise(count = n())

#Gather education to get average years of education by cluster number. Helps to show which areas have more educated individuals. 

education <- read_dta("sec2a.dta")

education <- education %>%
  select(clust,s2aq2) %>%
  group_by(clust) %>%
  summarise(avg_educ = mean(s2aq2,na.rm = TRUE))


#Group agricultural equipment value if sold into cluster number to get the total value of agricultural equipment currently in each area.

ag_equip <- read_dta("sec8a3.dta")

ag_equip <- ag_equip %>% 
  select(clust,s8aq35) %>% 
  mutate(land_no_na = ifelse(is.na(s8aq35), 0, s8aq35)) %>% 
  group_by(clust) %>% 
  summarise(total_eq_val = sum(land_no_na))


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
  summarise(total_land = sum(land_acres)) 

# Group total value of all harvest by area number to understand which areas have the highest yield based on value. 

harvest_value <- read_dta("sec8c1.dta")

harvest_value <- harvest_value %>% 
  select(clust,s8cq13) %>% 
  group_by(clust) %>% 
  summarise(harvest_value = sum(s8cq13, na.rm = TRUE)) 

# Type of land used



# Agricultural training

training <- read_dta("sec2b.dta")

training <- training %>% 
  mutate(total_training = s2bq2 + s2bq6) %>% 
  mutate(total_training = ifelse(is.na(total_training), 0, total_training))


training <- training %>% 
  select(clust,total_training) %>% 
  group_by(clust) %>% 
  summarise(total_training = sum(total_training))



newdf2 <- harvest_value %>% 
  full_join(total_land) %>% 
  full_join(ag_equip) %>% 
  full_join(education) %>% 
  full_join(num_people)

newdf2 <- newdf2 %>% 
  mutate(val_per_acre = harvest_value/total_land)

newdf2 <- newdf2 %>% 
  full_join(training)

model1 <- lm(formula = harvest_value ~ total_land + total_eq_val + count + total_training, data = newdf2)
summary(model1)


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

newdf2 <- newdf2 %>% 
  full_join(grouped_total_income) %>% 
  rename("hh_mem_count" = count)

model1 <- lm(formula = total_income ~ total_land + total_eq_val + count + total_training, data = newdf2)
summary(model1)


credits <- read_dta("sec12a2.dta") %>% 
  mutate(total_debt = s12aq6 - s12aq9) %>% 
  mutate(total_debt = ifelse(is.na(total_debt), 0, total_debt))

credits <- credits %>% 
  select(clust,total_debt) %>% 
  group_by(clust) %>% 
  summarise(debt = sum(total_debt))

savings <- read_dta("sec12c.dta")

savings <- savings %>% 
  select(clust,s12cq4) %>% 
  group_by(clust) %>% 
  summarise(total_savings = sum(s12cq4))

newdf2 <- newdf2 %>% 
  full_join(credits) %>% 
  full_join(savings) %>% 
  mutate(debt_to_savings_ratio = debt/total_savings)



# Variables to add:

# Group distance from drinking water by cluster - need to convert the unit of measures based on the table in the google doc.

water <- read_dta("sec7.dta")


expenditures <- read_dta("exp4.dta")

expenditures <- expenditures %>% 
  select(clust,cropexp) %>% 
  group_by(clust) %>% 
  summarise(total_exp = sum(cropexp))

newdf2 <- newdf2 %>% 
  full_join(expenditures) %>% 
  mutate(profit = total_income - total_exp)

model1 <- lm(formula = profit ~ total_land + total_eq_val + hh_mem_count + I(debt_to_savings_ratio * 100) + total_exp, data = newdf2)
summary(model1)

# Change the ever been vaccinated field to a zero if it's a 2. Group and get a rate of vaccination (general) for each cluster. 

health <- read_dta("sec3b.dta")

# Group to get pre-natal care spending per cluster. 

preg <- read_dta("sec3d.dta")