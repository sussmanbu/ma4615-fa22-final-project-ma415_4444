library(tidyverse)

##DF1
wage_data <- read_csv(here::here("dataset-ignore", "wage.csv"))

##DF2
#CCR: completing college rate (after 25) 2016-2020
#MHI: Median Household Income (2020)
state_data <- read_csv(here::here("dataset", "states.csv"))

## CLEAN DF1
wage_data_clean1 <- 
  wage_data %>% 
  filter(INCWAGE < 999998)

summary(wage_data_clean1$INCWAGE)
### outlier q0.75+1.5IQR=45000+1.5(45000-0)=112500
wage_data_clean2 <- 
  wage_data_clean1 %>% filter(INCWAGE < 112500)

##Combine two data frames
clean_wage <- merge(x = wage_data_clean2, y = state_data, by = "STATEFIP")

write_csv(clean_wage, file = here::here("dataset-ignore", "clean_wage.csv"))
save(clean_wage, file = here::here("dataset-ignore/clean_wage.RData"))
