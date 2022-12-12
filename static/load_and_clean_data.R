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

## change marriage status into 2 stages: married:1 unmarried:0
wage_data_clean2$MARST <- with(wage_data_clean2, ifelse(MARST == 2, 1,
                                                 ifelse(MARST == 1, 1, 0)))

## add new col of SPEAKENG: 0: doesn't speak any; 1: speak well; 2: a little
wage_data_clean2$SPEAKENG_new <- cut(wage_data_clean2$SPEAKENG,
                                     breaks = c(0, 1, 2, 5, Inf),
                                     labels = c(0, 1, 1, 2))

## add new col of EDUC: 0: combined highschool into one
library(dplyr)
wage_data_clean2 <- wage_data_clean2 %>%
  mutate(EDUC_new = case_when(EDUC == 1 ~ 1,
                              EDUC == 2 ~ 2,
                              EDUC == 3 ~ 3,
                              EDUC %in% c(4, 5, 6) ~ 3,
                              EDUC == 7 ~ 4,
                              EDUC == 8 ~ 5,
                              EDUC == 9 ~ 6,
                              EDUC == 10 ~ 7,
                                  TRUE ~ 8))


##Combine two data frames
clean_wage <- merge(x = wage_data_clean2, y = state_data, by = "STATEFIP")

write_csv(clean_wage, file = here::here("dataset-ignore", "clean_wage.csv"))
save(clean_wage, file = here::here("dataset-ignore/clean_wage.RData"))
