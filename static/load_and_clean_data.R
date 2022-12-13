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
wage_data_clean2$SPEAKENG <- cut(wage_data_clean2$SPEAKENG,
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
## remove unnecessary columns
library(dplyr)
wage_data_clean2 <- wage_data_clean2 %>%
  dplyr::select(-c(OWNERSHPD))%>%
  dplyr::select(-c(EDUCD))

##Combine two data frames
clean_wage <- merge(x = wage_data_clean2, y = state_data, by = "STATEFIP")


#creating the factor variables 
#EDUC.f
clean_wage$EDUC.f <- factor(clean_wage$EDUC)
is.factor(clean_wage$EDUC.f )
clean_wage$EDUC_new.f <- factor(clean_wage$EDUC_new)
is.factor(clean_wage$EDUC_new.f )

#SEX.f
clean_wage$SEX.f <- factor(clean_wage$SEX)
is.factor(clean_wage$SEX.f)

#MARST.f
clean_wage$MARST.f <- factor(clean_wage$MARST)
is.factor(clean_wage$MARST.f)

#SPEAKENG.f
clean_wage$SPEAKENG.f <- factor(clean_wage$SPEAKENG)
is.factor(clean_wage$SPEAKENG.f)

#STATE.f
clean_wage$STATE.f <- factor(clean_wage$STATE)
is.factor(clean_wage$STATE.f )

write_csv(wage_data_clean2, file = here::here("dataset", "wage_data_clean2.csv"))
save(wage_data_clean2, file = here::here("dataset/wage_data_clean2.RData"))

write_csv(state_data, file = here::here("dataset", "state_data.csv"))
save(state_data, file = here::here("dataset/state_data.RData"))

write_csv(clean_wage, file = here::here("dataset-ignore", "clean_wage.csv"))
save(clean_wage, file = here::here("dataset-ignore/clean_wage.RData"))
