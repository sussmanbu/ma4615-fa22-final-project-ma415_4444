install.packages("here")
library(here)
library(tidyverse)
library(dplyr)

##DF1
incomedata <- read_csv(here::here("dataset-ignore", "income.csv"))

## CLEAN the data
loan_data_clean <- loan_data

write_csv(loan_data_clean, file = here::here("dataset", "loan_refusal_clean.csv"))

save(loan_data_clean, file = here::here("dataset/loan_refusal.RData"))