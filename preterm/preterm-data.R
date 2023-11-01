library(tidycensus)
library(tidyverse)
library(here)

vars <- c(med_age_ = "B01002_001",
          med_inc_ = "B21004_001")

zips <- get_acs(geography = "zcta", 
                     variables = vars, year = 2011, 
                     output = "wide") |>
  mutate(year = 2011)

for (i in 2012:2021) {
  this_zips <- get_acs(geography = "zcta", 
                       variables = vars, year = i, 
                       output = "wide") |>
    mutate(year = i)
  
  zips <- rbind(zips, this_zips)
}

zips_messy <- zips |>
  mutate(age_income = paste0("Age: ", formatC(med_age_E, format = "f", digits = 1), " / Income: ", med_inc_E)) |>
  mutate(ZIP = substr(NAME, 7, 11)) |>
  select(ZIP, year, age_income) |>
  pivot_wider(names_from = year, values_from = age_income)

write_csv(zips_messy, here("preterm",
                           "messy_data.csv"))
