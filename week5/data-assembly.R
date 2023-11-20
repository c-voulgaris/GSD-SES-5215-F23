library(tidyverse)
library(tidycensus)
library(tigris)
library(here)
library(sf)


## Weather data 
## from https://www.wunderground.com/history/monthly/us/ma/boston/KBOS/date/2020-2
feb_weather <- here("week5",
                    "feb-2020-weather.csv") |>
  read_csv()


cities <- places(state = "MA") |>
  st_transform("WGS84")

tract_hus <- get_decennial(geography = "tract", 
                           variables = "H1_001N", 
                           year = 2020,
                           state = "MA")

tracts <- tracts(state = "MA", year = 2020) |>
  select(-NAME) |>
  left_join(tract_hus) |>
  mutate(tract_hu_per_sq_km = 1000000*value/ALAND) |>
  select(tract_hu_per_sq_km) |>
  st_transform("WGS84")

## Original file as downloaded 
## from https://s3.amazonaws.com/hubway-data/202002-bluebikes-tripdata.zip
bike_data <- here("week5",
             "202002-bluebikes-tripdata.csv") |>
  read_csv() |>
  mutate(start_date = str_extract(starttime, '^([^ ]+)'),
         end_date = str_extract(stoptime, '^([^ ]+)')) |>
  mutate(start_day = as.numeric(str_extract(start_date, '(?<=02-)(.+)'))) |>
  mutate(day_of_week = case_when((start_day+7)/7 == round((start_day+7)/7) ~ 
                                     "Friday",
                                   (start_day+8)/7 == round((start_day+8)/7) ~ 
                                     "Thursday",
                                   (start_day+9)/7 == round((start_day+9)/7) ~ 
                                     "Wednesday",
                                   (start_day+10)/7 == round((start_day+10)/7) ~ 
                                     "Tuesday",
                                   (start_day+11)/7 == round((start_day+11)/7) ~ 
                                     "Monday",
                                   (start_day+12)/7 == round((start_day+12)/7) ~ 
                                     "Sunday",
                                   TRUE ~ "Saturday")) |>
  mutate(day_type = case_when(day_of_week == "Sunday" ~ "Weekend",
                              day_of_week == "Saturday" ~ "Weekend",
                              start_day == 17 ~ "Holiday",
                              TRUE ~ "Weekday")) |>
  mutate(start_time = str_extract(starttime, '(?<=\\s)(.+$)'),
         end_time = str_extract(stoptime, '(?<=\\s)(.+$)')) |>
  mutate(start_hour = as.numeric(str_extract(start_time, '^([^:]+)')),
         end_hour = as.numeric(str_extract(end_time, '^([^:]+)'))) |>
  mutate(start_time_period = case_when(start_hour < 5 ~ "night",
                                       start_hour < 10 ~ "morning",
                                       start_hour < 16 ~ "mid-day",
                                       start_hour < 19 ~ "early evening",
                                       start_hour < 21 ~ "late evening",
                                       TRUE ~ "night")) |>
  mutate(age = 2020 - `birth year`,
         gender = case_when(gender == 0 ~ "unspecified",
                            gender == 1 ~ "female",
                            TRUE ~ "male")) |>
  st_as_sf(coords = c("start station longitude", "start station latitude"),
           crs = "WGS84") |>
  st_join(cities) |>
  select(tripduration,
         usertype,
         age,
         gender,
         start_day,
         day_of_week,
         day_type,
         NAME) |>
  left_join(feb_weather) |>
  rename(city = NAME) |>
  st_join(tracts) |>
  filter(tract_hu_per_sq_km > 0) |>
  st_drop_geometry()

model <- lm(log(tripduration) ~ 
              usertype +
                  age +
                  gender +
                  city +
                  day_type +
            #  min_temp +
           #   total_precip +
                  log(tract_hu_per_sq_km),
                data = bike_data)

summary(model)

write_csv(bike_data, here("week5", "bike-data.csv"))

