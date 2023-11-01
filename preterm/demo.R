library(tidyverse)
library(here)

data <- here("preterm",
             "messy_data.csv") |>
  read_csv() |>
  pivot_longer(cols = -ZIP,
               names_to = "year") |>
  mutate(age = substr(value, 6, 9)) |>
  mutate(income = str_sub(value, 21)) |>
  mutate(income = as.numeric(income)) |>
  mutate(age = as.numeric(age)) |>
  filter(!is.na(income)) |>
  select(-value) |>
  mutate(year = as.numeric(year))

write_csv(data, here("preterm", "clean_data.csv"))


data$year_char <- as.character(data$year)

data <- data |>
  mutate(year_num = as.numeric(year_char))

ggplot(data,
       aes(x = year,
           y = income)) +
  geom_point()


data_few_zips <- data |>
  filter(ZIP == "17774" | 
         ZIP == "02145" |
         ZIP == "84604")


ggplot(data_few_zips) +
  geom_line(aes(x = year,
                 y = income,
                 color = ZIP))

data_summary <- data |>
  group_by(ZIP) |>
  summarise(avg_inc = mean(income, na.rm = TRUE),
            avg_age = mean(age, na.rm = TRUE))
