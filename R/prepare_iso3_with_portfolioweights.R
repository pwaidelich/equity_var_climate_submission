# clean environment
rm(list = ls())

library(tidyverse)
library(readxl)

# read in portfolio weights calculated via MSCI index data
df_portfolioweights <- read_excel(file.path("data", "Calibration.xlsx"),
                                  sheet = "Portfolio",
                                  n_max = 195) %>%
  # select columns of interest
  select(iso3, name, has_any_weight) %>%
  # subset to countries that are included in any portfolio
  filter(has_any_weight)

# export the list of ISO3 as a CSV file
df_portfolioweights %>% select(iso3, name) %>%
  write_csv(file.path("data", "output", "META_countries_with_marketcap.csv"))
