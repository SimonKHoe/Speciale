### Created on: 26.04.04 ###
### Last edited: 26.04.13 ###

# Setup
library(dplyr)
library(readr)
library(ggplot2)
library(forcats)
library(purrr)
library(stringr)
library(haven)
library(labelled)
library(tidyr)

## Load data ##

# CHES
chapel_hill <-
  read_csv("1999-2024_CHES_dataset_means.csv") |>
  filter(country == 2) |> # Select DK
  filter(year == 2024) |> # Filter to 2024, as it is only there in 2024
  mutate(climate_env = (climate_change + environment) / 2) |>  # Divide it by 2 to normalize it back towards 1-10
  mutate(climate_env_salience = (climate_change_salience + enviro_salience) / 2) |>
  mutate(eu_1_10 = 1 + (eu_position - 1) * (9/6)) |>
  select(party, eu_1_10) |> # Select relevant topic here and pivot before join
  pivot_wider(
    names_from = party,
    values_from = eu_1_10 # Change the topic we wish to look at here
  )

# Qualtrics df
df_recoded <-
  readRDS("df_recoded.rds")


# Merge CHES placements onto Qualtrics df
df_merged <-
  df_recoded |>
  cbind(chapel_hill) # Cbind så vi får lagt ekspertplaceringerne på

# Export the merged df
df_merged |>
  saveRDS("df_merged.rds")

