### Created on: 26.04.04 ###
### Last edited: 26.04.23 ###

# Setup
library(dplyr)
library(readr)
library(ggplot2)
library(forcats)
library(purrr)
library(stringr)
library(haven)
library(labelled)

# Load data
df <-
  read_sav("260423_data.sav") |>
  mutate(
    across(where(is.character), as_factor), # All string vars to factors
    across(-all_of(c("Q9", "Q7", "Q48")) & where(is.labelled), haven::zap_labels), # Zap labels for numeric vars
    across(all_of(c("Q9", "Q7", "Q48")), as_factor) # keep these two as factors
  )


# Recode the df
df_recoded <-
  df |>
  set_variable_labels( # Fix the labels of the pre-measurement
    Q41_1 = "Pre-måling – Dansk Folkeparti",
    Q41_2 = "Pre-måling – Liberal Alliance",
    Q41_3 = "Pre-måling – Socialistisk Folkeparti",
    Q41_4 = "Pre-måling – Radikale Venstre",
    Q41_5 = "Pre-måling – Enhedslisten",
    Q41_6 = "Pre-måling – Venstre"
  ) |>
  set_variable_labels( # Fix labels of the post measurement
    Q43_1 = "Post-måling – Dansk Folkeparti",
    Q43_2 = "Post-måling – Liberal Alliance",
    Q43_3 = "Post-måling – Socialistisk Folkeparti",
    Q43_4 = "Post-måling – Radikale Venstre",
    Q43_5 = "Post-måling – Enhedslisten",
    Q43_6 = "Post-måling – Venstre"
  )


# Export as rds
df_recoded |>
  saveRDS("df_recoded.rds")




