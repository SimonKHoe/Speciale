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
library(tidyr)
library(lubridate)

## Load data ##
df_merged <-
  read_rds("df_merged.rds")

# Define the cutoff date for the prompt fix
cutoff <- as.POSIXct("2026-04-13 17:00:00", tz = "Europe/Copenhagen") # This is the cutoff around fixing the prompt
facebook <- as.POSIXct("2026-04-22 08:00:00", tz = "Europe/Copenhagen") # This is when I started collecting on FB

# Create the distance calculations
df_analysis <-
  df_merged |>
  filter(Status == 0) |> # Keep only non-preview
  mutate( # Calculate the learning for each party
         # DF
         pre_afstand_DF = abs(DF - Q41_1), # Peoples distance from expert placement, pre
         post_afstand_DF = abs(DF - Q43_1), # Peoples distance from expert placement, post
         læring_DF = pre_afstand_DF - post_afstand_DF, # The difference in differences for pre-post is the learning
         # LA
         pre_afstand_LA  = abs(LA - Q41_2),
         post_afstand_LA = abs(LA - Q43_2),
         læring_LA       = pre_afstand_LA - post_afstand_LA,
         # SF
         pre_afstand_SF  = abs(SF - Q41_3),
         post_afstand_SF = abs(SF - Q43_3),
         læring_SF       = pre_afstand_SF - post_afstand_SF,
         # RV
         pre_afstand_RV  = abs(RV - Q41_4),
         post_afstand_RV = abs(RV - Q43_4),
         læring_RV       = pre_afstand_RV - post_afstand_RV,
         # EL
         pre_afstand_EL  = abs(EL - Q41_5),  # distance pre
         post_afstand_EL = abs(EL - Q43_5),  # distance post
         læring_EL       = pre_afstand_EL - post_afstand_EL,  # learning (improvement)
         # V
         pre_afstand_V  = abs(V - Q41_6),
         post_afstand_V = abs(V - Q43_6),
         læring_V       = pre_afstand_V - post_afstand_V
         ) |>
  mutate( # Average of learning
    læring_total = rowMeans(
      pick(læring_DF, læring_LA, læring_SF, læring_RV, læring_EL, læring_V),
      na.rm = TRUE
    )
  ) |>
  mutate( # Average of pre-placement distance
    pre_afstand_total = rowMeans(
    pick(pre_afstand_DF, pre_afstand_LA, pre_afstand_SF, pre_afstand_RV, pre_afstand_EL, pre_afstand_V)
    )
  ) |>
  mutate( # Average of post-placement distance
    post_afstand_total = rowMeans(
      pick(post_afstand_DF, post_afstand_LA, post_afstand_SF, post_afstand_RV, post_afstand_EL, post_afstand_V),
      na.rm = TRUE
    ),
    post_viden = -post_afstand_total
  ) |>
  mutate( # z transformer post_viden og subjektiv_forståelse
    z_post_viden = as.numeric(scale(post_viden)),
    z_subjektiv_forståelse = as.numeric(scale(Q11))
  ) |>
  rename('Tillid' = 'Q12') |> # Rename "Tillid" for downstream clarity
  mutate(
    StartDate_cph = with_tz(StartDate, "Europe/Copenhagen"), # Convert the time stamp to Copenhagen
    after_cutoff = if_else(StartDate_cph > cutoff, "after", "before"), # This creates the dummy that marks the change in the prompt
    after_cutoff = as_factor(after_cutoff) # Convert it to factor
  ) |>
  mutate(facebook_dummy = if_else(StartDate_cph > facebook, "post-fb", "pre-fb")) |>
  rename('subjektiv_forståelse' = 'Q11') |>
  rename('partier_folketing' = 'Q48') |>
  mutate(source = if_else(source == "", "personal", source) # using if else for recode is not BP, but necessary to address blank spaces
  ) |>
  mutate(source = case_when( # Give source more clear names for downstream plotting
                            source == "fb" ~ "Personligt Facebook opslag",
                            source == "fb2" ~ "Facebook Gruppe opslag",
                            source == "li" ~ "Personlige LinkedIn opslag",
                            source == "personal" ~ "Distribution på besked"
                            )
         ) |>
  mutate(source_prompt = case_when(
                                   StartDate_cph < cutoff ~ "Før fix af prompt - personlige opslag",
                                   StartDate_cph > cutoff & StartDate_cph < facebook ~ "Efter fix af prompt - personlige opslag",
                                   StartDate_cph > facebook ~ "Efter fix - Facebook grupper"
                                   ),
         source_prompt = as_factor(source_prompt), # Turn into factor for downstream plotting
         source_prompt = fct_relevel(source_prompt, # relevel the factor categories for downstream plotting
           "Før fix af prompt - personlige opslag",
           "Efter fix af prompt - personlige opslag",
           "Efter fix - Facebook grupper"
         )
         )


# Export the analysis ready df
df_analysis |>
  saveRDS("df_analysis.rds")
