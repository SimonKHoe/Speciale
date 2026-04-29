### Created on: 26.04.29 ###
### Last edited: 26.04.29 ###

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
library(dotwhisker)
library(ggeffects)
library(ggthemes)
library(AER)

#### Load data ####
df_analysis <-
  read_rds("df_analysis.rds") |>
  mutate(conv_id = row_number())

# Define the df with the failed interactions filtered and manipulation check
df_failed <- # THis becomes ITT
  df_analysis |>
  #  filter(Q8_1 == 0 | is.na(Q8_1)) |>
  #  filter(partier_folketing == "179") |>
  filter(Progress > 75) # Remove people who haven't done post-placements

# Define a df where cutoff is introduced
df_cutoff_filtered <-
  df_failed |>
  filter((treatment == "chat bot" & after_cutoff == "after" | treatment == "artikel")) |>
  mutate(conv_id = row_number())

# THIS IS THE LATE DF
# df <- df_analysis
df <- df_cutoff_filtered
# df <- df_failed

# THIS DEFINES THE LATE DF

### THIS PULLS OUT MAX_TURNS FROM INTERACTIONS ### TODO: STREAMLINE
# Create a table in long format for each round of conversations
conversation_table <- map2_dfr(
  df$LUCIDUserFacingHistory,
  seq_len(nrow(df)),
  \(txt, id) {
    tibble(raw = txt, conv_id = id) %>%
      mutate(turns = str_split(
        raw,
        "\\s*(?=\\[(?:assistant|user)\\]:)",
        simplify = FALSE
      )) %>%
      unnest(turns) %>%
      filter(turns != "") %>%
      mutate(
        turn_order = row_number(),
        role = str_extract(turns, "(?<=\\[)(assistant|user)(?=\\]:)"),
        content = str_remove(turns, "^\\[(?:assistant|user)\\]:\\s*")
      ) %>%
      select(conv_id, turn_order, role, content)
  }
)

# Join df tilbage på, og lav interaktionsvariable
conversation_table_joined <-
  conversation_table |>
  left_join(df, by = "conv_id")

# Index on regression granularity
max_turn <-
  conversation_table_joined |>
  group_by(conv_id) |>
  slice_max(turn_order, n = 1) |>
  ungroup() |>
  select(conv_id, turn_order) |>
  rename(max_turn = turn_order)

df_late <- # Left joins max turns back onto OG df
  df |>
  left_join(max_turn) |>
  mutate(engaged_chatbot_dummy = if_else(!is.na(max_turn) & max_turn > 2, 1, 0)) |>  # Create the dummy on chat bot engagement
  mutate(treatment_dummy = if_else(treatment == "chat bot", 1, 0)) # Create numerical treatment dummy for easier IV interpretation


#### HYPOTHESIS 1 ####

# IV Regression
first_stage <- lm(engaged_chatbot_dummy ~ treatment_dummy + pre_afstand_total, data = df_late)
summary(first_stage)

iv_model <- ivreg(læring_total ~ engaged_chatbot_dummy + pre_afstand_total | treatment_dummy + pre_afstand_total, data = df_late)

summary(iv_model, diagnostics = TRUE)

