### Created on: 26.04.04 ###
### Last edited: 26.04.20 ###

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
library(patchwork)
library(ggthemes)

## Load data ##
df_analysis <-
  read_rds("df_analysis.rds") |>
  mutate(conv_id = row_number())

# Check the variables
LUCIDUserfacinghistory <- df_analysis |> pull(LUCIDUserFacingHistory)

# Create a table in long format for each round of conversations
conversation_table <- map2_dfr(
  df_analysis$LUCIDUserFacingHistory,
  seq_len(nrow(df_analysis)),
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

# Join df_analysis tilbage på, og lav interaktionsvariable
conversation_table_joined <-
  conversation_table |>
  left_join(df_analysis, by = "conv_id")


#### HYPOTHESIS 2 ####

# Index on regression granularity
max_turn <-
  conversation_table_joined |>
  group_by(conv_id) |>
  slice_max(turn_order, n = 1) |>
  ungroup() |>
  select(conv_id, turn_order) |>
  rename(max_turn = turn_order)

df_analysis_hyp_2 <-
  df_analysis |>
  left_join(max_turn)

# index creation
df_analysis_hyp_2 <-
  df_analysis_hyp_2 |>
  mutate( # add variable, that countrs # units
    n_chars = nchar(
      str_remove(
        as.character(LUCIDUserFacingHistory),
        "^\\[assistant\\]:.*?(?=\\[user\\]:)"
      )
    )
  ) |>
    mutate(conv_time_s = as.numeric(as.character(LUCIDTotalConvTimeMs)) / 1000) |> # Turn time with bot into seconds
    mutate( # Create index for interactivity
      log_rounds = log1p(max_turn),
      log_time   = log1p(conv_time_s),
      z_rounds = as.numeric(scale(log_rounds)),
      z_time   = as.numeric(scale(log_time)),
      interaction_index = (z_rounds + z_time) / 2
    )


# Look at learning distribution for the amount of turns used
conversation_table_joined |>
  group_by(conv_id) |>
  slice_max(turn_order, n = 1) |>
  ungroup() |>
  ggplot(aes(x = turn_order, y = læring_total)) +
  geom_point() +
  theme_minimal()

# Look at learning distribution for interaction index
df_analysis_hyp_2 |>
  ggplot(aes(x = interaction_index, y = læring_total)) +
  geom_point() +
  theme_minimal()

# No comparison what so ever

#### Classifications ####
df_analysis_hyp_2 <-
  df_analysis_hyp_2 |>
  mutate(turn_dummy = if_else(max_turn > 3, "elaborative", "no elaboration"))


# Lets check learning based on turn_dummy
reg_turn_dummy <- lm(læring_total ~ turn_dummy, data = df_analysis_hyp_2)
summary(reg_turn_dummy)


#### DESCRIPTIVES ON INTERACTIONS ####

# Grab the average number of turns
average_turn_num <-
  conversation_table_joined |>
  group_by(conv_id) |>
  slice_max(turn_order, n = 1) |>
  ungroup() |>
  summarise(average_turn_num = mean(turn_order, na.rm = TRUE))

# Grab the average length of conversations
average_conv_length <-
  df_analysis_hyp_2 |>
  summarise(averabe_conv_length = mean(conv_time_s, na.rm = TRUE))

# Grab the average number of characters for each user - opening message from Polibob
average_chars_num <-
  df_analysis_hyp_2 |>
  filter(treatment == "chat bot") |>
  summarise(average_chars_num = mean(n_chars, na.rm = TRUE))


## Histograms ##

# bar
p1 <-
  conversation_table_joined |>
  group_by(conv_id) |>
  slice_max(turn_order, n = 1) |>
  ungroup() |>
  ggplot(aes(x = turn_order)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(3, 14, by = 2)) +
#  scale_y_continuous(limits = c(0, 13), breaks = seq(0, 13, by = 4)) + # Change the y-axis truncation here, when answers come in
  theme_tufte() +
  labs(x = "Antal interaktioner med chat bot", y = "Frekvens") +
  theme(
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.ticks.x = element_blank(),
        title = element_text(hjust = 0.5)
  )

p2 <-
  df_analysis_hyp_2 |>
  ggplot(aes(x = conv_time_s)) +
  geom_histogram(bins = 30, binwidth = 40) +
#  scale_y_continuous(limits = c(0, 13), breaks = seq(0, 13, by = 4)) + # Change the y-axis truncation here, when answers come in
  theme_tufte() +
  labs(x = "Sekunder brugt med chat bot", y = "Frekvens") +
  theme(
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        title = element_text(hjust = 0.5)
  )


p3 <-
  df_analysis_hyp_2 |>
  filter(treatment == "chat bot") |>
  ggplot(aes(x = n_chars)) +
  geom_histogram(bins = 30, binwidth = 500) +
#  scale_y_continuous(limits = c(0, 13), breaks = seq(0, 13, by = 4)) + # Change the y-axis truncation here, when answers come in
  theme_tufte() +
  labs(x = "Antal anslag i samtalen", y = "Frekvens") +
  theme(
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        title = element_text(hjust = 0.5)
  )


combined_histograms <-
  p1 + p2 + p3
