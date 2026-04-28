### Created on: 26.04.04 ###
### Last edited: 26.04.24 ###

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
      log_chars = log1p(n_chars),
      z_chars  = as.numeric(scale(log_chars)),
      z_rounds = as.numeric(scale(log_rounds)),
      z_time   = as.numeric(scale(log_time)),
      interaction_index = (z_rounds + z_time) / 2,
      interaction_index_chars = (z_chars + z_rounds + z_time) / 3
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

# Fit a reg line
df_analysis_hyp_2 |>
  ggplot(aes(x = interaction_index, y = læring_total)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

### Interaction index regression ###
summary(lm(læring_total ~ interaction_index_chars, data = df_analysis_hyp_2 |> filter(after_cutoff == "after")))

### Interaction index regression ###
summary(lm(læring_total ~ interaction_index_chars + pre_afstand_total, data = df_analysis_hyp_2 |> filter(after_cutoff == "after")))

### Let's look at nchar ###
summary(lm(læring_total ~ n_chars + max_turn + conv_time_s, data = df_analysis_hyp_2 |> filter(after_cutoff == "after")))

### Let's look at them z transformed
summary(lm(læring_total ~ z_chars + z_rounds + z_time, data = df_analysis_hyp_2 |> filter(after_cutoff == "after")))

### Let's look at z_chars
summary(lm(læring_total ~ z_chars, data = df_analysis_hyp_2 |> filter(after_cutoff == "after")))

### Interaction - z_chars only works with the attention index
summary(lm(læring_total ~ z_chars * interaction_index + pre_afstand_total, data = df_analysis_hyp_2 |> filter(after_cutoff == "after")))

### Double interaction
summary(lm(læring_total ~ z_chars * z_time * z_rounds + pre_afstand_total, data = df_analysis_hyp_2 |> filter(after_cutoff == "after")))

### Trust added
summary(lm(læring_total ~ interaction_index_chars + pre_afstand_total + Tillid, data = df_analysis_hyp_2 |> filter(after_cutoff == "after")))

### Interaction - z_chars only works with the attention index
summary(lm(læring_total ~ z_chars * interaction_index + pre_afstand_total + Tillid, data = df_analysis_hyp_2 |> filter(after_cutoff == "after")))

### Subjective understanding added
summary(lm(læring_total ~ interaction_index_chars + pre_afstand_total + Tillid + subjektiv_forståelse, data = df_analysis_hyp_2 |> filter(after_cutoff == "after")))

### Interaction - z_chars only works with the attention index
summary(lm(læring_total ~ z_chars * interaction_index + pre_afstand_total + Tillid + subjektiv_forståelse, data = df_analysis_hyp_2 |> filter(after_cutoff == "after")))

### Pre-knowledge relationship with interaction index
summary(lm(interaction_index ~ pre_afstand_total, data = df_analysis_hyp_2 |> filter(after_cutoff == "after")))









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
  scale_x_continuous(breaks = seq(1, 30, by = 2)) +
#  scale_y_continuous(limits = c(0, 13), breaks = seq(0, 13, by = 4)) + # Change the y-axis truncation here, when answers come in
  theme_tufte() +
  labs(x = "Antal interaktioner med chat bot", y = "Frekvens (absolutte tal)") +
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


# The average interaction
mean_num_interactions <-
  conversation_table_joined



# Line chart - what happened to chat bot interactions after prompt fix?
df_failed |> # Needs to be the df version with both pre and post cutoff
  ggplot(aes(x = StartDate_cph, y = læring_total, group = treatment)) +
  #  geom_line(aes(color = treatment), linewidth = 0.8, alpha = 0.3) +
  geom_point(aes(color = treatment, shape = treatment), size = 2, alpha = 0.5) +
  #  geom_hline(yintercept = 0, linetype = "solid", linewidth = 1, alpha = 0.6) +
  geom_hline(
    data = means_df_2,
    aes(yintercept = mean_learning, color = treatment),
    linewidth = 1, linetype = "longdash"
  ) +
  facet_wrap(~source_prompt, scales = "free_x",
             labeller = labeller(source_prompt = label_wrap_gen(width = 20))) +
  labs(y = "Læring", x = "Tidspunkt / Dato", title = "Den gennemsnitlige læring for de to treatmenttyper,
       fordelt over de tre skelsættende indsamlingsperioder i projektet",
       caption = "Note: De stiplede linjer viser gennemsnittet af læring for perioden inden for hvert treatment.") +
  theme_simon(base_size = 14) +
  scale_color_manual(values = c(
    "chat bot" = "#000000",
    "artikel"  = "#A3A3A3"
  )) +
  theme(panel.spacing = unit(1, "cm"),
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.ticks.length = unit(2.5, "pt")) +
  scale_x_datetime(
    breaks = scales::breaks_pretty(n = 4),
    date_labels = "%d-%m\n%H:%M",
    guide = guide_axis(check.overlap = TRUE)
  )

