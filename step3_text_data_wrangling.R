### Created on: 26.04.04 ###
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
library(patchwork)
library(ggthemes)
library(officer)

## Load data ##
df_analysis <-
  read_rds("df_analysis.rds") |>
  mutate(conv_id = row_number())


# Define the df with the failed interactions filtered and manipulation check
df_failed <-
  df_analysis |>
#  filter(Q8_1 == 0 | is.na(Q8_1)) |>
#  filter(partier_folketing == "179")
  filter(Progress > 75)

# Define a df where cutoff is introduced
df_cutoff_filtered <-
  df_failed |>
  filter((treatment == "chat bot" & after_cutoff == "after" | treatment == "artikel")) |>
  mutate(conv_id = row_number())

# Set df for the entire regression results viz section here
# df <- df_analysis
df <- df_cutoff_filtered
# df <- df_failed

## EXPORT TO READ ##
df_export <- df |>
  distinct(conv_id, LUCIDUserFacingHistory) |>
  mutate(
    conversation_clean = LUCIDUserFacingHistory |>
      str_replace_all("\\[assistant\\]:", "\n\nASSISTANT:\n") |>
      str_replace_all("\\[user\\]:", "\n\nUSER:\n")
  )

doc <- read_docx()

for (i in seq_len(nrow(df_export))) {
  doc <- doc |>
    body_add_par(paste0("Conversation ID: ", df_export$conv_id[i]), style = "heading 1") |>
    body_add_par(df_export$conversation_clean[i], style = "Normal") |>
    body_add_break()
}

print(doc, target = "conversations_readable.docx")


# Check the variables
LUCIDUserfacinghistory <- df |> pull(LUCIDUserFacingHistory)

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


#### HYPOTHESIS 2 ####

# Index on regression granularity
max_turn <-
  conversation_table_joined |>
  group_by(conv_id) |>
  slice_max(turn_order, n = 1) |>
  ungroup() |>
  select(conv_id, turn_order) |>
  rename(max_turn = turn_order)

df_hyp_2 <-
  df |>
  left_join(max_turn)

# index creation
df_hyp_2 <-
  df_hyp_2 |>
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


### Interaction index regression ###
summary(lm(læring_total ~ interaction_index, data = df_hyp_2))

### Interaction index regression pre ###
summary(lm(læring_total ~ interaction_index + pre_afstand_total, data = df_hyp_2))

### Interaction index regression with controls
summary(lm(læring_total ~ interaction_index + pre_afstand_total + Tillid + subjektiv_forståelse, data = df_hyp_2))

### Interaction index regression with controls and n_char control
summary(lm(læring_total ~ interaction_index + n_chars + pre_afstand_total + Tillid + subjektiv_forståelse, data = df_hyp_2))

# Robustness - look at n_chars inside the index
summary(lm(læring_total ~ interaction_index_chars + n_chars + pre_afstand_total + Tillid + subjektiv_forståelse, data = df_hyp_2))

# Plot the main reg
pred <- ggpredict(
  lm(læring_total ~ interaction_index + pre_afstand_total + Tillid + subjektiv_forståelse, data = df_hyp_2),
  terms = "interaction_index"
)

ggplot(pred, aes(x = x, y = predicted)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  geom_hline(yintercept = 0) +
  labs(
    x = "Chat bot interaktion",
    y = "Forudsagt læring"
  ) +
  theme_simon(base_size = 14) +
  scale_y_continuous(limits = c(-0.8, 0.8)) +
  scale_x_continuous(limits = c(-3, 1))


# Visualize the interaction

# Plot 1
m_interaction <- lm(
  læring_total ~ z_chars * interaction_index +
    pre_afstand_total + Tillid + subjektiv_forståelse,
  data = df_hyp_2
)

pred_interaction <- ggpredict(
  m_interaction,
  terms = c("interaction_index", "z_chars [-1, 0, 1]")
)

ggplot(pred_interaction,
       aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.05, color = NA) +
  geom_hline(yintercept = 0, alpha = 0.7) +
  labs(
    x = "Chat bot interaktion",
    y = "Forudsagt læring",
    color = "Tekstmængde",
    fill = "Tekstmængde"
  ) +
  scale_color_discrete(
    labels = c("Lav tekstmængde (-1 SD)", "Gennemsnitlig tekstmængde", "Høj tekstmængde (+1 SD)")
  ) +
  scale_fill_discrete(
    labels = c("Lav tekstmængde (-1 SD)", "Gennemsnitlig tekstmængde", "Høj tekstmængde (+1 SD)")
  ) +
  theme_simon(base_size = 14)

# Plot 2
m_interaction <- lm(
  læring_total ~ z_chars * interaction_index +
    pre_afstand_total + Tillid + subjektiv_forståelse,
  data = df_hyp_2
)

pred_interaction <- ggpredict(
  m_interaction,
  terms = c("z_chars", "interaction_index [-1, 0, 1]")
)

ggplot(pred_interaction,
       aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.15, color = NA) +
  geom_hline(yintercept = 0, alpha = 0.7) +
  labs(
    x = "Tekstmængde (standardiseret)",
    y = "Forudsagt læring",
    color = "Interaktion",
    fill = "Interaktion"
  ) +
  scale_color_discrete(
    labels = c("Lav interaktion (-1 SD)", "Gennemsnitlig interaktion", "Høj interaktion (+1 SD)")
  ) +
  scale_fill_discrete(
    labels = c("Lav interaktion (-1 SD)", "Gennemsnitlig interaktion", "Høj interaktion (+1 SD)")
  ) +
  theme_simon(base_size = 14)

# How do the behavorial items interact? #


### Let's look at items z transformed
summary(lm(læring_total ~ z_chars + z_rounds + z_time, data = df_hyp_2))

# z_chars looks like more chars - more learning

### Interaction - z_chars with the attention index and controls
summary(lm(læring_total ~ z_chars * interaction_index + pre_afstand_total + Tillid + subjektiv_forståelse, data = df_hyp_2))

# Visualize the interaction






#### Classifications ####


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
  df_hyp_2 |>
  summarise(averabe_conv_length = mean(conv_time_s, na.rm = TRUE))

# Grab the average number of characters for each user - opening message from Polibob
average_chars_num <-
  df_hyp_2 |>
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
  df_hyp_2 |>
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
  df_hyp_2 |>
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




### Line chart - what happened to chat bot interactions after prompt fix? ###
means_df_3 <-
  df_hyp_2 |>
  group_by(source_prompt) |>
  summarise(
    mean_learning = mean(interaction_index, na.rm = TRUE),
    .groups = "drop"
  )

df_hyp_2 |> # Needs to be the df version with both pre and post cutoff
  ggplot(aes(x = StartDate_cph, y = interaction_index)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_hline(
    data = means_df_3,
    aes(yintercept = mean_learning),
    linewidth = 1, linetype = "longdash"
  ) +
  facet_wrap(~source_prompt, scales = "free_x",
             labeller = labeller(source_prompt = label_wrap_gen(width = 20))) +
  labs(y = "Interaktionsmængde (Normaliseret)", x = "Tidspunkt / Dato", title = "Den gennemsnitlige chat bot interaktionsmængde,
       fordelt over de tre skelsættende indsamlingsperioder i projektet") +
  theme_simon(base_size = 14) +
  theme(panel.spacing = unit(1, "cm"),
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.ticks.length = unit(2.5, "pt")) +
  scale_x_datetime(
    breaks = scales::breaks_pretty(n = 4),
    date_labels = "%d-%m\n%H:%M",
    guide = guide_axis(check.overlap = TRUE)
  )


