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
# conversation_table_joined |>
#   group_by(conv_id) |>
#   slice_max(turn_order, n = 1) |>
#   ungroup() |>
#   ggplot(aes(x = turn_order, y = læring_total)) +
#   geom_point() +
#   theme_minimal()

# Interaction X Learning #

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

# Plot the interaction index binary
pred <- ggpredict(
  lm(læring_total ~ interaction_index + pre_afstand_total + Tillid + subjektiv_forståelse, data = df_hyp_2),
  terms = "interaction_index"
)

ggplot(pred, aes(x = x, y = predicted)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  geom_hline(yintercept = 0) +
  labs(
    x = "Chat bot interaktion (normaliseret)",
    y = "Forudsagt læring"
  ) +
  theme_simon(base_size = 14) +
  scale_y_continuous(limits = c(-0.8, 0.8))

### BEHAVIORIAL REGRESSIONS ###
# How do the behavorial items interact? #

### Let's look at items z transformed
summary(lm(læring_total ~ z_chars + z_rounds + z_time, data = df_hyp_2))

# z_chars looks like more chars - more learning

### Interaction - z_chars with the attention index and controls
summary(lm(læring_total ~ z_chars * interaction_index + pre_afstand_total + Tillid + subjektiv_forståelse, data = df_hyp_2))

## What happens if we test the "good user" archetype?
summary(lm(læring_total ~ z_chars + z_rounds + pre_afstand_total + Tillid + subjektiv_forståelse, data = df_hyp_2))

## Interact them
good_user_interaction <- lm(læring_total ~ z_chars * z_rounds + pre_afstand_total + Tillid + subjektiv_forståelse, data = df_hyp_2)




# Visualize the interactions #

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

h2_interaktion_tekst.pdf <-
  ggplot(pred_interaction,
       aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.04, color = NA) +
  geom_segment(aes(y = 0, xend = 2), colour = "black", alpha = 0.7) +
  labs(
    x = "Chat bot interaktionsniveau (normaliseret)",
    y = "Forudsagt læring",
    color = str_wrap("Informationsmængde (standardiseret)", width = 15),
    fill = str_wrap("Informationsmængde (standardiseret)", width = 15),
    caption = str_wrap("Note: Forudsagte værdier baseret på lineær regression med 95% konfidensintervaller, og kontrol for tillid og subjektiv forståelse.", width = 45), hjust = 0.5
  ) +
  scale_color_manual(
    values = c("#D55E00", "#009E73", "#0072B2"),
    labels = c("Lav (-1 SD)", "Gennemsnitlig", "Høj (+1 SD)")
  ) +
  scale_fill_manual(
    values = c("#D55E00", "#009E73", "#0072B2"),
    labels = c("Lav (-1 SD)", "Gennemsnitlig", "Høj (+1 SD)")
  ) +
  theme_simon(base_size = 12, caption_size = 10) +
  theme(
    plot.caption = element_text(margin = margin(t = 25)),
    legend.position = "bottom"
  )

ggsave("h2_interation_tekst.pdf",
       plot = h2_interaktion_tekst.pdf,
       width = 6,
       height = 6)

# Plot 2
# m_interaction <- lm(
#   læring_total ~ z_chars * interaction_index +
#     pre_afstand_total + Tillid + subjektiv_forståelse,
#   data = df_hyp_2
# )
#
# pred_interaction <- ggpredict(
#   m_interaction,
#   terms = c("z_chars", "interaction_index [-1, 0, 1]")
# )
#
# ggplot(pred_interaction,
#        aes(x = x, y = predicted, color = group, fill = group)) +
#   geom_line(linewidth = 1) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
#               alpha = 0.15, color = NA) +
#   geom_hline(yintercept = 0, alpha = 0.7) +
#   labs(
#     x = "Tekstmængde (standardiseret)",
#     y = "Forudsagt læring",
#     color = "Interaktion",
#     fill = "Interaktion"
#   ) +
#   scale_color_discrete(
#     labels = c("Lav interaktion (-1 SD)", "Gennemsnitlig interaktion", "Høj interaktion (+1 SD)")
#   ) +
#   scale_fill_discrete(
#     labels = c("Lav interaktion (-1 SD)", "Gennemsnitlig interaktion", "Høj interaktion (+1 SD)")
#   ) +
#   theme_simon(base_size = 14)


# Visualize the interaction

# # Lets visualize the good user interaction
# pred_interaction <- ggpredict(
#   good_user_interaction,
#   terms = c("z_chars", "z_rounds [-1, 0, 1]")
# )
#
# ggplot(pred_interaction,
#        aes(x = x, y = predicted, color = group, fill = group)) +
#   geom_line(linewidth = 1) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
#               alpha = 0.15, color = NA) +
#   geom_hline(yintercept = 0, alpha = 0.7) +
#   labs(
#     x = "z_chars",
#     y = "Forudsagt læring",
#     color = "Runder",
#     fill = "Runder"
#   ) +
#   scale_color_discrete(
#     labels = c("Lavt antal runder (-1 SD)", "Gennemsnitlig antal runder", "Højt antal runder (+1 SD)")
#   ) +
#   scale_fill_discrete(
#     labels = c("Lavt antal runder (-1 SD)", "Gennemsnitlig antal runder", "Højt antal runder (+1 SD)")
#   ) +
#   theme_simon(base_size = 14)


# split til to grupper - dem med one shot-interaktioner, og dem uden
df_hyp_2 <-
  df_hyp_2 |>
  mutate(one_shot = case_when(
    is.na(max_turn) ~ "artikelbruger",
    max_turn <= 2 ~ "ingen brugerforespørgsel",
    max_turn == 3 ~ "én brugerforespørgsel",
    max_turn > 3 ~ "flere brugerforespørgsler"
  ))


## Regressioner med de nye brugertyper
# Effekten af one_shot variablen
summary(lm(læring_total ~ one_shot + pre_afstand_total + Tillid + subjektiv_forståelse, data = df_hyp_2 |> filter(one_shot != "artikelbruger")))

# One_shot kombineret med z_chars
summary(lm(læring_total ~ one_shot + z_chars + pre_afstand_total + Tillid + subjektiv_forståelse, data = df_hyp_2 |> filter(one_shot != "artikelbruger")))

# Interaktion med z_chars
summary(lm(læring_total ~ one_shot * z_chars + pre_afstand_total + Tillid + subjektiv_forståelse, data = df_hyp_2 |> filter(one_shot != "artikelbruger")))

# Visualize the one shot reg
# One-shot model
one_shot_reg <- lm(
  læring_total ~ one_shot + z_chars + pre_afstand_total + Tillid + subjektiv_forståelse,
  data = df_hyp_2 |> filter(one_shot != "artikelbruger")
)

# Count n observations pr. one_shot category
n_df_one_shot <- df_hyp_2 |>
  filter(one_shot != "artikelbruger") |>
  group_by(one_shot) |>
  summarise(n = n(), .groups = "drop")

# Marginal means plot data
newdata_one_shot <- data.frame(
  one_shot = c("ingen brugerforespørgsel", "én brugerforespørgsel", "flere brugerforespørgsler"),
  z_chars = mean(df_hyp_2$z_chars, na.rm = TRUE),
  pre_afstand_total = mean(df_hyp_2$pre_afstand_total, na.rm = TRUE),
  Tillid = mean(df_hyp_2$Tillid, na.rm = TRUE),
  subjektiv_forståelse = mean(df_hyp_2$subjektiv_forståelse, na.rm = TRUE)
)

# Make sure factor/order matches plot order
newdata_one_shot <- newdata_one_shot |>
  mutate(one_shot = factor(one_shot, levels = c("ingen brugerforespørgsel", "én brugerforespørgsel", "flere brugerforespørgsler")))

n_df_one_shot <- n_df_one_shot |>
  mutate(one_shot = factor(one_shot, levels = c("ingen brugerforespørgsel", "én brugerforespørgsel", "flere brugerforespørgsler")))

pred_one_shot <- predict(one_shot_reg, newdata = newdata_one_shot, interval = "confidence")

pred_df_one_shot <- bind_cols(newdata_one_shot, as.data.frame(pred_one_shot)) |>
  left_join(n_df_one_shot, by = "one_shot")

labels_vec_one_shot <- setNames(
  paste0(pred_df_one_shot$one_shot, "\n(n = ", pred_df_one_shot$n, ")"),
  pred_df_one_shot$one_shot
)

p_one_shot <-
  ggplot(pred_df_one_shot, aes(x = one_shot, y = fit)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.08) +
  geom_hline(yintercept = 0, alpha = 0.8) +
  scale_x_discrete(labels = labels_vec_one_shot) +
  theme_simon(base_size = 14, caption_size = 11) +
  labs(
    y = "Forudsagt læring"
  ) +
  theme(
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    title = element_text(hjust = 0.5)
  )

p_one_shot

ggsave("h2_one_shot.pdf",
       plot = p_one_shot,
       width = 6,
       height = 6
)



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


