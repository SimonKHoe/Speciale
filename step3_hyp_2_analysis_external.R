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
library(ggeffects)

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

# This is the ITT DF
df <- df_cutoff_filtered # This is the default setting. Don't change


# THIS PART IS NOT RUNNABLE WITH THE ANONYMIZED DATA, BUT IS KEPT OUTCOMMENTED
# FOR TRANSPARENCY

#### THE CREATION PIPE OF HYP_2 AND HYP 2_2 AND TEXT EXPORT STARTS ####

# # Check the variables
# LUCIDUserfacinghistory <- df |> pull(LUCIDUserFacingHistory)
#
# # Create a table in long format for each round of conversations
# conversation_table <- map2_dfr(
#   df$LUCIDUserFacingHistory,
#   seq_len(nrow(df)),
#   \(txt, id) {
#     tibble(raw = txt, conv_id = id) %>%
#       mutate(turns = str_split(
#         raw,
#         "\\s*(?=\\[(?:assistant|user)\\]:)",
#         simplify = FALSE
#       )) %>%
#       unnest(turns) %>%
#       filter(turns != "") %>%
#       mutate(
#         turn_order = row_number(),
#         role = str_extract(turns, "(?<=\\[)(assistant|user)(?=\\]:)"),
#         content = str_remove(turns, "^\\[(?:assistant|user)\\]:\\s*")
#       ) %>%
#       select(conv_id, turn_order, role, content)
#   }
# )
#
# # Join df tilbage på, og lav interaktionsvariable
# conversation_table_joined <-
#   conversation_table |>
#   left_join(df, by = "conv_id")
#
#
# #### HYPOTHESIS 2 ####
#
# # Index on regression granularity
# max_turn <-
#   conversation_table_joined |>
#   group_by(conv_id) |>
#   slice_max(turn_order, n = 1) |>
#   ungroup() |>
#   select(conv_id, turn_order) |>
#   rename(max_turn = turn_order)
#
# # Join back
# df_hyp_2 <-
#   df |>
#   left_join(max_turn)
#
# # Export for use in step 3.2
# df_hyp_2 |>
#   select(-LUCIDUserFacingHistory) |>  # Remove the convos
#   saveRDS("df_hyp_2.rds")
#
# # This creates interaction variables from the convos
# df_hyp_2_2 <-
#   df_hyp_2 |>
#   mutate( # add variable, that countrs # units
#     n_chars = nchar(
#       str_remove(
#         as.character(LUCIDUserFacingHistory),
#         "^\\[assistant\\]:.*?(?=\\[user\\]:)"
#       )
#     )
#   ) |>
#   mutate(conv_time_s = as.numeric(as.character(LUCIDTotalConvTimeMs)) / 1000) |> # Turn time with bot into seconds
#   mutate( # Create index for interactivity
#     z_chars  = as.numeric(scale(n_chars)),
#     z_rounds = as.numeric(scale(max_turn)),
#     z_time   = as.numeric(scale(conv_time_s)),
#     interaction_index = (z_rounds + z_time) / 2,
#     interaction_index_chars = (z_chars + z_rounds + z_time) / 3
#   )
#
# # Export for external use
# df_hyp_2_2 |>
#   select(-LUCIDUserFacingHistory) |>
#   saveRDS("df_hyp_2_2.rds")

### THE CREATION OF HYP_2 and HYP_2_2 DATA ENDS ###



### ANALYSIS STARTS ###

# Load the df's needed to run script.
# Has the necessary variables, but not the conversations themselves
df_hyp_2_2 <-
  readRDS("df_hyp_2_2.rds")

# Interaction X Learning #

### Interaction index regression ###
h2_bivariate <- lm(læring_total ~ interaction_index, data = df_hyp_2_2)
summary(h2_bivariate)


### Interaction index regression pre ###
h2_no_chars <- lm(læring_total ~ interaction_index + pre_afstand_total, data = df_hyp_2_2)
summary(h2_no_chars)

### Interaction index regression with controls
# summary(lm(læring_total ~ interaction_index + pre_afstand_total + Tillid + subjektiv_forståelse, data = df_hyp_2))

### Interaction index regression with n_char control
h2_control <- lm(læring_total ~ interaction_index + z_chars + pre_afstand_total, data = df_hyp_2_2)
summary(h2_control)

# Plot the interaction index binary
pred <- ggpredict(
  lm(læring_total ~ interaction_index + pre_afstand_total + n_chars, data = df_hyp_2_2),
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
  theme_simon(base_size = 14)
# + scale_y_continuous(limits = c(-0.8, 0.8))

### BEHAVIORIAL REGRESSIONS AND INDEX SPLITS ###
# How do the behavorial items interact? #

### Let's look at items z transformed
summary(lm(læring_total ~ z_chars + z_rounds + z_time, data = df_hyp_2_2))

# z_chars looks like more chars - more learning

### Interaction - z_chars with the attention index and controls
h2_interaction <- lm(læring_total ~ z_chars * interaction_index + pre_afstand_total , data = df_hyp_2_2)
summary(h2_interaction)

# Visualize the interactions #

# Plot 1

pred_interaction <- ggpredict(
  h2_interaction,
  terms = c("interaction_index", "z_chars [meansd]")
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
    color = str_wrap("Informationsmængde", width = 15),
    fill = str_wrap("Informationsmængde", width = 15),
    caption = str_wrap("Note: Forudsagte værdier baseret på lineær regression med 95% konfidensintervaller.", width = 45), hjust = 0.5
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

# NB the linear regs will be exported along with one-shot reg later



# Split into three groups - none, one shots and multiples
df_hyp_2_one_shot <-
  df_hyp_2_2 |>
  mutate(one_shot = case_when(
    is.na(max_turn) ~ "artikelbruger",
    max_turn <= 2 ~ "ingen brugerforespørgsel",
    max_turn == 3 ~ "én brugerforespørgsel",
    max_turn > 3 ~ "flere brugerforespørgsler"
  ))


## Regressioner med de nye brugertyper

# Visualize the one shot reg
# One-shot model
one_shot_reg <- lm(
  læring_total ~ one_shot + z_chars + pre_afstand_total,
  data = df_hyp_2_one_shot |> filter(one_shot != "artikelbruger")
)

# Count n observations pr. one_shot category
n_df_one_shot <- df_hyp_2_one_shot |>
  filter(one_shot != "artikelbruger") |>
  group_by(one_shot) |>
  summarise(n = n(), .groups = "drop")

# Marginal means plot data
newdata_one_shot <- data.frame(
  one_shot = c("ingen brugerforespørgsel", "én brugerforespørgsel", "flere brugerforespørgsler"),
  z_chars = mean(df_hyp_2_one_shot$z_chars, na.rm = TRUE),
  pre_afstand_total = mean(df_hyp_2_one_shot$pre_afstand_total, na.rm = TRUE)
  # ,Tillid = mean(df_hyp_2_one_shot$Tillid, na.rm = TRUE),
  # subjektiv_forståelse = mean(df_hyp_2_one_shot$subjektiv_forståelse, na.rm = TRUE)
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
  ggplot(pred_df_one_shot,
         aes(x = one_shot, y = fit)) +
  geom_point(size = 2.5, shape = 17) +
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
       height = 5
)

### MODELSUMMARY FOR LINEAR REGS AND ONE-SHOT REG
### ADD THE MAIN REGS IN MODELSUMMARY ###
modelsummary(
  list(
    "Bivariat" = h2_bivariate,
    "Kontrol for præ-afstand" = h2_no_chars,
    "Kontrol for tekstmængde" = h2_control,
    "Interaktion" = h2_interaction,
    "One-shot reg" = one_shot_reg
  ),
  stars = TRUE,
  fmt = 3,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map = c(
    "(Intercept)" = "Konstant",
    "interaction_index" = "Interaktionsniveau",
    "pre_afstand_total" = "Præ-treatment afstand",
    "z_chars" = "Tekstmængde (standardiseret)",
    "z_chars:interaction_index" = "Interaktionsniveau × tekstmængde",
    "one_shotingen brugerforespørgsel" = "Ingen brugerforespørgsel",
    "one_shotflere brugerforespørgsler" = "Flere brugerforespørgsler"
  ),
  gof_map = c(
    "nobs",
    "r.squared",
    "adj.r.squared"
  ),
  title = "Interaktion og læring i chatbotbetingelsen",
  output = "regressions/h2_regressioner_2.tex"
)


### REGRESSIONS WITH NO INTERACTIONS EXCLUDED ###
df_excluded <-
  df_hyp_2_one_shot |>
  filter(one_shot != "ingen brugerforespørgsel")

# Bivariate
h2_excluded_bivariate <- lm(læring_total ~ interaction_index, data = df_excluded)
summary(h2_excluded_bivariate)


# Pre-learning
h2_excluded_pre <- lm(læring_total ~ interaction_index + pre_afstand_total, data = df_excluded)
summary(h2_excluded_pre)

### Interaction index regression with controls
# summary(lm(læring_total ~ interaction_index + pre_afstand_total + Tillid + subjektiv_forståelse, data = df_hyp_2))

# z_chars added
h2_excluded_control <- lm(læring_total ~ interaction_index + z_chars + pre_afstand_total, data = df_excluded)
summary(h2_excluded_control)

### Interaction - z_chars with the attention index and controls
h2_excluded_interaction <- lm(læring_total ~ z_chars * interaction_index + pre_afstand_total , data = df_excluded)
summary(h2_excluded_interaction)

# Use interaction reg to make predictions
pred_excluded_interaction <- ggpredict(
  h2_excluded_interaction,
  terms = c("interaction_index", "z_chars [meansd]")
)

# Viz the interaction with excluded
h2_excluded_interaktion_tekst.pdf <-
  ggplot(pred_excluded_interaction,
         aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.04, color = NA) +
  geom_segment(
    aes(
      x = min(pred_excluded_interaction$x),
      xend = max(pred_excluded_interaction$x),
      y = 0,
      yend = 0
    ),
    inherit.aes = FALSE,
    colour = "black",
    alpha = 0.7
  ) +
  # geom_segment(aes(y = 0, xend = 3), colour = "black", alpha = 0.7) +
  #  geom_hline(yintercept = 0) +
  labs(
    x = "Chat bot interaktionsniveau (normaliseret)",
    y = "Forudsagt læring",
    color = str_wrap("Informationsmængde", width = 15),
    fill = str_wrap("Informationsmængde", width = 15),
    caption = str_wrap("Note: Forudsagte værdier baseret på lineær regression med 95% konfidensintervaller.", width = 45), hjust = 0.5
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

ggsave("h2_excluded_interaktion_tekst.pdf",
       plot = h2_excluded_interaktion_tekst.pdf,
       height = 6,
       width = 6
)


## MODELSUMMARY FOR THE EXCLUDED REGS ##
modelsummary(
  list(
    "Bivariat" = h2_excluded_bivariate,
    "Kontrol for præ-afstand" = h2_excluded_pre,
    "Kontrol for tekstmængde" = h2_excluded_control,
    "Interaktion" = h2_excluded_interaction
  ),
  stars = TRUE,
  fmt = 3,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map = c(
    "(Intercept)" = "Konstant",
    "interaction_index" = "Interaktionsniveau",
    "pre_afstand_total" = "Præ-treatment afstand",
    "z_chars" = "Tekstmængde (standardiseret)",
    "z_chars:interaction_index" = "Interaktionsniveau × tekstmængde"
  ),
  gof_map = c(
    "nobs",
    "r.squared",
    "adj.r.squared"
  ),
  title = "Logged interaktion og læring i chatbotbetingelsen",
  output = "regressions/h2_excluded_regressioner.tex"
)


### THIS IS THE LOG TRANSFORMED ###
# index creation
log_df_hyp_2 <-
  df_hyp_2_one_shot |>
  filter(one_shot != "ingen brugerforespørgsel") |>
  mutate( # add variable, that countrs # units # Doing this again is not strictly necessary, but legacy
    n_chars = nchar(
      str_remove(
        as.character(LUCIDUserFacingHistory),
        "^\\[assistant\\]:.*?(?=\\[user\\]:)"
      )
    )
  ) |>
  mutate(conv_time_s = as.numeric(as.character(LUCIDTotalConvTimeMs)) / 1000) |> # Turn time with bot into seconds
  mutate( # Create index for interactivity
    log_rounds = log1p(max_turn), # Log transform the index this time
    log_time   = log1p(conv_time_s),
    log_chars = log1p(n_chars),
    z_chars  = as.numeric(scale(log_chars)),
    z_rounds = as.numeric(scale(log_rounds)),
    z_time   = as.numeric(scale(log_time)),
    interaction_index = (z_rounds + z_time) / 2,
    interaction_index_chars = (z_chars + z_rounds + z_time) / 3
  )


### Interaction index regression ###
h2_log_bivariate <- lm(læring_total ~ interaction_index, data = log_df_hyp_2)
summary(h2_log_bivariate)


### Interaction index regression pre ###
h2_log_no_chars <- lm(læring_total ~ interaction_index + pre_afstand_total, data = log_df_hyp_2)
summary(h2_log_no_chars)

### Interaction index regression with controls
# summary(lm(læring_total ~ interaction_index + pre_afstand_total + Tillid + subjektiv_forståelse, data = df_hyp_2))

### Interaction index regression with n_char control
h2_log_control <- lm(læring_total ~ interaction_index + z_chars + pre_afstand_total, data = log_df_hyp_2)
summary(h2_log_control)

### BEHAVIORIAL REGRESSIONS AND INDEX SPLITS ###
# How do the behavorial items interact? #

### Interaction - z_chars with the attention index and controls
h2_log_interaction <- lm(læring_total ~ z_chars * interaction_index + pre_afstand_total , data = df_hyp_2_2)
summary(h2_log_interaction)


### ADD THE MAIN REGS IN MODELSUMMARY ###
modelsummary(
  list(
    "Bivariat" = h2_log_bivariate,
    "Kontrol for præ-afstand" = h2_log_no_chars,
    "Kontrol for tekstmængde" = h2_log_control,
    "Interaktion" = h2_log_interaction
  ),
  stars = TRUE,
  fmt = 3,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map = c(
    "(Intercept)" = "Konstant",
    "interaction_index" = "Interaktionsniveau",
    "pre_afstand_total" = "Præ-treatment afstand",
    "z_chars" = "Tekstmængde (standardiseret)",
    "z_chars:interaction_index" = "Interaktionsniveau × tekstmængde"
  ),
  gof_map = c(
    "nobs",
    "r.squared",
    "adj.r.squared"
  ),
  title = "Logged interaktion og læring i chatbotbetingelsen",
  output = "regressions/h2_log_regressioner.tex"
)

### BEHAVIORIAL REGRESSIONS AND INDEX SPLITS ###
# How do the behavorial items interact? #

### Interaction - z_chars with the attention index and controls
h2_log_interaction <- lm(læring_total ~ z_chars * interaction_index + pre_afstand_total , data = df_hyp_2_2)
summary(h2_log_interaction)

### NO INTERACTIONS ENDS ###


# TRUST INTERACTION Interactionindex #
summary(lm(læring_total ~ Tillid + interaction_index + pre_afstand_total, data = df_hyp_2_2 |> filter(treatment == "chat bot")))

summary(lm(læring_total ~ Tillid * interaction_index + pre_afstand_total, data = df_hyp_2_2 |> filter(treatment == "chat bot")))

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
  df_hyp_2_2 |>
  summarise(averabe_conv_length = mean(conv_time_s, na.rm = TRUE))

# Grab the average number of characters for each user - opening message from Polibob
average_chars_num <-
  df_hyp_2_2 |>
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
  labs(x = str_wrap("Antal beskeder i chat bot samtalen", 25), y = "Frekvens (absolutte tal)") +
  theme(
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.ticks.x = element_blank(),
    title = element_text(hjust = 0.5)
  )

p2 <-
  df_hyp_2_2 |>
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
  df_hyp_2_2 |>
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

ggsave("appendix_a/combined_histograms.pdf",
       plot = combined_histograms,
       width = 6,
       height = 5)

# Visualize interaction level
interaktion_hist <-
  df_hyp_2_2 |>
  filter(treatment == "chat bot") |>
  ggplot(aes(x = interaction_index)) +
  geom_histogram(
    bins = 30,
    alpha = 0.7
  ) +
  geom_vline(
    xintercept = mean(df_hyp_2_2$interaction_index, na.rm = TRUE),
    linetype = "dashed",
    alpha = 0.7
  ) +
  theme_simon(base_size = 14, ticks = FALSE) +
  labs(
    title = str_wrap("Fordelingen af interaktionsniveau (z-transformeret og uden ekskludering)", 50),
    x = "Interaktionsniveau",
    y = "Antal respondenter"
  ) +
  theme(
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15))
  )

ggsave(
  "interaktion_hist.pdf",
  plot = interaktion_hist,
  height = 5,
  width = 6
)
