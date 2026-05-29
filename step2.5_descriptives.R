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
library(dotwhisker)
library(ggeffects)
library(ggthemes)
library(modelsummary)
library(tidyverse)
source("utils.R")

#### Load data ####
df_analysis <-
  read_rds("df_analysis.rds")


# Define the df with the failed interactions filtered and manipulation check
df_failed <-
  df_analysis |>
#  filter(Q8_1 == 0 | is.na(Q8_1)) |>
#  filter(partier_folketing == "179")
  filter(Progress > 75)

# Define a df where cutoff is introduced
df_cutoff_filtered <-
  df_failed |>
  filter((treatment == "chat bot" & after_cutoff == "after" | treatment == "artikel"))

# Set df for the entire regression results viz section here
# df <- df_analysis
df <- df_cutoff_filtered # This is the main group # 247
# df <- df_failed

#### ####

#### Descriptives ####

### Speeders ###
df |>
  filter(Duration__in_seconds_ < 60)
# No speeders


### PARTIES ###
# Pre-placements #
# Prep the plot #
df_pre_long <-
  df |>
  select(pre_afstand_DF, pre_afstand_LA, pre_afstand_SF,
         pre_afstand_RV, pre_afstand_EL, pre_afstand_V) |>
  pivot_longer(
    cols = everything(),
    names_to = "parti",
    values_to = "afstand"
  ) |>
  mutate(
    parti = str_remove(parti, "pre_afstand_"),
    parti = factor(parti, levels = c("DF", "LA", "SF", "RV", "EL", "V"))
  )

# PRE #
# summary
pre_summary <- df_pre_long |>
  group_by(parti) |>
  summarise(
    mean_afstand = mean(afstand, na.rm = TRUE),
    sd_afstand = sd(afstand, na.rm = TRUE),
    n = sum(!is.na(afstand)),
    se = sd_afstand / sqrt(n),
    .groups = "drop"
  )

# Plot
 pre_bar <-
  ggplot(pre_summary, aes(x = parti |> fct_reorder(desc(mean_afstand)), y = mean_afstand)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_afstand - 1.96 * se,
                    ymax = mean_afstand + 1.96 * se),
                width = 0.2) +
  labs(
#    title = "Gennemsnitlig afstand i pre-placeringer pr. parti",
    y = "Gennemsnitlig afstand til ekspertplacering"
  ) +
  theme_simon(base_size = 14) +
  theme(
    axis.title.x = element_blank(),
#    axis.title.y = element_blank()
  ) +
  scale_y_continuous(breaks = seq(0, 2.5, by = 0.5), limits = c(0,2.8))

ggplot(pre_summary, aes(x = parti |> fct_reorder(desc(mean_afstand)),
                        y = mean_afstand)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_afstand - 1.96 * se,
                    ymax = mean_afstand + 1.96 * se),
                width = 0.15) +
  labs(
    y = "Gennemsnitlig afstand til ekspertplacering"
  ) +
  theme_simon(base_size = 12) +
  theme(
    axis.title.x = element_blank()
  ) +
  scale_y_continuous(breaks = seq(0, 2.5, by = 0.5), limits = c(0, 2.8))

# Split in treatments
party_order <- pre_summary |>
  group_by(parti) |>
  summarise(mean_pre = mean(mean_afstand), .groups = "drop") |>
  arrange(desc(mean_pre)) |>
  pull(parti)

df_pre_long <-
  df |>
  select(treatment, pre_afstand_DF, pre_afstand_LA, pre_afstand_SF,
         pre_afstand_RV, pre_afstand_EL, pre_afstand_V) |>
  pivot_longer(
    cols = -treatment,
    names_to = "parti",
    values_to = "afstand"
  ) |>
  mutate(
    parti = str_remove(parti, "pre_afstand_"),
    parti = factor(parti, levels = c("DF", "LA", "SF", "RV", "EL", "V")),
    treatment = as.factor(treatment)
  )

pre_summary <- df_pre_long |>
  group_by(treatment, parti) |>
  summarise(
    mean_afstand = mean(afstand, na.rm = TRUE),
    sd_afstand = sd(afstand, na.rm = TRUE),
    n = sum(!is.na(afstand)),
    se = sd_afstand / sqrt(n),
    .groups = "drop"
  ) |>
  mutate(parti = factor(parti, levels = party_order))

pd <- position_dodge(width = 0.45)

pre_points <-
 pre_summary |>
  ggplot(aes(x = parti, y = mean_afstand, color = treatment, shape = treatment)) +
  geom_point(size = 2.8, position = pd) +
  geom_errorbar(aes(ymin = mean_afstand - 1.96 * se,
                    ymax = mean_afstand + 1.96 * se),
                width = 0.12,
                position = pd) +
  labs(y = "Præ-afstand",
       shape = "Treatment",
       color = "Treatment") +
  theme_simon(base_size = 12) +
  scale_color_manual(values = c(
    "chat bot" = "#000000",
    "artikel"  = "#A3A3A3"
  )) +
  scale_shape_manual(values = c(
    "chat bot" = 17,
    "artikel"  = 16
  )) +
  theme(
    axis.title.x = element_blank(),
    legend.position = "bottom"
  )



## POST PLACERINGER ##
df_post_long <-
  df |>
  select(post_afstand_DF, post_afstand_LA, post_afstand_SF,
         post_afstand_RV, post_afstand_EL, post_afstand_V) |>
  pivot_longer(
    cols = everything(),
    names_to = "parti",
    values_to = "afstand"
  ) |>
  mutate(
    parti = str_remove(parti, "post_afstand_"),
    parti = factor(parti, levels = c("DF", "LA", "SF", "RV", "EL", "V"))
  )

# summary
post_summary <- df_post_long |>
  group_by(parti) |>
  summarise(
    mean_afstand = mean(afstand, na.rm = TRUE),
    sd_afstand = sd(afstand, na.rm = TRUE),
    n = sum(!is.na(afstand)),
    se = sd_afstand / sqrt(n),
    .groups = "drop"
  )

# Plot
post_bar <-
  ggplot(post_summary, aes(x = parti |> fct_reorder(desc(mean_afstand)), y = mean_afstand)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_afstand - 1.96 * se,
                    ymax = mean_afstand + 1.96 * se),
                width = 0.2) +
  labs(
    title = "Gennemsnitlig afstand i post-placeringer pr. parti",
    x = "Parti",
    y = "Gennemsnitlig afstand til ekspertplacering"
  ) +
  theme_simon(base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
#    axis.ticks.y = element_blank()
  ) +
  scale_y_continuous(breaks = seq(0, 2.5, by = 0.5), limits = c(0,2.8)
                     # , labels = NULL
                     )

ggsave("appendix_a/post_bar.pdf",
  plot = post_bar,
  height = 6,
  width = 6
)


## LÆRING ##
df_læring_long <-
  df |>
  select(læring_DF, læring_LA, læring_SF,
         læring_RV, læring_EL, læring_V) |>
  pivot_longer(
    cols = everything(),
    names_to = "parti",
    values_to = "afstand"
  ) |>
  mutate(
    parti = str_remove(parti, "læring_"),
    parti = factor(parti, levels = c("DF", "LA", "SF", "RV", "EL", "V"))
  )

# PRE #
# summary
læring_summary <- df_læring_long |>
  group_by(parti) |>
  summarise(
    mean_afstand = mean(afstand, na.rm = TRUE),
    sd_afstand = sd(afstand, na.rm = TRUE),
    n = sum(!is.na(afstand)),
    se = sd_afstand / sqrt(n),
    .groups = "drop"
  ) |>
  mutate(parti = factor(parti, levels = party_order))

# Plot
læring_bar <-
  ggplot(læring_summary, aes(x = parti |> fct_reorder(desc(mean_afstand)), y = mean_afstand)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_afstand - 1.96 * se,
                    ymax = mean_afstand + 1.96 * se),
                width = 0.2) +
  labs(
    title = "Læring pr. parti",
    x = "Parti",
    y = "Gennemsnitlig afstand til ekspertplacering"
  ) +
  theme_simon(base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
  )


# Patchwork
pre_post_bars <- pre_bar + post_bar



# Learning facetted on treatment #
df_læring_long <-
  df |>
  select(treatment, læring_DF, læring_LA, læring_SF,
         læring_RV, læring_EL, læring_V) |>
  pivot_longer(
    cols = -treatment,
    names_to = "parti",
    values_to = "afstand"
  ) |>
  mutate(
    parti = str_remove(parti, "læring_"),
    parti = factor(parti, levels = c("DF", "LA", "SF", "RV", "EL", "V")),
    treatment = as.factor(treatment)
  )

# Summary pr. treatment og parti
læring_summary <- df_læring_long |>
  group_by(treatment, parti) |>
  summarise(
    mean_afstand = mean(afstand, na.rm = TRUE),
    sd_afstand = sd(afstand, na.rm = TRUE),
    n = sum(!is.na(afstand)),
    se = sd_afstand / sqrt(n),
    .groups = "drop"
  ) |>
  mutate(parti = factor(parti, levels = party_order))

# Plot

# Læring dotwhisker

pd <- position_dodge(width = 0.45)

læring_dotwhisker <-
  ggplot(
    læring_summary,
    aes(
      x = parti,
      y = mean_afstand,
      color = treatment,
      shape = treatment
    )
  ) +
  geom_point(size = 2.8, position = pd) +
  geom_errorbar(
    aes(
      ymin = mean_afstand - 1.96 * se,
      ymax = mean_afstand + 1.96 * se
    ),
    width = 0.12,
    position = pd
  ) +
  geom_hline(yintercept = 0, alpha = 0.85) +
  labs(
#    title = "Læring pr. parti og treatment",
    x = "Parti",
    y = "Gennemsnitlig læring",
    color = "Treatment",
    shape = "Treatment"
  ) +
  theme_simon(base_size = 12) +
  scale_color_manual(values = c(
    "chat bot" = "#000000",
    "artikel"  = "#A3A3A3"
  )) +
  scale_shape_manual(values = c(
    "chat bot" = 17,
    "artikel"  = 16
  )) +
  scale_y_continuous(breaks = seq(-0.4, 1.4, by = 0.2)) +
  theme(
    axis.title.x = element_blank(),
#    axis.title.y = element_blank()
  )


# patchwork pre_placement and learning
pre_og_læring_patchwork <-
  pre_points / læring_dotwhisker +
  theme(legend.position = "bottom")

# Export patchwork
ggsave("pre_og_læring_patch.pdf",
       plot = pre_og_læring_patchwork,
       height = 7,
       width = 6
)


### Variable ###

# Pre - placering #
# Delt op på treatment
pre_afstand_boxplot <-
  df |>
  ggplot(aes(x = treatment, y = pre_afstand_total)) +
  geom_boxplot() +
  theme_simon(base_size = 14, ticks = FALSE) +
  geom_hline(yintercept = 0, size = 1.1, color = "grey") +
  scale_y_continuous(breaks = seq(-1, 6, by = 0.5)) +
  labs(title = "Fordelingen af pre-afstand mellem de to treatments", y = "Pre-afstand", x = "Treatment") +
  theme(
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)))

ggsave("appendix_a/pre_afstand_boxplot.pdf",
       plot = pre_afstand_boxplot,
       height = 6,
       width = 6)

# Spredning samlet
pre_treatment_spredning <-
  df |>
  ggplot(aes(x = pre_afstand_total)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 0.1, alpha = 0.4) +
  geom_density(linewidth = 1) +
  labs(
    title = "Fordelingen i afstand inden treatment",
    y = "Tæthed"
  ) +
  theme_simon(base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    panel.spacing = unit(1.5, "cm")
  )

ggsave("appendix_a/pre_treatment_spredning.pdf",
       plot = pre_treatment_spredning,
       height = 6,
       width = 6)

# Post - placering #
# Delt op på treatment
post_afstand_boxplot <-
  df |>
  ggplot(aes(x = treatment, y = post_afstand_total)) +
  geom_boxplot() +
  theme_simon(base_size = 14, ticks = FALSE) +
  geom_hline(yintercept = 0, size = 1.1, color = "grey") +
  scale_y_continuous(breaks = seq(-1, 6, by = 0.5)) +
  labs(title = "Fordelingen af post-afstand mellem de to treatments", y = "Post-afstand", x = "Treatment") +
  theme(
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)))

ggsave("appendix_a/post_afstand_boxplot.pdf",
       plot = post_afstand_boxplot,
       height = 6,
       width = 6)

# Spredning samlet
post_treatment_spredning <-
  df |>
  ggplot(aes(x = post_afstand_total)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 0.1, alpha = 0.4) +
  geom_density(linewidth = 1) +
  labs(
    title = "Fordelingen i afstand efter treatment",
    y = "Tæthed"
  ) +
  theme_simon(base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    panel.spacing = unit(1.5, "cm")
  )

ggsave("appendix_a/post_treatment_spredning.pdf",
       plot = post_treatment_spredning,
       height = 6,
       width = 6)

### Attention check
attention_bars <-
  df |>
  ggplot(aes(x = Q9)) +
  geom_bar(aes(y = after_stat(prop), group = 1)) +
  geom_text(
    stat = "count",
    aes(
      y = after_stat(prop),
      label = scales::percent(after_stat(prop), accuracy = 1),
      group = 1
    ),
    vjust = -0.3
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_simon(base_size = 14) +
  theme(
    axis.title.x = element_blank()
  ) +
  scale_x_discrete(labels = \(x) stringr::str_wrap(x, width = 10)) +
  labs(
    title = "Fordelingen af svar til attention check",
    x = "Hvad handlede din [treatment] primært om?",
    y = "Procent"
  )

ggsave("appendix_a/attention_bars.pdf",
       plot = attention_bars,
       height = 5,
       width = 6)

# Chat bot issues #

# Grab labels
var_labels <- sapply(
  df |> select(starts_with("Q8_")),
  function(x) str_replace(var_label(x), "^.*Selected Choice\\s*-?\\s*:?\\s*", "")
)

# Pivot multi selects
n_respondenter <- df |>
  select(starts_with("Q8_"), -Q8_5_TEXT) |>
  filter(if_any(everything(), ~ !is.na(.))) |>
  nrow()

df_plot <- df |>
  select(-Q8_5_TEXT) |>
  select(starts_with("Q8_")) |>
  pivot_longer(
    cols = everything(),
    names_to = "option",
    values_to = "selected"
  ) |>
  filter(!is.na(selected)) |>
  filter(selected == 1) |>
  count(option) |>
  mutate(number = n)

# Readd labels

df_long_cb <- df_plot |>
  mutate(option_label = var_labels[option])

# Plot
problemer_chat_bot <-
  df_long_cb |>
  ggplot(aes(x = reorder(option_label, -number), y = number)) +
  geom_col(width =0.5) +
  theme_simon(base_size = 14) +
  scale_x_discrete(labels = \(x) stringr::str_wrap(x, width = 10)) +
  theme(axis.title.x = element_blank()) +
  labs(
    title = paste0(
      "Fordelingen af rapporterede problemer med chatbotten (n = ",
      n_respondenter,
      ")"
    ),
    y = "Antal, der har valgt kategorien"
  )

ggsave("appendix_a/problemer_chat_bot.pdf",
       plot = problemer_chat_bot,
       height = 4,
       width = 6)

# Plot the single select issues question
problemer_chat_bot_single <-
  df |>
  filter(!is.na(Q7)) |>
  ggplot(aes(x = Q7)) +
  geom_bar(aes(y = after_stat(count / sum(count))),
           width = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  theme_simon() +
  labs(title = str_wrap("Fordelingen på single select spørgsmålet, om man havde problemer med chatbotten", 80,),
       y = "Procent") +
  theme(axis.title.x = element_blank()
  ) +
  scale_x_discrete(labels = \(x) stringr::str_wrap(x, width = 20))

ggsave("problemer_chat_bot_single.pdf",
       plot = problemer_chat_bot_single,
       height = 4,
       width = 6)


# Visualize the chatbot issues text answers
df_quotes <- df |>
  filter(!is.na(Q8_5_TEXT), Q8_5_TEXT != "") |>
  mutate(
    id = row_number() * 3,
    quote = str_wrap(paste0("“", Q8_5_TEXT, "”"), width = 100)
  )

text_answers_problems <-
  ggplot(df_quotes, aes(x = 1, y = id, label = quote)) +
  geom_text(hjust = 0, size = 3.5, lineheight = 1.1) +
  xlim(1, 5) +
  theme_void()

ggsave("appendix_a/text_answers_problems.pdf",
       plot = text_answers_problems)

# Time spent on article #
df |>
  ggplot(aes(x = Q40_time_Page_Submit)) +
  geom_histogram(
                 binwidth = 30, alpha = 0.4) +
  labs(
    title = "Fordelingen i tid brugt på artiklen, sekunder",
    y = "Frekvens (absolutte tal)"
  ) +
  theme_simon(base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    panel.spacing = unit(1.5, "cm")
  ) +
  scale_x_continuous(breaks = seq(0, 600, by = 30))

### Pull out average time spent on survey
average_time <-
  df |>
  summarise(mean_time_minutes = (mean(Duration__in_seconds_)) / 60) |>
  pull(mean_time_minutes)

### Pull median time spent
median_time <-
  df |>
  summarise(median_time_minutes = median(Duration__in_seconds_) / 60) |>
  pull(median_time_minutes)


# Check out distribution of time percent
chat_bot_time <-
  df |>
  filter(treatment == "chat bot") |>
  mutate(Duration_minutes = Duration__in_seconds_ / 60) |>
  ggplot(aes(x = Duration_minutes,
             y = after_stat(count / sum(count)))) +
  geom_histogram(bins = 100) +
  scale_y_continuous(labels = scales::percent) +
  theme_simon(base_size = 14) +
  labs(
    title = "Tid brugt på survey for chat bot brugere, utrunkeret",
    x = "Tid brugt på survey i minutter",
    y = "Procent af respondenter"
  )

ggsave("chat_bot_time.pdf",
       plot = chat_bot_time,
       height = 5,
       width = 6)

# Truncated percent
time_spent_hist <-
  df |>
  mutate(Duration_minutes = Duration__in_seconds_ / 60) |>
  ggplot(aes(x = Duration_minutes,
             y = after_stat(count / sum(count)))) +
  geom_histogram(bins = 12) +
  scale_y_continuous(label = scales::percent) +
  theme_simon(base_size = 14) +
  labs(
    x = "Tid brugt på survey i minutter",
    y = "Procent af respondenter",
    caption = str_wrap("Bemærk at x-aksen er trunkeret. 18 observationer er udenfor 11 minutter", 45)
  ) +
  scale_x_continuous(breaks = seq(0, 11, by = 1), limits = c(0, 12)) +
  theme(plot.caption = element_text(margin = margin(t = 40), size = 11))

ggsave("appendix_a/time_spent_hist.pdf",
       plot = time_spent_hist,
       height = 6,
       width = 6)

# Boxplot time
time_spent_treatments <-
  df |>
  mutate(Duration_minutes = Duration__in_seconds_ / 60) |>
  ggplot(aes(x = treatment, y = Duration_minutes)) +
  geom_boxplot() +
  theme_simon(base_size = 14, ticks = FALSE) +
  labs(
    title = str_wrap("Fordelingen af tid brugt mellem de to treatments med outliers", 35),
       y = "Tid brugt på survey i minutter",
       x = "Treatment",
#       caption = str_wrap("Bemærk at y-aksen er trunkeret. 15 observationer er udenfor det plottede område", 45)
       ) +
  theme(
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    plot.caption = element_text(margin = margin(t = 40), size = 11)
    )
#  scale_y_continuous(breaks = seq(0, 12, by = 1), limits = c(0, 12))

ggsave("appendix_a/time_spent_treatments_untruncated.pdf",
       plot = time_spent_treatments,
       height = 5,
       width = 6)


## BOXPLOT TREATMENTS ##

# Læring #
# visualize differences in learning using box plots
læring_box <-
  df |>
  ggplot(aes(x = treatment, y = læring_total)) +
  geom_boxplot() +
  theme_simon(base_size = 14, ticks = FALSE) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = seq(-6, 6, by = 0.5)) +
  labs(title = "Fordelingen af læring mellem de to treatments", y = "Samlet læring", x = "Treatment") +
  theme(
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15)))

ggsave("læring_box.pdf",
       plot = læring_box,
       height = 5,
       width = 6)

# Læring histogram
læring_hist <-
  df |>
  ggplot(aes(x = læring_total)) +
  geom_histogram(
    bins = 30,
    alpha = 0.7
  ) +
  geom_vline(xintercept = 0) +
  theme_simon(base_size = 14, ticks = FALSE) +
  labs(
    title = "Fordelingen af læring i samplen",
    x = "Samlet læring",
    y = "Antal respondenter"
  ) +
  theme(
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15))
  )

ggsave(
  "læring_hist.pdf",
  plot = læring_hist,
  height = 5,
  width = 6
)

# Tillid #

# Samlet
tillid_bar <-
  df |>
  mutate(
    Tillid = as_factor(Tillid),
    Tillid = fct_recode(
      Tillid,
      'Meget utroværdig' = '1',
      'Utroværdig' = '2',
      'Hverken troværdig eller utroværdig' = '3',
      'Troværdig' = '4',
      'Meget troværdig' = '5'
    )
  ) |>
  ggplot(aes(x = Tillid)) +
  geom_bar(
    aes(y = after_stat(prop), group = 1)
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Fordelingen af tillid i hele samplen, procent",
    x = "Hvor troværdig synes du, at den information du har fået fra [Treatment] er?, på en skala fra 1-5"
  ) +
  theme_simon(base_size = 12) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(1.5, "cm")
  ) +
  scale_x_discrete(labels = \(x) stringr::str_wrap(x, width = 5))


# Facetteret
tillid_bar_facet <-
  df |>
  mutate(
    Tillid = as_factor(Tillid),
    Tillid = fct_recode(
      Tillid,
      'Meget utroværdig' = '1',
      'Utroværdig' = '2',
      'Hverken troværdig eller utroværdig' = '3',
      'Troværdig' = '4',
      'Meget troværdig' = '5'
    )
  ) |>
  ggplot(aes(x = Tillid)) +
  geom_bar(
    aes(y = after_stat(prop), group = 1)
  ) +
  facet_wrap(~ treatment, axes = "all_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(labels = \(x) stringr::str_wrap(x, width = 5)) +
  labs(
    title = "Fordelingen af tillid til de to treatmentkilder, procent",
    x = "Hvor troværdig synes du, at den information du har fået fra [Treatment] er?, på en skala fra 1-5",
    caption = str_wrap(
      "Note: Respondenterne har svaret på spørgsmålet: 'Hvor troværdig synes du, at den information du har fået fra [Treatment] er?'",
      60)
  ) +
  theme_simon(base_size = 12) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(1.5, "cm"),
    plot.caption = element_text(margin = margin(t = 40))
  )


patch_tillid <- tillid_bar / tillid_bar_facet

ggsave("patch_tillid.pdf",
       plot = patch_tillid,
       height = 7,
       width = 7)


# Subjektiv forståelse
subjektiv_bar <-
  df |>
  mutate(
    subjektiv_forståelse = as_factor(subjektiv_forståelse),
    subjektiv_forståelse = fct_recode(
      subjektiv_forståelse,
      'Meget usikker' = '1',
      'Usikker' = '2',
      'Hverken sikker eller usikker' = '3',
      'Sikker' = '4',
      'Meget sikker' = '5'
    )
  ) |>
  ggplot(aes(x = subjektiv_forståelse)) +
  geom_bar(
    aes(y = after_stat(prop), group = 1)
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Fordelingen af subjektiv forståelse i hele samplen, procent"
  ) +
  theme_simon(base_size = 12) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(1.5, "cm")
  ) +
  scale_x_discrete(labels = \(x) stringr::str_wrap(x, width = 5))


# Facetteret
subjektiv_bar_facet <-
  df |>
  mutate(
    subjektiv_forståelse = as_factor(subjektiv_forståelse),
    subjektiv_forståelse = fct_recode(subjektiv_forståelse,
                        'Meget usikker' = '1',
                        'Usikker' = '2',
                        'Hverken sikker eller usikker' = '3',
                        'Sikker' = '4',
                        'Meget sikker' = '5'
    )
  ) |>
  ggplot(aes(x = subjektiv_forståelse)) +
  geom_bar(aes(y = after_stat(prop),
               group = 1)) +
  facet_wrap(~ treatment, axes = "all_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Fordelingen af subjektiv forståelse ved de to treatmentkilder, procent",
    caption = str_wrap(
      "Note: Respondenterne har svaret på spørgsmålet: 'Hvor sikker er du på, at dine nye placeringer er korrekte?'",
      60)
  ) +
  theme_simon(base_size = 12) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(1.5, "cm"),
    plot.caption = element_text(margin = margin(t = 40))
  ) +
  scale_x_discrete(labels = \(x) stringr::str_wrap(x, width = 5))

patch_subjektiv <- subjektiv_bar / subjektiv_bar_facet

ggsave("patch_subjektiv.pdf",
       plot = patch_subjektiv,
       height = 7,
       width = 7)


# Let's look at the political sophistication var
df_failed |>
  ggplot(aes(x = partier_folketing, y = after_stat(prop), group = 1)) +
  geom_bar() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(drop = FALSE) +
  theme_tufte(base_size = 14) +
  labs(title = "Andel af respondentsvar til, hvor mange sæder, der er i Folketinget", y = "Andel", x = "Svarmuligheder") +
  theme(
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)))

## SOURCE ##
# Source bar plot #
df_failed |>
  ggplot(aes(x = forcats::fct_infreq(source))) +
  geom_bar() +
  theme_simon(base_size = 14, ticks = FALSE) +
  labs(x = "Kilde", y = "Frekvens", title = "Antallet af spørgeskemabesvarelser fordelt på sourcing-kilde") +
  theme(
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15))
  ) +
  scale_x_discrete(labels = \(x) stringr::str_wrap(x, width = 15))

# Sophistication
sofistikation_bar <-
  df |>
  ggplot(aes(x = partier_folketing, y = after_stat(prop), group = 1)) +
  geom_bar(width = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(drop = FALSE) +
  theme_tufte(base_size = 14) +
  labs(title = str_wrap("Andel af respondentsvar til, hvor mange sæder, der er i Folketinget", 35),
                        y = "Andel", x = "Svarmuligheder",) +
  theme(
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)))


ggsave("appendix_a/sofistikation_bar.pdf",
       plot = sofistikation_bar,
       height = 5,
       width = 6)

## SOURCE_PROMPT ##

# Line chart #
# Df that includes the chat bot means for pre and post
means_df_2 <- df_failed |>
  group_by(source_prompt, treatment) |>
  summarise(
    mean_learning = mean(læring_total, na.rm = TRUE),
    .groups = "drop"
  )

# V2
facet_counts <- df_failed |> # Count n
  count(source_prompt)

facet_labels <- setNames( # Named vector for plotting
  paste0(facet_counts$source_prompt, "\n", "(n = ", scales::comma(facet_counts$n), ")"),
  facet_counts$source_prompt
)

sampling_viz <-
  df_failed |>
  ggplot(aes(x = StartDate_cph, y = læring_total, group = treatment)) +
  geom_point(aes(color = treatment, shape = treatment), size = 2, alpha = 0.35) +
  geom_hline(
    data = means_df_2,
    aes(yintercept = mean_learning, color = treatment),
    linewidth = 1, linetype = "longdash"
  ) +
    facet_wrap(
      ~source_prompt,
      scales = "free_x",
      labeller = labeller(
        source_prompt = function(x) {
          paste0(
            stringr::str_wrap(x, width = 20),   # wrap ONLY the title
            "\n(n = ", scales::comma(facet_counts$n[match(x, facet_counts$source_prompt)]), ")"
          )
        }
      )
    ) +
  labs(
    y = "Læring",
    x = "Tidspunkt / Dato",
    caption = str_wrap("Note: De stiplede linjer viser gennemsnittet af
                       læring for perioden inden for hvert treatment.
                       10 outliers er uden for det plottede område.", 50)
  ) +
  theme_simon(base_size = 12) +
  scale_color_manual(values = c(
    "chat bot" = "#000000",
    "artikel"  = "#A3A3A3"
  )) +
    scale_x_datetime(
      breaks = function(x) {
        rng <- range(x, na.rm = TRUE)
        rng[1] + diff(rng) * c(0.1, 0.5, 0.9)
      },
      date_labels = "%d-%m\n%H:%M",
      expand = expansion(mult = c(0.02, 0.08))
    ) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(limits = c(-1, 1.8)) +
  theme(
    panel.spacing = unit(1, "cm"),
    plot.margin = margin(10, 25, 10, 10),     # extra right export margin
    axis.title.x = element_text(margin = margin(t = 19)),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.ticks.length = unit(2.5, "pt"),
    legend.position = "bottom"
  )

ggsave("sampling.viz.pdf",
       plot = sampling_viz,
       width = 7,
       height = 7
)

#### Descriptives end ####
