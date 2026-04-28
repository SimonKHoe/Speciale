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

#### Load data ####
df_analysis <-
  read_rds("df_analysis.rds")


# Define the df with the failed interactions filtered and manipulation check
df_failed <-
  df_analysis |>
  filter(Q8_1 == 0 | is.na(Q8_1)) |>
  filter(partier_folketing == "179")

# Define a df where cutoff is introduced
df_cutoff_filtered <-
  df_failed |>
  filter((treatment == "chat bot" & after_cutoff == "after" | treatment == "artikel"))

# Set df for the entire regression results viz section here
# df <- df_analysis
df <- df_cutoff_filtered
# df <- df_failed

#### ####


#### Descriptives ####

### PARTIES ###
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
    title = "Gennemsnitlig afstand i pre-placeringer pr. parti",
    x = "Parti",
    y = "Gennemsnitlig afstand til ekspertplacering"
  ) +
  theme_simon(base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  scale_y_continuous(breaks = seq(0, 2.5, by = 0.5), limits = c(0,2.8))


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
  )

# Plot
læring_bar <-
  ggplot(læring_summary, aes(x = parti |> fct_reorder(desc(mean_afstand)), y = mean_afstand)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_afstand - 1.96 * se,
                    ymax = mean_afstand + 1.96 * se),
                width = 0.2) +
  labs(
    title = "Gennemsnitlig læring pr. parti",
    x = "Parti",
    y = "Gennemsnitlig afstand til ekspertplacering"
  ) +
  theme_simon(base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )


# Patchwork
(pre_bar + post_bar) / læring_bar


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
  )

# Plot
# læring_bar_facet <-
#   ggplot(læring_summary, aes(x = fct_reorder(parti, mean_afstand, .desc = TRUE),
#                              y = mean_afstand)) +
#   geom_col() +
#   geom_errorbar(aes(ymin = mean_afstand - 1.96 * se,
#                     ymax = mean_afstand + 1.96 * se),
#                 width = 0.2) +
#   facet_wrap(~ treatment, axes = "all_y") +
#   labs(
#     title = "Gennemsnitlig læring pr. parti og treatment",
#     x = "Parti",
#     y = "Gennemsnitlig læring"
#   ) +
#   theme_simon(base_size = 14) +
#   theme(
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank(),
#     panel.spacing = unit(1.5, "cm")
#   )

#læring_dotwhisker_facet <-
  ggplot(
    læring_summary,
    aes(x = fct_reorder(parti, mean_afstand, .desc = TRUE),
        y = mean_afstand)
  ) +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = mean_afstand - 1.96 * se,
        ymax = mean_afstand + 1.96 * se),
    width = 0.1
  ) +
  geom_hline(yintercept = 0, alpha = 0.95) +
  facet_wrap(~ treatment, axes = "all_y") +
  labs(
    title = "Gennemsnitlig læring pr. parti og treatment",
    x = "Parti",
    y = "Gennemsnitlig læring"
  ) +
  theme_simon(base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(1.5, "cm")
  ) +
  scale_y_continuous(breaks = seq(-0.4, 1.4, by = 0.2))


### Variable ###

# Pre - placering #
# Delt op på treatment
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

# Spredning samlet
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

# Post - placering #
# Delt op på treatment
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

# Spredning samlet
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


# Manipulation check #
df |>
  ggplot(aes(x = Q9)) +
  geom_bar() +
  labs(
    title = "Fordelingen af svar til manipulationstjek, absolutte tal",
    x = "Hvad handlede din [treatment] primært om?"
    # ,caption = str_wrap(
    #   "Note: Respondenterne har svaret på spørgsmålet: 'Hvor troværdig synes du, at den information du har fået fra [Treatment] er?'",
    #   60)
  ) +
  theme_simon(base_size = 12) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(1.5, "cm")
  ) +
  scale_x_discrete(labels = \(x) stringr::str_wrap(x, width = 10))

# Chat bot issues #
df_long_cb <- df |>
  select(-Q8_5_TEXT) |>
  select(starts_with("Q8_")) |>
  pivot_longer(
    cols = everything(),
    names_to = "option",
    values_to = "selected"
  ) |>
  filter(!is.na(selected))


## BOXPLOT TREATMENTS ##

# Læring #
# visualize differences in learning using box plots
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

# Tillid #

# Samlet
tillid_bar <-
  df |>
  mutate(
    Tillid = as_factor(Tillid),
    Tillid = fct_recode(Tillid,
                        'Meget utroværdig' = '1',
                        'Utroværdig' = '2',
                        'Hverken troværdig eller utroværdig' = '3',
                        'Troværdig' = '4',
                        'Meget troværdig' = '5'
    )
  ) |>
  ggplot(aes(x = Tillid)) +
  geom_bar() +
  labs(
    title = "Fordelingen af tillid i hele samplen, absolutte tal",
    x = "Hvor troværdig synes du, at den information du har fået fra [Treatment] er?, på en skala fra 1-5"
    # ,caption = str_wrap(
    #   "Note: Respondenterne har svaret på spørgsmålet: 'Hvor troværdig synes du, at den information du har fået fra [Treatment] er?'",
    #   60)
  ) +
  theme_simon(base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(1.5, "cm")
  )


# Facetteret
tillid_bar_facet <-
  df |>
  mutate(
    Tillid = as_factor(Tillid),
    Tillid = fct_recode(Tillid,
                        'Meget utroværdig' = '1',
                        'Utroværdig' = '2',
                        'Hverken troværdig eller utroværdig' = '3',
                        'Troværdig' = '4',
                        'Meget troværdig' = '5'
    )
  ) |>
  ggplot(aes(x = Tillid)) +
  geom_bar() +
  facet_wrap(~ treatment, axes = "all_y") +
  scale_x_discrete(labels = \(x) stringr::str_wrap(x, width = 10)) +
  labs(
    title = "Fordelingen af tillid til de to treatmentkilder, absolutte tal",
    x = "Hvor troværdig synes du, at den information du har fået fra [Treatment] er?, på en skala fra 1-5",
    caption = str_wrap(
      "Note: Respondenterne har svaret på spørgsmålet: 'Hvor troværdig synes du, at den information du har fået fra [Treatment] er?'",
      60)
  ) +
  theme_simon(base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(1.5, "cm"),
    plot.caption = element_text(margin = margin(t = 40))
  )

tillid_bar / tillid_bar_facet

## PROMPT FIX ##

# RDD PLOT prompt fix #

# Df that includes the chat bot means for pre and post
# means_df <- df_analysis |>
#   mutate(after_cutoff = recode(after_cutoff,
#                                'before' = 'før prompt fix',
#                                'after' = 'efter prompt fix')) |>
#   group_by(after_cutoff, treatment) |>
#   summarise(
#     mean_learning = mean(læring_total, na.rm = TRUE),
#     .groups = "drop"
#   )

# # This can be run with df_analysis AND df_failed
# df_failed |> # Needs to be the df version with both pre and post cutoff
#   mutate(after_cutoff = recode(after_cutoff,
#                                'before' = 'før prompt fix',
#                                'after' = 'efter prompt fix')) |>
#   ggplot(aes(x = StartDate_cph, y = læring_total, group = treatment)) +
# #  geom_line(aes(color = treatment), linewidth = 0.8, alpha = 0.7) +
#   geom_point(aes(color = treatment, shape = treatment), size = 2, alpha = 0.8) +
#   #  geom_hline(yintercept = 0, linetype = "solid", linewidth = 1, alpha = 0.6) +
#   geom_hline(
#     data = means_df,
#     aes(yintercept = mean_learning, color = treatment),
#     linewidth = 1, linetype = "dashed"
#   ) +
#   facet_wrap(~after_cutoff, scales = "free_x") +
#   labs(y = "Læring", x = "Tidspunkt / Dato") +
#   theme_simon(base_size = 14) +
#   scale_color_manual(values = c(
#     "chat bot" = "#000000",
#     "artikel"  = "#ABABAB"
#   )) +
#   theme(panel.spacing = unit(1, "cm"),
#         axis.title.x = element_text(margin = margin(t = 15)),
#         axis.title.y = element_text(margin = margin(r = 15)))



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



## SOURCE_PROMPT ##

# Line chart #
# Df that includes the chat bot means for pre and post
means_df_2 <- df_failed |>
  group_by(source_prompt, treatment) |>
  summarise(
    mean_learning = mean(læring_total, na.rm = TRUE),
    .groups = "drop"
  )

# This can be run with df_analysis AND df_failed
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


#### Descriptives end ####
