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
library(dotwhisker)
library(ggeffects)
library(ggthemes)
library(emmeans)
source("utils.R")

#### Load data ####
df_analysis <-
  read_rds("df_analysis.rds") |>
  mutate(conv_id = row_number())

# Define the df with the failed interactions
df_failed <- # THis becomes ITT
  df_analysis |>
  filter(Progress > 75) # Remove people who haven't done post-placements

# Define a df where cutoff is introduced
df_cutoff_filtered <-
  df_failed |>
  filter((treatment == "chat bot" & after_cutoff == "after" | treatment == "artikel")) |>
  mutate(conv_id = row_number())

# THIS IS THE ITT DF
df <- df_cutoff_filtered


#### HYPOTHESIS 1 ####
# Test whether the two treatments actually taught them something statistically different from 0
df_article <- # Filtered to the article for t-test
  df |>
  filter(treatment == "artikel")

df_chat_bot <-
  df |>
  filter(treatment == "chat bot")

## Article ##

# Test whether mean leraning is statistically differenct from 0
t.test(df_article$læring_total) # two sided t-test

## Chat bot ##
t.test(df_chat_bot$læring_total) # two sided t-test

# Two item t-test
t.test(læring_total ~ treatment, data = df)

# Bivariate
h1_bivariate <- lm(læring_total ~ treatment, data = df)

# Export for joined h1 table
h1_bivariate |>
  saveRDS("h1_bivariate.rds")

# Pre-learning control
h1_pre_learning_reg <- lm(læring_total ~ treatment + pre_afstand_total, data = df)

# Export for joined h1 table
h1_pre_learning_reg |>
  saveRDS("h1_pre_learning_reg.rds")

summary(h1_pre_learning_reg)

# Create modelsummary for the two
modelsummary(
  list(
    "Bivariat" = h1_bivariate,
    "Kontrol" = h1_pre_learning_reg
  ),
  stars = TRUE,
  fmt = 3,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map = c(
    "(Intercept)" = "Konstant",
    "treatmentchat bot" = "Chatbot",
    "pre_afstand_total" = "Præ-treatment afstand"
  ),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  output = "regressions/h1_regression.tex"
)


# ANCOVA Robustness
summary(lm(post_afstand_total ~ treatment + pre_afstand_total, data = df))

# Mean sensecheck - descriptive statistics
mean(df_chat_bot$pre_afstand_total)
mean(df_article$pre_afstand_total)
mean(df_chat_bot$post_afstand_total)
mean(df_article$post_afstand_total)

## Plot and regression pipe ##

# Count n observations pr. treatment in the df
n_df <- df |>
  group_by(treatment) |>
  summarise(n = n(), .groups = "drop")

# Marginal means plot
newdata_2 <- data.frame(
  treatment = c("artikel", "chat bot"),
  pre_afstand_total = mean(df$pre_afstand_total, na.rm = TRUE)
)

pred <- predict(h1_pre_learning_reg, newdata = newdata_2, interval = "confidence")

pred_df <- bind_cols(newdata_2, as.data.frame(pred)) |>
  left_join(n_df, by = "treatment") # Join the n's onto the plot df


labels_vec <- setNames(
  paste0(pred_df$treatment, "\n(n = ", pred_df$n, ")"),
  pred_df$treatment
)

p_pred <-
  ggplot(pred_df, aes(x = treatment, y = fit, shape = treatment)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.08) +
  geom_hline(yintercept = 0, alpha = 0.8) +
  scale_x_discrete(labels = labels_vec) +
  scale_shape_manual(
    values = c("artikel" = 16, "chat bot" = 17),
    guide = "none"
  ) +
  theme_simon(base_size = 12) +
  labs(y = "Forudsagt læring") +
  theme(
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    title = element_text(hjust = 0.5)
  )

p_pred

ggsave("h1_læring_plot.pdf",
       plot = p_pred,
       width = 6, height = 5)



#### HYPOTHESIS 3 ####

# LM for trust?
h3_trust_outcome <- lm(Tillid ~ treatment, data = df)
summary(h3_trust_outcome)

# Export the LM for trust
modelsummary(
  list(
    "Tillid" = h3_trust_outcome
  ),
  stars = TRUE,
  fmt = 3,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map = c(
    "(Intercept)" = "Konstant",
    "treatmentchat bot" = "Chatbot"
  ),
  title = "Tillid forklaret ved treatment",
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  output = "regressions/h3_trust_outcome.tex"
)

# There is a significant difference in trust between the two information sources

# No trust (baseline model)
h3_baseline <- lm(læring_total ~ treatment + pre_afstand_total, data = df)

# Trust explain learning?
h3_trust_only <- lm(læring_total ~  Tillid + pre_afstand_total, data = df)
summary(h3_trust_only) # Without treatment, trust explains learning

# What happens then if we control for trust with learning?
h3_trust_main <- lm(læring_total ~ treatment + Tillid + pre_afstand_total, data = df)
summary(h3_trust_main)

# Trust doees not sap the difference between the two treatments, but it looks like it could have an effect

# Trust explain learning for CB vs. artikel
h3_chat_bot <- lm(læring_total ~ Tillid + pre_afstand_total, data = df |> filter(treatment == "chat bot"))
h3_article <- lm(læring_total ~ Tillid + pre_afstand_total, data = df |> filter(treatment == "artikel"))

# Interaktion
h3_interaktion <- lm(læring_total ~ Tillid * treatment + pre_afstand_total, data = df)
summary(h3_interaktion)

# MODELSUMMARY THE REGRESSIONS #
modelsummary(
  list(
    "Baseline" = h3_baseline,
    "Tillid" = h3_trust_only,
    "Tillid + treatment" = h3_trust_main
  ),
  stars = TRUE,
  fmt = 3,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map = c(
    "(Intercept)" = "Konstant",
    "treatmentchat bot" = "Chatbot",
    "Tillid" = "Tillid",
    "pre_afstand_total" = "Præ-treatment afstand",
    "Tillid:treatmentchat bot" = "Tillid × chatbot"
  ),
  title = "Tillid og læring",
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  output = "regressions/h3_regressions_main.tex"
)

modelsummary(
  list(
    "Chatbotbrugere" = h3_chat_bot,
    "Artikelbrugere" = h3_article,
    "Tillid × treatment" = h3_interaktion
  ),
  stars = TRUE,
  fmt = 3,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map = c(
    "(Intercept)" = "Konstant",
    "treatmentchat bot" = "Chatbot",
    "Tillid" = "Tillid",
    "pre_afstand_total" = "Præ-treatment afstand",
    "Tillid:treatmentchat bot" = "Tillid × chatbot"
  ),
  title = "Tillid og læring, splittet på treatments og interaktion",
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  output = "regressions/h3_regressions_split.tex"
)


# ANCOVA Robustness
summary(lm(post_afstand_total ~ treatment + Tillid + pre_afstand_total, data = df))

#### HYPOTESE 4 ####

h4_bivariate <- lm(post_viden ~ subjektiv_forståelse, data = df)
summary(h4_bivariate)
# There is a relationship between how much you think you understand and how much you actually understand

h4_chat_bot <- lm(post_viden ~ subjektiv_forståelse, data = df |> filter(treatment == "chat bot"))
summary(h4_chat_bot)

h4_article <- lm(post_viden ~ subjektiv_forståelse, data = df |> filter(treatment == "artikel"))
summary(h4_article)

# The relationship is only existant for the article  - try interaction
h4_interaction <- lm(post_viden ~ subjektiv_forståelse * treatment, data = df)
summary(h4_interaction)

# Plot the interaction

pred_interaction_h4 <- ggpredict(
  h4_interaction,
  terms = c("subjektiv_forståelse [all]", "treatment")
)

h4_interaktion <-
  ggplot(pred_interaction_h4,
         aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.04, color = NA) +
  geom_segment(
    aes(
      x = min(pred_interaction_h4$x),
      xend = max(pred_interaction_h4$x),
      y = mean(df$post_viden, na.rm = TRUE),
      yend = mean(df$post_viden, na.rm = TRUE)
    ),
    linetype = "dashed",
    colour = "black",
    alpha = 0.3
  ) +
  labs(
    x = "Subjektiv forståelse",
    y = "Forudsagt post-viden (ud fra afstand)",
    color = "Informationskilde",
    fill = "Informationskilde",
    caption = str_wrap(
      "Note: Forudsagte værdier baseret på lineær regression med 95% konfidensintervaller. Den stiplede linje indikerer det gennemsnitlige vidensniveau i samplen.",
      width = 45
    )
  ) +
  scale_color_manual(
    values = c("#0072B2", "#D55E00"),
    labels = c("Artikel", "Chat bot")
  ) +
  scale_fill_manual(
    values = c("#0072B2", "#D55E00"),
    labels = c("Artikel", "Chat bot")
  ) +
  theme_simon(base_size = 12) +
  theme(
    plot.caption = element_text(margin = margin(t = 25)),
    legend.position = "bottom"
  )

# Export the interaction plot
ggsave("h4_interaktion.pdf",
       plot = h4_interaktion,
       width = 6,
       height = 6
       )


### MODELSUMMARY OF THE REGRESSIONS ###
modelsummary(
  list(
    "Bivariat" = h4_bivariate,
    "Chatbotbrugere" = h4_chat_bot,
    "Aritkelbrugere" = h4_article,
    "Interaktion" = h4_interaction
  ),
  stars = TRUE,
  fmt = 3,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map = c(
    "(Intercept)" = "Konstant",
    "subjektiv_forståelse" = "Subjektiv forståelse",
    "treatmentchat bot" = "Chatbot",
    "subjektiv_forståelse:treatmentchat bot" = "Subjektiv forståelse × chatbot"
  ),
  title = "Sammenhængen mellem subjektiv forståelse og viden",
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  output = "regressions/h4_regressions.tex"
)

# Do chat bot users overestimate theselves?
df$overconfidence <- scale(df$subjektiv_forståelse) - scale(df$post_viden)

h4_overconfidence <- lm(overconfidence ~ treatment, data = df)

## EXPORT THE REG ##

modelsummary(
  list(
    "Overkonfidens" = h4_overconfidence
  ),
  stars = TRUE,
  fmt = 3,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map = c(
    "(Intercept)" = "Konstant",
    "treatmentchat bot" = "Chatbot"
  ),
  title = "Sammenhængen mellem overkonfidens og treatment",
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  output = "regressions/h4_overkonfidens.tex"
)

# Dumbbell plot #
dumbbell_plot_h4 <-
  df |>
  select(z_subjektiv_forståelse, z_post_viden, treatment) |>
  group_by(treatment) |>
  summarise(
    mean_z_subjektiv_forståelse = mean(z_subjektiv_forståelse),
    mean_z_post_viden = mean(z_post_viden)
  ) |>
  ungroup() |>
  ggplot() +

  geom_segment(
    aes(
      x = mean_z_post_viden,
      xend = mean_z_subjektiv_forståelse,
      y = treatment,
      yend = treatment
    )
  ) +

  geom_point(
    aes(
      x = mean_z_post_viden,
      y = treatment,
      color = "Post-viden",
      shape = treatment
    ),
    size = 4
  ) +

  geom_point(
    aes(
      x = mean_z_subjektiv_forståelse,
      y = treatment,
      color = "Subjektiv forståelse",
      shape = treatment
    ),
    size = 4
  ) +

  scale_color_manual(
    name = NULL,
    values = c(
      "Post-viden" = "#A3A3A3",
      "Subjektiv forståelse" = "black"
    )
  ) +

  scale_shape_manual(
    name = NULL,
    values = c(
      "artikel" = 16,
      "chat bot" = 17
    ),
    labels = c(
      "artikel" = "Artikel",
      "chat bot" = "Chatbot"
    )
  ) +

  labs(x = "Z-Standardiseret niveau",
#       y = "Treatment",
       caption = str_wrap("Note: Højere værdier angiver relativt højere subjektiv forståelse eller
       post_viden sammenlignet med samplets gennemsnit", 45)
       ) +
  theme_simon(base_size = 12, ticks = FALSE) +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.spacing.y = unit(0, "cm"),
        plot.caption = element_text(margin = margin(t = 15)),
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_blank()
        ) +
    guides(
      color = guide_legend(
        order = 1,
        override.aes = list(shape = 15, size = 3.5)
      ),
      shape = guide_legend(
        order = 2,
        override.aes = list(color = "black", size = 3.5)
      )
    )

ggsave("dumbbell_plot_h4.pdf",
       plot = dumbbell_plot_h4,
       height = 5,
       width = 6)


# Test whether over/underconfidence can be attributed to each group
# Create z_subjektiv_post
df_h4 <-
  df |>
  mutate(z_subjektiv_post = z_subjektiv_forståelse - z_post_viden)


t.test(
  df_h4$z_subjektiv_post[df_h4$treatment == "artikel"],
  mu = 0
)

t.test(
  df_h4$z_subjektiv_post[df_h4$treatment == "chat bot"],
  mu = 0
)


#### HYPOTESE 5 ####

# Let's look at the political sofistication var - I'm expecting it to be useless
summary(lm(læring_total ~ partier_folketing + pre_afstand_total, data = df_analysis))

