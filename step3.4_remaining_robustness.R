### Created on: 26.04.28 ###
### Last edited: 26.04.28 ###

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
source("utils.R")

#### Load data ####
df_analysis <-
  read_rds("df_analysis.rds")

# Define the df with the failed interactions filtered and manipulation check
df_failed <- # THis becomes ITT
  df_analysis |>
  #  filter(Q8_1 == 0 | is.na(Q8_1)) |>
  #  filter(partier_folketing == "179") |>
  filter(Progress > 75) # Remove people who haven't done post-placements

# Define a df where cutoff is introduced
df_cutoff_filtered <-
  df_failed |>
  filter((treatment == "chat bot" & after_cutoff == "after" | treatment == "artikel"))

# Set df for the entire regression results viz section here
df <- df_cutoff_filtered # THIS IS THE ITT DF


#### ####

#### HYPOTHESIS 1 ####

# Test whether the two treatments actually taught them something statistically different from 0
df_article <- # Filtered to the article for t-test
  df |>
  filter(treatment == "artikel")

df_chat_bot <-
  df |>
  filter(treatment == "chat bot")


## Run hypothesis 1 regression without SF ##

robust_learning_reg <- lm(læring_robust_sf ~ treatment + pre_afstand_total, data = df)

# Export for joined H1 table
robust_learning_reg |>
  saveRDS("robust_learning_reg_sf.rds")

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

pred <- predict(robust_learning_reg, newdata = newdata_2, interval = "confidence")

pred_df <- bind_cols(newdata_2, as.data.frame(pred)) |>
  left_join(n_df, by = "treatment") # Join the n's onto the plot df


labels_vec <- setNames(
  paste0(pred_df$treatment, "\n(n = ", pred_df$n, ")"),
  pred_df$treatment
)

p_pred <-
  ggplot(pred_df, aes(x = treatment, y = fit)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.08) +
  geom_hline(yintercept = 0, alpha = 0.8) +
  scale_x_discrete(labels = labels_vec) +
  theme_simon(base_size = 12) +
  labs(title = "Forudsagt læring for de to treatmenttyper") +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    title = element_text(hjust = 0.5)
  )

p_pred

ggsave("sf_robusthed.pdf",
       plot = p_pred,
       height = 5,
       width = 6)

## Excluding everyone before prompt fix ##
exclude_robustness <-
  lm(læring_total ~ treatment + pre_afstand_total, data = df_failed |> filter(after_cutoff == "after"))

# Export for joined H1
exclude_robustness |>
  saveRDS("exclude_robustness.rds")

## Robustness - add sourcing period to reg ##

robust_learning_reg_source <- lm(læring_total ~ treatment + pre_afstand_total + source_prompt, data = df_failed)

# Export for joined H1 table
robust_learning_reg_source |>
  saveRDS("robust_learning_reg_source.rds")


# robust_learning_reg_source_2 <- lm(læring_total ~ treatment + pre_afstand_total, data = df_failed |> filter(source_prompt != "Før fix af prompt - personlige opslag"))


# Tæl de to grupper i regressionen
df_failed |>
  filter(source_prompt != "Før fix af prompt - personlige opslag") |>
  pull(treatment) |> table()


df_failed$source_prompt |> table()


# Check attention check influence
robust_learning_reg_source <- lm(læring_total ~ treatment + pre_afstand_total, data = df)

h1_attention_reg <- lm(læring_total ~ treatment + attention_check_dummy + pre_afstand_total, data = df)

# Export for joined h1 table
h1_attention_reg |>
  saveRDS("h1_attention_reg.rds")

attention_pred <- lm(attention_check_dummy ~ treatment + pre_afstand_total, data = df)

modelsummary(
  list(
    "Opmærksomhedstjek" = attention_pred
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
  gof_map = c(
    "nobs",
    "r.squared",
    "adj.r.squared"
  ),
  title = "Sandsynlighed for korrekt emneidentifikation",
  output = "regressions/attention_check.tex"
)


# Attention check source prompt
attention_check_2 <- lm(læring_total ~ treatment + pre_afstand_total + source_prompt + attention_check_dummy, data = df_failed)



