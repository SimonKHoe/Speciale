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
library(broom)

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


### PLOT WITH ITT ###
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
summary(lm(læring_total ~ treatment, data = df))

# Pre-learning control
pre_learning_reg <- lm(læring_total ~ treatment + pre_afstand_total, data = df)
summary(pre_learning_reg)


# ANCOVA Robustness
summary(lm(post_afstand_total ~ treatment + pre_afstand_total, data = df))

# Mean sensecheck - descriptive statistics
mean(df_chat_bot$pre_afstand_total)
mean(df_article$pre_afstand_total)

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

pred <- predict(pre_learning_reg, newdata = newdata_2, interval = "confidence")

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
  theme_simon(base_size = 14) +
  labs(
    # title = "Forudsagt læring for de to treatmenttyper, kontrolleret for præ-placeringer",
    y = "Forudsagt læring") +
  theme(
    #    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    title = element_text(hjust = 0.5)
  )

p_pred

# Extract estimates
# ITT (ANCOVA / pre_learning_reg)
itt_est <- tidy(pre_learning_reg) |>
  filter(term == "treatmentchat bot") |>
  mutate(model = "ITT")

# LATE (IV)
late_est <- tidy(iv_model) |>
  filter(term == "engaged_chatbot_dummy") |>
  mutate(model = "LATE")

plot_df <- bind_rows(itt_est, late_est)

# Plot them together

ggplot(plot_df, aes(x = model, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error,
                    ymax = estimate + 1.96*std.error),
                width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_simon(base_size = 14) +
  labs(
    x = "",
    y = "Effekt på læring"
  )


