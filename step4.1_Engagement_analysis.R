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

# THIS IS THE ITT DF
# df <- df_analysis
df <- df_cutoff_filtered
# df <- df_failed


# THIS DEFINES THE FILTERED/LATE DF

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

df_hyp_2 <- # Left joins max turns back onto OG df
  df |>
  left_join(max_turn)

# Turn df_hyp_2 (joined max_turns) into the new df
df <- ### THIS IS THE FILTER DF - ENGAGEMENT V. ENGAGEMENT
  df_hyp_2 |>
  filter(max_turn != 1 | is.na(max_turn)) |>  # Now we can filter out 1-turn chat bot interactions by keeping bigger than 1 or NA (article)
  filter(Q40_time_Page_Submit > 30 | is.na(Q40_time_Page_Submit))

  #### ####

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
  theme_tufte(base_size = 14) +
  labs(title = "Forudsagt læring for de to treatmenttyper") +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    title = element_text(hjust = 0.5)
  )

p_pred

## Robustness ##



#### HYPOTHESIS 3 ####

# Trust
df |>
  ggplot(aes(x = Tillid, y = læring_total)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  facet_wrap(~treatment)

# LM for trust?
reg_trust <-
  lm(Tillid ~ treatment, data = df_analysis)

# There is a significant difference in trust between the two information sources

summary(reg_trust) # More trust for the article

# No trust (baseline model)
summary(lm(læring_total ~ treatment + pre_afstand_total, data = df))

# What happens then if we control for trust with learning?
summary(lm(læring_total ~ treatment + Tillid + pre_afstand_total, data = df))

# Trust DOES sap the difference between the two treatments, but it looks like it could have an effect

# Trust explain learning?
summary(lm(læring_total ~ Tillid + pre_afstand_total, data = df)) # Without treatment, trust explains learning

# Trust explain learning for CB vs. artikel
summary(lm(læring_total ~ Tillid + pre_afstand_total, data = df |> filter(treatment == "chat bot")))
summary(lm(læring_total ~ Tillid + pre_afstand_total, data = df |> filter(treatment == "artikel")))

# Interaktion
summary(lm(læring_total ~ Tillid * treatment + pre_afstand_total, data = df))
# We lack power for the interaction - but could also be that it isn't there


# ANCOVA Robustness
summary(lm(post_afstand_total ~ treatment + Tillid + pre_afstand_total, data = df))

# Interaction
summary(lm(læring_total ~ treatment*Tillid + pre_afstand_total, data = df))
# The difference between the treatments isn't conditioned by the level of trust, it is trust itself that has effect


# What happens if we only regress Learning on trust for chat bot
summary(lm(læring_total ~ Tillid, data = df |> filter(treatment == "chat bot")))



#### HYPOTESE 4 ####

summary(lm(post_viden ~ subjektiv_forståelse, data = df))
# There is a relationship between how much you think you understand and how much you actually understand

summary(lm(post_viden ~ subjektiv_forståelse, data = df |> filter(treatment == "chat bot")))

summary(lm(post_viden ~ subjektiv_forståelse, data = df |> filter(treatment == "artikel")))

# The relationship is stronger for the article but is positive and significant in both

summary(lm(post_viden ~ subjektiv_forståelse * treatment, data = df))

summary(lm(læring_total ~ subjektiv_forståelse, data = df |> filter(treatment == "artikel")))

# Visualize the correlation facetted
df |>
  ggplot(aes(y = post_viden, x = subjektiv_forståelse, groups = treatment)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_tufte(base_size = 14) +
  facet_wrap(~treatment, scales = "free_x")



# Exploration
df_h4 <-
  df |>
  mutate(z_subjektiv_post = z_subjektiv_forståelse - z_post_viden)

# Viz
df_h4 |>
  ggplot(aes(y = z_subjektiv_post, x = treatment)) +
  geom_col()

# Post knowledge and Subjective understanding
summary(lm(z_subjektiv_forståelse ~ z_post_viden * treatment, data = df))

# Learning and subjective understanding
df |>
  ggplot(aes(x = z_subjektiv_forståelse, y = læring_total)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  facet_wrap(~treatment)


summary(lm(læring_total ~ subjektiv_forståelse + pre_afstand_total, data = df |> filter(treatment == "chat bot")))

summary(lm(læring_total ~ subjektiv_forståelse + pre_afstand_total, data = df |> filter(treatment == "artikel")))

# The relationship between subjective understanding and learning exists for article but not chat bot

# Difference between post-knowledge and subjective understanding

summary(lm(z_subjektiv_post ~ treatment, data = df_h4))


#### HYPOTESE 5 ####

# Let's look at the political sofistication var - I'm expecting it to be useless
df |>
  ggplot(aes(x = partier_folketing, y = after_stat(prop), group = 1)) +
  geom_bar() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(drop = FALSE) +
  theme_tufte(base_size = 14) +
  labs(title = "Andel af respondentsvar til, hvor mange sæder, der er i Folketinget", y = "Andel", x = "Svarmuligheder") +
  theme(
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)))

# It's useless, let's try to have a look at it anyway

summary(lm(læring_total ~ partier_folketing + pre_afstand_total, data = df_analysis))

