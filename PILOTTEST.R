### Created on: 26.04.22 ###
### Last edited: 26.04.22 ###

# Setup
library(dplyr)
library(readr)
library(ggplot2)
library(forcats)
library(purrr)
library(stringr)
library(haven)
library(labelled)

# Load data
df_pilot <-
  read_sav("pilot.sav") |>
  filter(!is.na(Q11))


# Let's pull duration
df_pilot |>
  ggplot(aes(x = Duration__in_seconds_)) +
  geom_bar(width = 20)

df_pilot |>
  ggplot(aes(x = Duration__in_seconds_)) +
  geom_boxplot() +
  theme_tufte(base_size = 14) +
  scale_x_continuous(limits = c(200, 1650), breaks = seq(from = 200, to = 1600, by = 100)) +
  labs(title = "Boxplot for piloten", x = "Tid brugt i sekunder") +


df_cutoff_filtered |>
  ggplot(aes(x = Duration__in_seconds_)) +
  geom_boxplot() +
  theme_tufte(base_size = 14) +
  scale_x_continuous(limits = c(200, 1650), breaks = seq(from = 200, to = 1600, by = 100)) +
  scale_x_continuous(limits = c(200, 1650), breaks = seq(from = 200, to = 1600, by = 100)) +
  labs(title = "Boxplot for eksperiment", x = "Tid brugt i sekunder")

