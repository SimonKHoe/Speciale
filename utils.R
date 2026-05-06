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

# Define my own ggplot theme

theme_simon <- function(base_size = 12, caption_size = 10, ticks = TRUE) {
  theme_tufte(base_size = base_size, ticks = ticks) +
    theme(
      # Sæt ALT tekst til sort
      text = element_text(color = "black"),

      # Eksplicit (for at undgå overrides fra theme_tufte)
      axis.text   = element_text(color = "black"),
      axis.title  = element_text(color = "black"),
      plot.title  = element_text(color = "black"),
      plot.subtitle = element_text(color = "black"),
      plot.caption = element_text(color = "black", size = caption_size),
      strip.text  = element_text(color = "black"),
      legend.text = element_text(color = "black"),
      legend.title = element_text(color = "black"),

      # Margins
      axis.title.x = element_text(margin = margin(t = 15)),
      axis.title.y = element_text(margin = margin(r = 15))
    )
}




