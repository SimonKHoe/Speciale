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



##### SANDBOX #####
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
  theme_simon() +
  scale_color_manual(values = c(
    "chat bot" = "#000000",
    "artikel"  = "#A3A3A3"
  )) +
  theme(panel.spacing = unit(1, "cm"),
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.ticks.length = unit(2.5, "pt")) +
  scale_x_datetime(
    date_labels = "%d-%m %H:%M",
    guide = guide_axis(n.dodge = 2, check.overlap = TRUE)
  )
