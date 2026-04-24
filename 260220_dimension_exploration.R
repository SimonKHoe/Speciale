### Created on: 26.02.20 ###
### Last edited: 26.02.20 ###

# Libs
library(dplyr)
library(readr)
library(ggplot2)
library(forcats)
library(purrr)
library(stringr)

# Load data
chapel_hill <-
  read_csv("1999-2024_CHES_dataset_means.csv")

# Filter it to relevant dk and year and recode the climate change and environment variable
chapel_hill_filtered <-
  chapel_hill |>
  filter(country == 2) |> # Select DK
  filter(year == 2024) |> # Filter to 2024, as it is only there in 2024
  mutate(climate_env = (climate_change + environment) / 2) |>  # Divide it by 2 to normalize it back towards 1-10
  mutate(climate_env_salience = (climate_change_salience + enviro_salience) / 2)

# Table it to see how many years we're working with
chapel_hill_filtered |>
  select(year, climate_env) |>
  table()


# Pull a table of the parties with their variables before creation?
pre_mutate_variables_table <-
  chapel_hill_filtered |>
  select(party, climate_change, environment, climate_env)

### climate v. environment scatterplot NO COLOR
chapel_hill_filtered |>
  mutate(climate_change = climate_change - 5, # Rescale around 0
         environment = environment - 5) |>
  ggplot(aes(x = climate_change, y = environment, label = party)) +
  geom_point() +
  geom_text(vjust = -1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
  labs(
    x = "Klima 'venstre' (−)  ←  →  Klima 'højre' (+)",
    y = "Miljø venstre (−)  ←  →  Miljø højre (+)",
    title = "Danske partier i et todimensionelt klima/miljø rum 2024 (CHES-data)"
  ) +
  theme_minimal() +
  xlim(-5, 5) +
  ylim(-5, 5)

### climate v. environment scatterplot COLOR

# Color def
bloc_colors <- c(
  "S"  = "#BA0C2F",
  "SF" = "#BA0C2F",
  "M"  = "blue",
  "V"  = "blue",
  "EL" = "#BA0C2F",
  "DF" = "blue",
  "RV" = "#BA0C2F",
  "LA" = "blue",
  "NB" = "blue",
  "ALT"= "#BA0C2F",
  "DD" = "blue",
  "DKF" = "blue"
)

gov_colors <- c(
  "S"  = "purple",
  "SF" = "#BA0C2F",
  "M"  = "purple",
  "V"  = "purple",
  "EL" = "#BA0C2F",
  "DF" = "blue",
  "RV" = "#BA0C2F",
  "LA" = "blue",
  "NB" = "blue",
  "ALT"= "#BA0C2F",
  "DD" = "blue",
  "DKF" = "blue"
)

# Plot
climate_environment_plot <-
  chapel_hill_filtered |>
  filter(party != "NB") |>
  mutate(climate_change = climate_change - 5, # Rescale around 0
         environment = environment - 5) |>
  ggplot(aes(x = climate_change, y = environment, label = party)) +
  geom_point(aes(color = party), size = 2) +
  geom_text(vjust = -1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.8) +
  labs(
    x = "Klima venstre  ←  →  Klima højre",
    y = "Miljø venstre  ←  →  Miljø højre",
#    title = "Danske partier i et todimensionelt klima/miljø rum 2024 (CHES-data)",
    caption = str_wrap("Note: partierne er placeret ift. hvor meget de støtter beskyttelse af klimaet / miljøet,
                       selv på bekostning af økonomisk vækst. Jo lavere en score, jo mere villig er man til at
                       gå på kompromis med økonomisk vækst for at beskytte miljøet / klimaet.", width = 65)
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(
    axis.text = element_text(color = "grey15"),
    axis.title = element_text(color = "grey15"),
    plot.caption = element_text(color = "grey15", size = 7),
    axis.text.x = element_text(color = "grey15"),
    axis.text.y = element_text(color = "grey15")
  ) +
  xlim(-5, 5) +
  ylim(-5, 5) +
  scale_color_manual(values = bloc_colors)

# Export as pdf
climate_environment_plot |>
  ggsave(filename = "climate_environment_plot.pdf",
         width = 6,
         height = 6)

# It's only in the 2024 data
# Lets see if it's just salience that is captured?
chapel_hill_filtered |>
  mutate(climate_change_salience = climate_change_salience - 5, # Rescale around 0
         enviro_salience = enviro_salience - 5) |>
  ggplot(aes(x = climate_change_salience, y = enviro_salience, label = party)) +
  geom_point(aes(color = party), size = 2) +
  geom_text(vjust = -1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
  labs(
    x = "Klimasaliens lav  ←  →  Klimasaliens høj",
    y = "Miljøsaliens lav  ←  →  Miljøsaliens høj",
    title = "Danske partier i et todimensionelt rum for fokus på klima/miljø-politik 2024 (CHES-data)",
    caption = str_wrap("Note: partierne er placeret ift. hvor meget fokus (saliens), de ligger på hhv. klimapolitik og miljøpolitik", width = 85)
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(-5, 5) +
  ylim(-5, 5) +
  scale_color_manual(values = bloc_colors)

# Select only the parties respondents are placing
chapel_hill_filtered |>
  filter(party %in% c("S", "RV", "DKF", "SF", "EL", "DF")) |>
  select(party, climate_env, climate_env_salience, year)

