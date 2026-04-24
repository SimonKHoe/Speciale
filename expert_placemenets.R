### Created on: 011326 ###
### Last edited: 011326 ###

# Libs
library(dplyr)
library(readr)
library(ggplot2)
library(forcats)
library(purrr)

# Load data
chapel_hill <-
  read_csv("1999-2024_CHES_dataset_means.csv")


# Filter to DK and newest

chapel_hill_filtered <-
  chapel_hill |>
  filter(country == 2) |> # This selects DK
  filter(electionyear == 2022) # This picks the latest election year, 2022


## Viz

# LRgen scatterplot
chapel_hill_filtered |>
  ggplot(aes(x = fct_reorder(party, lrgen), y = lrgen)) +
  geom_point() +
  theme_minimal()

# galtan ecogen scatterplot
chapel_hill_filtered |>
  mutate(lrecon = lrecon - 5, # Rescale around 0
         galtan = galtan - 5) |>
  ggplot(aes(x = lrecon, y = galtan, label = party)) +
  geom_point() +
  geom_text(vjust = -1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
  labs(
    x = "Økonomisk venstre (−)  ←  →  Økonomisk højre (+)",
    y = "GAL (−)  ←  →  TAN (+)",
    title = "Danske partier i todimensionelt policy-rum, 2024 (CHES-data)"
  ) +
  theme_minimal() +
  xlim(-5, 5) +
  ylim(-5, 5)

# Selection process illustrated

# Coordinates of Venstre and Soc Dem for illustration
chapel_hill_rescale <-
  chapel_hill_filtered |>
  mutate(lrecon = lrecon - 5, # Rescale around 0
         galtan = galtan - 5)

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


# Create coords tibble for S illlustration
lines_df <- tibble(
  x = chapel_hill_rescale$lrecon[chapel_hill_rescale$party == "S"],
  y = chapel_hill_rescale$galtan[chapel_hill_rescale$party == "S"],
  xend = chapel_hill_rescale$lrecon[chapel_hill_rescale$party %in% c("M","SF","V")],
  yend = chapel_hill_rescale$galtan[chapel_hill_rescale$party %in% c("M","SF","V")]
  ) |>
  mutate(
    mid_x = (x + xend) / 2,
    mid_y = (y + yend) / 2,
    label = c("2", "3", "1")  # whatever text you want
  )


# BLOC colors mechanism
bloc_mechanism <-
  chapel_hill_rescale |>
  ggplot(aes(x = lrecon, y = galtan, label = party)) +
  geom_point(aes(color = party), size = 2) +
  geom_text(aes(color = party), vjust = -1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_segment(
    data = lines_df,
    aes(x = x, y = y, xend = xend, yend = yend),
    color = c("grey", "green", "grey"),  # directly assign
    size = 1,
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = lines_df,
    aes(x = mid_x, y = mid_y, label = label),
    color = c("grey10", "grey10", "grey10"),
    inherit.aes = FALSE,
    vjust = -1,
    size = 3,
    alpha = 1
  ) +
  scale_color_manual(values = bloc_colors) +
  labs(
    x = "Økonomisk venstre (−)  ←  →  Økonomisk højre (+)",
    y = "GAL (−)  ←  →  TAN (+)",
    title = "De tre tætteste partier på S i et todimensionelt policy-rum, 2024"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(-5, 5) +
  ylim(-5, 5)

# GOV colors mechanism
gov_mechanism <-
  chapel_hill_rescale |>
  ggplot(aes(x = lrecon, y = galtan, label = party)) +
  geom_point(aes(color = party), size = 2) +
  geom_text(aes(color = party), vjust = -1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_segment(
    data = lines_df,
    aes(x = x, y = y, xend = xend, yend = yend),
    color = c("grey", "green", "grey"),  # directly assign
    size = 1,
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = lines_df,
    aes(x = mid_x, y = mid_y, label = label),
    color = c("grey10", "grey10", "grey10"),
    inherit.aes = FALSE,
    vjust = -1,
    size = 3,
    alpha = 1) +
  scale_color_manual(values = gov_colors) +
  labs(
    x = "Økonomisk venstre (−)  ←  →  Økonomisk højre (+)",
    y = "GAL (−)  ←  →  TAN (+)",
    title = "De tre tætteste partier på S i et todimensionelt policy-rum, 2024",
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(-5, 5) +
  ylim(-5, 5)

# Export the two mech plots
bloc_mechanism |>
  ggsave(filename = "bloc_mechanism.pdf", width = 7, height = 7)

gov_mechanism |>
  ggsave(filename = "gov_mechanism.pdf", height = 7, width = 7)

## Calculations of 2nd and 3 closest parties in the 2-dimensional space

# Prepare df to calcs
df_filter_2 <-
  chapel_hill_filtered |>
  select(party, galtan, lrecon) |>
  mutate(id = row_number())


# This is the matrix df that gets distance for each pair
cross <-
  cross_join(x = df_filter_2, y = df_filter_2, suffix = c("_i", "_j")) |> # Cross join to get pairwise combinations for each party
#  filter(id_i < id_j) |> # This removes duplicates
#  filter(party_i != party_j) |> # This removes same party matches
  mutate(distance = sqrt((galtan_i-galtan_j)^2 + (lrecon_i - lrecon_j)^2)) |>  # Calculate the distance for the party-pairs
  filter(distance != 0) |> # removes same party pairings
  select(-id_i, -id_j) # Get rid of id's, no longer needed

# Viz

# 2, 3 and 4 for each party in the 2-dimensional space

party_colors <- c(
  "S"   = "red",
  "V"   = "darkblue",
  "DKF" = "darkgreen",
  "SF"  = "#ffb3b3",
  "EL"  = "darkred",
  "RV"  = "purple",
  "ALT" = "green",
  "M"   = "#660066",
  "DF"  = "yellow",
  "LA"  = "#00ccff",
  "NB"  = "grey10",
  "DD"  = "#006699"
)


# Create the tibble for drawing the lines
all_lines_df <-
  cross |>
  group_by(party_i) |>
  arrange(distance) |>
  slice(2:4) |>            # 2nd, 3rd, 4th closest
  ungroup()

# only 1 placement is needed pr. party for the scatter
points_df <-
  cross |>
  select(party = party_i, galtan = galtan_i, lrecon = lrecon_i) |>
  distinct()

# Plot all parties with lines to 2, 3 and 4
scatter_2_4 <-
  ggplot(points_df, aes(x = lrecon, y = galtan)) +
  geom_point(aes(color = party), size = 2) +
  geom_curve(
    data = all_lines_df,
    aes(
      x = lrecon_i, y = galtan_i,
      xend = lrecon_j, yend = galtan_j,
      color = party_i          # <- map line color to originating party
    ),
    curvature = 0.15,
    alpha = 0.6,
    size = 0.8
  ) +
  scale_color_manual(values = party_colors) +
  labs(
    x = "Økonomisk venstre  ←  →  Økonomisk højre",
    y = "GAL  ←  →  TAN",
    title = "Partier i 2D policy-rum med forbindelser til 2., 3. og 4. nærmeste parti"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(1,9) +
  ylim(1,9) +
  geom_hline(yintercept = 5, linetype = "dashed", alpha = 0.6, color = "grey") +
  geom_vline(xintercept = 5, linetype = "dashed", alpha = 0.6, color = "grey") +
  geom_text(aes(label = party, color = party), vjust = -1.2, size = 5)

# Export as pdf
scatter_2_4 |>
  ggsave(filename = "scatter_2_4.pdf", width = 6.5, height = 6.5)

# Divide it by block colors
bloc_scatter_2_4 <-
  ggplot(points_df, aes(x = lrecon, y = galtan)) +
  geom_point(aes(color = party), size = 2) +
  geom_abline(
    intercept = 9,
    slope = -1,
    linetype = "dashed",
    color = "black",
    alpha = 0.8,
    size = 1.1
  ) +
  geom_curve(
    data = all_lines_df,
    aes(
      x = lrecon_i, y = galtan_i,
      xend = lrecon_j, yend = galtan_j,
      color = party_i          # <- map line color to originating party
    ),
    curvature = 0.15,
    alpha = 0.6,
    size = 0.8
  ) +
  scale_color_manual(values = bloc_colors) + # use block colors
  labs(
    x = "Økonomisk venstre  ←  →  Økonomisk højre",
    y = "GAL  ←  →  TAN",
    title = "Partier i 2D policy-rum med forbindelser til 2., 3. og 4. nærmeste parti"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(1,9) +
  ylim(1,9) +
  geom_hline(yintercept = 5, linetype = "dashed", alpha = 0.6, color = "grey") +
  geom_vline(xintercept = 5, linetype = "dashed", alpha = 0.6, color = "grey") +
  geom_text(aes(label = party, color = party), vjust = -1.2, size = 5)

# Export som pdf
bloc_scatter_2_4 |>
  ggsave(filename = "bloc_scatter_2_4.pdf", width = 6.5, height = 6.5)


# Divide it by gov colors
gov_scatter_2_4 <-
  ggplot(points_df, aes(x = lrecon, y = galtan)) +
  geom_point(aes(color = party), size = 2) +
  # geom_abline(
  #   intercept = 12,
  #   slope = -1,
  #   linetype = "dashed",
  #   color = "black",
  #   alpha = 0.8,
  #   size = 1.1
  # ) +
  #   geom_abline(
  #   intercept = 7.7,
  #   slope = -1,
  #   linetype = "dashed",
  #   color = "black",
  #   alpha = 0.8,
  #   size = 1.1
  # ) +
  geom_curve(
    data = all_lines_df,
    aes(
      x = lrecon_i, y = galtan_i,
      xend = lrecon_j, yend = galtan_j,
      color = party_i          # <- map line color to originating party
    ),
    curvature = 0.15,
    alpha = 0.7,
    size = 0.8
  ) +
  scale_color_manual(values = gov_colors) + # use block colors
  labs(
    x = "Økonomisk venstre  ←  →  Økonomisk højre",
    y = "GAL  ←  →  TAN",
    title = "Partier i 2D policy-rum med forbindelser til 2., 3. og 4. nærmeste parti"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(1,9) +
  ylim(1,9) +
  geom_hline(yintercept = 5, linetype = "dashed", alpha = 0.6, color = "grey") +
  geom_vline(xintercept = 5, linetype = "dashed", alpha = 0.6, color = "grey") +
  geom_text(aes(label = party, color = party), vjust = -1.2, size = 5)

# Export som pdf
gov_scatter_2_4 |>
  ggsave(filename = "gov_scatter_2_4.pdf", width = 6.5, height = 6.5)

# Socialdemokratiet
cross |>
  filter(party_i == "S") |>
  ggplot(aes(x = fct_reorder(party_j, distance), y = distance)) +
  geom_point() +
  theme_minimal()


