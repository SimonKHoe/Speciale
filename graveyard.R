### Created on: 26.04.20 ###
### Last edited: 26.04.20 ###

# Box plots
p1 <-
  conversation_table_joined |>
  group_by(conv_id) |>
  slice_max(turn_order, n = 1) |>
  ungroup() |>
  ggplot(aes(x = turn_order)) +
  geom_boxplot() +
  coord_flip() +
  scale_x_continuous(breaks = seq(3, 14, by = 2)) +
  theme_tufte() +
  labs(title = "Antal ture med chat bot") +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        title = element_text(hjust = 0.5)
  )


p2 <-
  df_analysis_hyp_2 |>
  ggplot(aes(x = conv_time_s)) +
  geom_boxplot() +
  coord_flip() +
  theme_tufte() +
  labs(title = "Samtalelængde i sekunder") +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        title = element_text(hjust = 0.5)
  )


p3 <-
  df_analysis_hyp_2 |>
  filter(treatment == "chat bot") |>
  ggplot(aes(x = n_chars)) +
  geom_boxplot() +
  coord_flip() +
  theme_tufte() +
  labs(title = "Antallet af anslag pr. interaktion") +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        title = element_text(hjust = 0.5)
  )


combined_boxplots <-
  p1 + p2 + p3
