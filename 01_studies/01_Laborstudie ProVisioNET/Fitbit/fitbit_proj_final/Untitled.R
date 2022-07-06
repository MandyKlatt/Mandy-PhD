df_heart_all %>% 
  clean_names() %>% 
  group_by(id, type) %>% 
  fill() %>% 
  mutate(heart_rate = na_kalman(heart_rate),
         heart_rate = as.numeric(heart_rate),
         time = str_remove_all(time, "x"),
         time = as.numeric(time)) %>% 
  ungroup() %>%
  ggplot(mapping = aes(x = time,
                         y = heart_rate,
                       group = id) 
    ) +
    geom_line(
    ) +
    labs(x = "Time (in Seconds)",
         y = "Heart Rate\n(in Beats per Minute)") +
    theme_minimal() +
    theme(text = element_text(family = "serif"),
          legend.position = "none",
          strip.text = element_text(size = 16)) +
  facet_grid(cols = vars(type))

