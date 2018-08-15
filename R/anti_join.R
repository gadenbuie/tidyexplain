source(here::here("R/00_base.R"))

step2 <- initial_dfs %>%
  filter(.id == "x" | value %in% paste(1:2)) %>%
  mutate(
    frame = 2,
    .x = ifelse(.id == "y", 1, .x),
    .x = .x + 1
  )

step3 <- step2 %>%
  filter(grepl("3", value)) %>%
  ungroup() %>%
  mutate(frame = 3, .y = -1)

aj <- initial_dfs %>%
  mutate(removed = .id == "y", removed = as.integer(removed)) %>%
  bind_rows(step2, step3) %>%
  mutate(removed = ifelse(is.na(removed), 0, removed)) %>%
  arrange(removed, .y, .x, desc(frame)) %>% #View()
  mutate(alpha = case_when(
    grepl("3", value) ~ 1,
    frame == 2 & label == "id" ~ 0.25,
    frame == 2 ~ 0.65,
    TRUE ~ 1
  )) %>%
  {
    plot_data(., "anti_join(x, y)") +
      aes(alpha = alpha) +
      scale_alpha_identity()
  } %>%
  animate_plot()

aj <- animate(aj)
anim_save(here::here("images", "anti-join.gif"), aj)
