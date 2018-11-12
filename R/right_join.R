source(here::here("R/00_base_join.R"))

rj_joined_dfs <- right_join(x, y, "id") %>%
  proc_data("y", add_colnames = TRUE) %>%
  mutate(frame = 2, .x = .x + 1)

rj_extra_blocks <- inner_join(x, y, "id") %>%
  select(id) %>%
  proc_data("x", add_colnames = TRUE) %>%
  mutate(frame = 2, .x = .x + 1)

rj <- bind_rows(
  initial_join_dfs,
  rj_joined_dfs,
  rj_extra_blocks
) %>%
  filter(!is.na(value)) %>%
  mutate(
    .id = ifelse(label == "x", label, .id),
    removed = as.integer(grepl("3", value))
  ) %>%
  arrange(label, .id, .x, .y) %>%
  plot_data('right_join(x, y, by = "id")') %>%
  animate_plot()

rj <- animate(rj)
anim_save(here::here("images", "right-join.gif"), rj)

rj_g <- plot_data(rj_joined_dfs, 'right_join(x, y, by = "id")')
save_static_plot(rj_g, "right-join")
