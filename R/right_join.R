source(here::here("R/00_base.R"))

joined_dfs <- right_join(x, y, "id") %>%
  proc_data("y") %>%
  mutate(frame = 2, .x = .x + 1)

extra_blocks <- inner_join(x, y, "id") %>%
  select(id) %>%
  proc_data("x") %>%
  mutate(frame = 2, .x = .x + 1)

rj <- initial_dfs %>%
  bind_rows(joined_dfs, extra_blocks) %>%
  filter(!is.na(value)) %>%
  mutate(
    .id = ifelse(label == "x", label, .id),
    removed = as.integer(grepl("3", value))
  ) %>%
  arrange(removed, .id, frame, value) %>%
  plot_data("right_join(x, y)") %>%
  animate_plot()


rj <- animate(rj)
anim_save(here::here("images", "right-join.gif"), rj)

rj_g <- plot_data(joined_dfs, "right_join(x, y)")
save_static_plot(rj_g, "right-join")
