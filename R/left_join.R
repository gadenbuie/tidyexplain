source(here::here("R/00_base_join.R"))

lj_joined_dfs <- left_join(x, y, "id") %>%
  proc_data("x") %>%
  mutate(frame = 2, .x = .x + 1)

lj_extra_blocks <- inner_join(x, y, "id") %>%
  select(id) %>%
  proc_data("y") %>%
  mutate(frame = 2, .x = .x + 1)

lj <- bind_rows(
  initial_join_dfs,
  lj_joined_dfs,
  lj_extra_blocks
) %>%
  mutate(color = ifelse(is.na(value), "#ffffff", color)) %>%
  arrange(value) %>%
  plot_data("left_join(x, y)") %>%
  animate_plot()

lj <- animate(lj)
anim_save(here::here("images", "left-join.gif"), lj)

lj_g <- plot_data_join(lj_joined_dfs, "left_join(x, y)")

save_static_plot(lj_g, "left-join")
