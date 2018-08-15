source(here::here("R/00_base.R"))

joined_dfs <- left_join(x, y, "id") %>%
  proc_data("x") %>%
  mutate(frame = 2, .x = .x + 1)

extra_blocks <- inner_join(x, y, "id") %>%
  select(id) %>%
  proc_data("y") %>%
  mutate(frame = 2, .x = .x + 1)

lj <- initial_dfs %>%
  bind_rows(joined_dfs) %>%
  bind_rows(extra_blocks) %>%
  mutate(color = ifelse(is.na(value), "#ffffff", color)) %>%
  plot_data("left_join(x, y)") %>%
  animate_plot()

lj <- animate(lj)
anim_save(here::here("images", "left-join.gif"), lj)
