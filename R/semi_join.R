source(here::here("R/00_base.R"))

joined_df <- semi_join(x, y, "id") %>%
  proc_data("x") %>%
  mutate(frame = 2, .x = .x + 1)

extra_blocks <- inner_join(x, y, "id") %>%
  select(id) %>%
  proc_data("y") %>%
  mutate(frame = 2, .x = .x + 1)

sj <- initial_dfs %>%
  bind_rows(joined_df, extra_blocks) %>%
  plot_data("semi_join(x, y)") %>%
  animate_plot()

sj <- animate(sj)
anim_save(here::here("images", "semi-join.gif"), sj)

# Static Images
sj_g <- semi_join(x, y, "id") %>%
  proc_data() %>%
  plot_data("semi_join(x, y)")

save_static_plot(sj_g, "semi-join")
