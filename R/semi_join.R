source(here::here("R/00_base_join.R"))

sj_joined_df <- semi_join(x, y, "id") %>%
  proc_data("x") %>%
  mutate(frame = 2, .x = .x + 1.5)

sj_extra_blocks <- inner_join(x, y, "id") %>%
  select(id) %>%
  proc_data("y") %>%
  mutate(frame = 2, .x = .x + 1.5)

sj <- bind_rows(
  initial_join_dfs,
  sj_joined_df,
  sj_extra_blocks
) %>%
  arrange(value) %>%
  plot_data("semi_join(x, y)") %>%
  animate_plot()

sj <- animate(sj)
anim_save(here::here("images", "semi-join.gif"), sj)

# Static Images
sj_g <- semi_join(x, y, "id") %>%
  proc_data() %>%
  mutate(.x = .x + 1.5) %>%
  plot_data_join("semi_join(x, y)")

save_static_plot(sj_g, "semi-join")
