source(here::here("R/00_base_join.R"))

ij_joined_df <- inner_join(x, y, "id")
ij_joined_df <- bind_rows(
  proc_data(ij_joined_df, "x", add_colnames = TRUE),
  proc_data(ij_joined_df, "y", add_colnames = TRUE)
) %>%
  filter(!(label == "x" & .id == "y") & !(label == "y" & .id == "x")) %>%
  mutate(frame = 2, .x = .x + 1)

ij <- bind_rows(
  initial_join_dfs,
  ij_joined_df
) %>%
  mutate(removed = value %in% c("3", "4", "x3", "y4"),
         removed = as.integer(removed)) %>%
  arrange(label, .id, .x, .y) %>%
  plot_data('inner_join(x, y, by = "id")') %>%
  animate_plot()

ij <- animate(ij)
anim_save(here::here("images", "inner-join.gif"), ij)

ij_g <- inner_join(x, y, by = "id") %>%
  proc_data() %>%
  mutate(.x = .x + 1) %>%
  plot_data_join('inner_join(x, y, by = "id")')

save_static_plot(ij_g, "inner-join")
