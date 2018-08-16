source(here::here("R/00_base.R"))

joined_df <- inner_join(x, y, "id")
joined_df <- bind_rows(
  proc_data(joined_df, "x"),
  proc_data(joined_df, "y")
) %>%
  filter(!(label == "x" & .id == "y") & !(label == "y" & .id == "x")) %>%
  mutate(frame = 2, .x = .x + 1)

ij <- initial_dfs %>%
  bind_rows(joined_df) %>%
  mutate(removed = value %in% c("3", "4", "x3", "y4"),
         removed = as.integer(removed)) %>%
  arrange(desc(frame), removed) %>%
  plot_data("inner_join(x, y)") %>%
  animate_plot()

ij <- animate(ij)
anim_save(here::here("images", "inner-join.gif"), ij)

ij_g <- inner_join(x, y, by = "id") %>%
  proc_data() %>%
  plot_data("inner_join(x, y)")

save_static_plot(ij_g, "inner-join")
