source(here::here("R/00_base_set.R"))

ins_df <- intersect(x,y)
ins_step2 <-
  bind_rows(
    proc_data_set(ins_df, "x"),
    proc_data_set(ins_df, "y")
  ) %>%
  filter(.y == -1) %>%
  mutate(frame = 2, .x = .x + 1.5)

ins <-
  initial_set_dfs %>%
  bind_rows(ins_step2) %>%
  arrange(desc(frame)) %>%
  plot_data_set("intersect(x, y)") %>%
  animate_plot()

ins <- animate(ins)

anim_save(here::here("images", "intersect.gif"), ins)

ins_g <- intersect(x, y) %>%
  proc_data_set() %>%
  mutate(.x = .x + 1.5) %>%
  plot_data_set("intersect(x, y)")

save_static_plot(ins_g, "intersect")
