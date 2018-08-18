source(here::here("R/03_base2.R"))

is_df <- intersect(x,y)
step2 <-
  bind_rows(
    proc_data2(is_df, "x"),
    proc_data2(is_df, "y")
  ) %>%
  filter(.y == -1) %>%
  mutate(frame = 2, .x = .x + 2)

is <-
  initial_dfs %>%
  bind_rows(step2) %>%
  arrange(desc(frame)) %>%
  filter(!(label == "id")) %>%
  plot_data("intersect(x, y)") %>%
  animate_plot()

is <- animate(is)

anim_save(here::here("images", "intersect.gif"), is)

is_g <- intersect(x, y) %>%
  proc_data2() %>%  filter(label != "id") %>%
  plot_data("intersect(x, y)")

save_static_plot(is_g, "intersect")
