source(here::here("R/03_base2.R"))

union_df <- union(x,y) # %>% arrange(id)
union_df <-
  bind_rows(
    proc_data2(union_df, "x"),
    proc_data2(union_df, "y")
  ) %>%
  filter(.id != "y" | (.id == "y" & .y == -4)) %>%
  mutate(frame = 2, .x = .x + 2)

u <-
  initial_dfs %>%
  bind_rows(union_df) %>%
  arrange(desc(frame)) %>%
  filter(!(label == "id")) %>%
  plot_data("union(x, y)") %>%
  animate_plot()

u <- animate(u)

anim_save(here::here("images", "union.gif"), u)

u_g <- union(x, y) %>% # %>% arrange(id)
  proc_data2() %>%  filter(label != "id") %>%
  plot_data("union(x, y)")

save_static_plot(u_g, "union")
