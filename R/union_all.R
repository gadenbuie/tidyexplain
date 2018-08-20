source(here::here("R/00_base_set.R"))

ua <- bind_rows(
  initial_set_dfs,
  initial_set_dfs %>% mutate(frame = 2, .y = ifelse(.id == "y", .y - 3, .y)), # fly y down
  proc_data_set(x, "ux") %>% mutate(frame = 3, .x = .x + 1.5),                # merge
  proc_data_set(y, "uy") %>% mutate(frame = 3, .x = .x + 1.5, .y = .y - 3),   # un-merge
  initial_set_dfs %>% mutate(frame = 4, .y = ifelse(.id == "y", .y - 3, .y))  # fly y up
) %>%
  arrange(desc(frame)) %>%
  plot_data_set("union_all(x, y)", ylims = ylim(-5.5, -0.5)) +
  transition_states(frame, 1, c(1, 0, 1, 0))

ua <- animate(ua)

anim_save(here::here("images", "union-all.gif"), ua)

ua_g <- union_all(x, y) %>%
  proc_data_set() %>%
  mutate(.x = .x + 1.5) %>%
  plot_data_set("union_all(x, y)", ylims = ylim(-5.5, -0.5))

save_static_plot(ua_g, "union-all")
