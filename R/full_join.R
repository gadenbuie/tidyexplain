source(here::here("R/00_base.R"))

joined_df <- full_join(x, y, "id") %>%
  proc_data("x") %>%
  mutate(.id = ifelse(value %in% c("4", "y4"), "y", .id)) %>%
  mutate(frame = 2, .x = .x + 1)

extra_blocks <- inner_join(x, y, "id") %>%
  select(id) %>%
  proc_data("y") %>%
  mutate(frame = 2, .x = .x + 1)

fj <- initial_dfs %>%
  bind_rows(joined_df, extra_blocks) %>%
  plot_data("full_join(x, y)") +
  transition_states(frame, transition_length = 2, state_length = 1) +
  enter_appear() +
  exit_disappear(early = TRUE) +
  ease_aes("sine-in-out")

fj <- animate(fj)
anim_save(here::here("images", "full-join.gif"), fj)
