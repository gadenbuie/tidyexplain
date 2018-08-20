source(here::here("R/00_base_join.R"))

initial_join_dfs <- initial_join_dfs %>%
  arrange(.x, .y) %>%
  mutate(.obj = row_number(), .obj = .obj + 90 * as.integer(.id == "y"))

aj_step2 <- initial_join_dfs %>%
  filter(.id == "x" | value %in% paste(1:2)) %>%
  mutate(frame = 2,
         .x = ifelse(.id == "y", 2.5, .x + 1.5),
         alpha = case_when(
           .x > 3 && .id == "x" ~ 0.5,
           .y > -2.5 ~ 0.25,
           TRUE ~ 1
         ))

aj_step3 <- aj_step2 %>%
  filter(alpha == 1) %>%
  mutate(frame = 3)

aj_step4 <- aj_step2 %>%
  filter(alpha == 1) %>%
  mutate(frame = 4, .y = -1)

aj <- bind_rows(
  initial_join_dfs,
  aj_step2,
  aj_step3,
  aj_step4
) %>%
  mutate(
    alpha = ifelse(is.na(alpha), 1, alpha),
    .obj = ifelse(value == 4, 0, .obj)
  ) %>%
  arrange(.obj, frame) %>%
  plot_data("anti_join(x, y)") %>%
  animate_plot(transition_length = c(2, 1, 2),
               state_length = c(1, 0, 0, 1))

aj <- animate(aj)
anim_save(here::here("images", "anti-join.gif"), aj)

aj_g <- anti_join(x, y, by = "id") %>%
  proc_data() %>%
  mutate(.x = .x + 1.5) %>%
  plot_data_join("anti_join(x, y)")

save_static_plot(aj_g, "anti-join")
