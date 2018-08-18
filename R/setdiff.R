source(here::here("R/03_base2.R"))

step2 <- initial_dfs %>%
  filter(.id == "x" | (.id == "y" & .y == -1)) %>%
  mutate(
    frame = 2,
    .x = ifelse(.id == "y", .x - 4, .x),
    .x = .x + 2
  )

step3 <- step2 %>%
  filter(.y != -1) %>%
  ungroup() %>%
  mutate(frame = 3, .y = .y+1)

sd_df <- setdiff(x,y)
step4 <-
  bind_rows(
    proc_data2(sd_df, "x"),
    proc_data2(sd_df, "y")
  ) %>%
  filter(.id == "x") %>%
  mutate(frame = 4, .x = .x + 2)

sd <-
  initial_dfs %>%
  bind_rows(step2, step3, step4) %>%
  arrange(desc(frame)) %>%
  filter(label != "id") %>%
  mutate(alpha = case_when(
    frame == 2 & .y == -1 ~ 0.25,
    TRUE ~ 1
  )) %>%
  {
    plot_data(., "setdiff(x, y)") +
      aes(alpha = alpha) +
      scale_alpha_identity()
  } %>%
  animate_plot()

sd <- animate(sd)

anim_save(here::here("images", "setdiff.gif"), sd)

sd_g <- setdiff(x, y) %>%
  proc_data2() %>% filter(label != "id") %>%
  plot_data("setdiff(x, y)")

save_static_plot(sd_g, "setdiff")
