source(here::here("R/00_base_set.R"))

# ---- setdiff(x, y) ----

# Dim elements unique to y
setd_step2 <- initial_set_dfs %>%
  mutate(
    frame = 2,
    alpha = case_when(
      .y == -1 ~ 0.55,
      .id == "y" ~ 0.15,
      TRUE ~ 1
    )
  )

# Merge, dim overlapping elements
setd_step3 <- initial_set_dfs %>%
  filter(!(.id == "y" & .y == -2)) %>%
  mutate(
    frame = 3,
    alpha = ifelse(.y == -1, 0.25, 1),
    .x = ifelse(.id == "y", .x - 3, .x),
    .x = .x + 1.5
  )

# Result of setdiff
setd_step4 <- setdiff(x, y) %>%
  proc_data_set("xy") %>%
  mutate(frame = 4, .x = .x + 1.5)

setd <- bind_rows(
  initial_set_dfs,
  setd_step2,
  setd_step3,
  setd_step4
) %>%
  mutate(alpha = ifelse(is.na(alpha), 1, alpha)) %>%
  arrange(frame, desc(.y), desc(.id)) %>%
  plot_data_set(., "setdiff(x, y)") %>%
  animate_plot()

setd <- animate(setd)

anim_save(here::here("images", "setdiff.gif"), setd)

setd_g <- setdiff(x, y) %>%
  proc_data_set() %>%
  mutate(.x = .x + 1.5) %>%
  plot_data_set("setdiff(x, y)")

save_static_plot(setd_g, "setdiff")


# ---- setdiff(y, x) ----

# Dim elements unique to x
setd2_step2 <- initial_set_dfs %>%
  mutate(
    frame = 2,
    alpha = case_when(
      .y == -1 ~ 0.55,
      .id == "x" ~ 0.15,
      TRUE ~ 1
    )
  )

# Merge, dim overlapping elements
setd2_step3 <- initial_set_dfs %>%
  filter(!(.id == "x" & .y <= -2)) %>%
  mutate(
    frame = 3,
    alpha = ifelse(.y == -1, 0.25, 1),
    .x = ifelse(.id == "y", .x - 3, .x),
    .x = .x + 1.5
  )

# Result of setdiff
setd2_step4 <- setdiff(y, x) %>%
  proc_data_set("xy") %>%
  mutate(frame = 4, .x = .x + 1.5)

setd2 <- bind_rows(
  initial_set_dfs,
  setd2_step2,
  setd2_step3,
  setd2_step4
) %>%
  mutate(alpha = ifelse(is.na(alpha), 1, alpha)) %>%
  arrange(frame, desc(.y), .id) %>%
  plot_data_set(., "setdiff(y, x)") %>%
  animate_plot()

setd2 <- animate(setd2)

anim_save(here::here("images", "setdiff-rev.gif"), setd2)

setd2_g <- setdiff(x, y) %>%
  proc_data_set() %>%
  mutate(.x = .x + 1.5) %>%
  plot_data_set("setdiff(y, x)")

save_static_plot(setd2_g, "setdiff-rev")
