source(here::here("R/00_base_set.R"))

# ---- union(x, y) ----
uxy <-
  bind_rows(
    initial_set_dfs,
    union(x, y) %>% proc_data_set("xy") %>% mutate(frame = 2, .x = .x + 1.5),
    intersect(x, y) %>% proc_data_set("xy") %>% mutate(frame = 2, .y = -1, .x = .x + 1.5)
  ) %>%
  plot_data_set("union(x, y)", ylims = ylim(-4.5, -0.5)) %>%
  animate_plot(wrap = FALSE) +
  ease_aes(y = "exponential-out")

uxy <- animate(uxy, rewind = TRUE)

anim_save(here::here("images", "union.gif"), uxy)

uxy_g <- union(x, y) %>%
  proc_data_set() %>%
  mutate(.x = .x + 1.5) %>%
  plot_data_set("union(x, y)", ylims = ylim(-0.5, -4.5))

save_static_plot(uxy_g, "union")


# ---- union(y, x) ----
uyx <-
  bind_rows(
    initial_set_dfs,
    union(y, x) %>% proc_data_set("xy") %>% mutate(frame = 2, .x = .x + 1.5),
    intersect(y, x) %>% proc_data_set("xy") %>% mutate(frame = 2, .y = -1, .x = .x + 1.5)
  ) %>%
  plot_data_set(., "union(y, x)", ylims = ylim(-4.5, -0.5)) %>%
  animate_plot(wrap = FALSE) +
  ease_aes(y = "exponential-out")

uyx <- animate(uyx, rewind = TRUE)

anim_save(here::here("images", "union-rev.gif"), uyx)

uyx_g <- union(y, x) %>%
  proc_data_set() %>%
  mutate(.x = .x + 1.5) %>%
  plot_data_set("union(y, x)", ylims = ylim(-4.5, -0.5))

save_static_plot(uyx_g, "union-rev")
