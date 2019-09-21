source(here::here("R", "00_base_tidyr.R"))

sg_wide <- wide %>%
  proc_data("0-wide", colorize_wide_tidyr) %>%
  mutate(frame = 1, .id = "0-wide")

sg_long <- wide %>%
  tidyr::gather("key", "val", -id) %>%
  proc_data("3-tall", color_fun = function(x, y) x) %>%
  split(.$label)

sg_long$id <-
  sg_wide %>%
  filter(label == "id") %>%
  select(value, color) %>%
  left_join(sg_long$id, ., by = "value") %>%
  mutate(alpha = 1)

sg_long$key <-
  sg_wide %>%
  filter(label != "id") %>%
  select(label, color) %>%
  left_join(sg_long$key, ., by = c("value" = "label")) %>%
  distinct() %>%
  mutate(alpha = 1)

sg_long$val <-
  sg_wide %>%
  filter(label != "id", .y < 0) %>%
  select(value, color) %>%
  left_join(sg_long$val, ., by = "value") %>%
  mutate(alpha = 0.6)

sg_long <- bind_rows(sg_long) %>% mutate(frame = 2)

sg_long_labels <- tibble(id = 1, a = "id", x = "key", y = "val") %>%
  proc_data("4-label") %>%
  filter(label != "id") %>%
  mutate(color = "#FFFFFF", .y = 0, .x = .x -1, frame = 2, alpha = 1, label = recode(label, "a" = "id"))

sg_wide_labels <- tibble(id = 1, a = "id") %>%
  proc_data("2-label") %>%
  filter(label != "id") %>%
  mutate(color = "#FFFFFF", .y = 0, .x = .x -1, frame = 1, alpha = 1, label = recode(label, "a" = "id"))

sg_long_extra_keys <- map_dfr(
  seq_len(nrow(wide) - 1),
  ~ filter(sg_wide, .y > -1) # Extra key blocks in long column
)

n_key_cols <- length(setdiff(colnames(wide), "id"))

sg_long_extra_id <- map_dfr(
  seq_len(n_key_cols - 1),
  ~ filter(sg_wide, .x == 1) # Extra id column blocks for long column
)

sg_data <- bind_rows(
  sg_wide,
  sg_wide_labels,
  sg_long,
  sg_long_labels,
  sg_long_extra_keys,
  sg_long_extra_id
) %>%
  mutate(
    label = ifelse(value %in% setdiff(colnames(wide), "id"), "key", label),
    label = ifelse(value %in% c("key", "val"), "zzz", label),
    .text_color = ifelse(grepl("label", .id), "black", "white"),
    .text_size = ifelse(grepl("label", .id), 8, 12)
  ) %>%
  arrange(label, .id, value) %>%
  mutate(frame = factor(frame, labels = c('spread(long, key, val)', 'gather(wide, key, val, x:z)'))) %>%
  select(.x, .y, everything())

sg_static <-
  sg_data %>%
  split(.$frame) %>%
  imap(~ plot_data(.x, .y) +
         ylim(-6.5, 0.5) +
         labs(subtitle = "returns") +
         theme(plot.subtitle = element_text(family = "Fira Sans", size = 14, color = "grey50", hjust = 0.5, margin = margin(25)))
  )

save_static_plot(sg_static[[1]], "tidyr-spread")
save_static_plot(sg_static[[2]], "tidyr-gather")

sg_anim <-
  sg_data %>%
  plot_data() %>%
  animate_plot() +
  view_follow() +
  labs(title = "{ifelse(transitioning, next_state, ifelse(grepl('gather', next_state), 'long', 'wide'))}") +
  ease_aes("sine-in-out", x = "exponential-out")

sg_anim <- animate(sg_anim)
anim_save(here::here("images", "tidyr-spread-gather.gif"), sg_anim)
