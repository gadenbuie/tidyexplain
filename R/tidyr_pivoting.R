source(here::here("R", "00_base_tidyr.R"))

pv_wide <-
  wide %>%
  proc_data("0-wide", colorize_wide_tidyr) %>%
  mutate(frame = 1, .id = "0-wide")

pv_long <-
  wide %>%
  tidyr::pivot_longer(x:z, names_to = "key", values_to = "val") %>%
  proc_data("3-tall", color_fun = function(x, y) x) %>%
  split(.$label)

pv_long$id <-
  pv_wide %>%
  filter(label == "id") %>%
  select(value, color) %>%
  left_join(pv_long$id, ., by = "value") %>%
  mutate(alpha = 1)

pv_long$key <-
  pv_wide %>%
  filter(label != "id") %>%
  select(label, color) %>%
  left_join(pv_long$key, ., by = c("value" = "label")) %>%
  distinct() %>%
  mutate(alpha = 1)

pv_long$val <-
  pv_wide %>%
  filter(label != "id", .y < 0) %>%
  select(value, color) %>%
  left_join(pv_long$val, ., by = "value") %>%
  mutate(alpha = 0.6)

pv_long <- bind_rows(pv_long) %>% mutate(frame = 2)

pv_wide <- pv_wide %>%
  # add (hidden) copies of cells that are duplicated in the long form
  copy_rows(value %in% 1:2, n = 2) %>%
  copy_rows(value %in% c("x", "y", "z"))

pv_long_labels <-
  tibble(id = 1, a = "id", x = "key", y = "val") %>%
  proc_data("4-label") %>%
  filter(label != "id") %>%
  mutate(color = "#FFFFFF", .y = 0, .x = .x -1, frame = 2, alpha = 0, label = recode(label, "a" = "id"))

pv_wide_labels <-
  tibble(id = 1, a = "id") %>%
  proc_data("2-label") %>%
  filter(label != "id") %>%
  mutate(color = "#FFFFFF", .y = 0, .x = .x -1, frame = 1, alpha = 0, label = recode(label, "a" = "id"))

# An intermediate step with key and value in the right margin
pv_wide_intermediate <-
  bind_rows(pv_wide, pv_long_labels) %>%
  mutate(
    frame = 1.5,
    .id = sub("^\\d", "1", .id),
    .x = ifelse(value %in% c("key", "val"), 5, .x),
    .y = ifelse(value == "val", -1.5, .y)
  )

# Fly "key" and "value" up into title to reset
pv_wide_extra_labels <-
  pv_long_labels %>%
  filter(value %in% c("key", "val")) %>%
  mutate(alpha = 0, frame = 1, .id = "0-label") %>%
  mutate(
    .x = 3.66,
    .y = ifelse(value == "key", 2, 1)
  )

pv_long_extra_keys <-
  map_dfr(
    seq_len(nrow(wide) - 1),
    ~ filter(pv_wide, .y > -1) # Extra key blocks in long column
  )

n_key_cols <- length(setdiff(colnames(wide), "id"))

pv_long_extra_id <-
  map_dfr(
    seq_len(n_key_cols - 1),
    ~ filter(pv_wide, .x == 1) # Extra id column blocks for long column
  )

titles <- list(
  wider = 'pivot_wider(long,\n   names_from = key,\n  values_from = val)',
  longer_int = '\n     cols = x:z,\n   names_to = "key",\n  values_to = "val" ',
  longer = 'pivot_longer(wide,\n     cols = x:z,\n   names_to = "key",\n  values_to = "val")'
)

pv_data <-
  bind_rows(
    pv_wide,
    pv_wide_labels,
    pv_wide_extra_labels,
    pv_wide_intermediate,
    pv_long,
    pv_long_labels,
    # pv_long_extra_keys,
    pv_long_extra_id,
  ) %>%
  mutate(
    label = ifelse(value %in% setdiff(colnames(wide), "id"), "key", label),
    label = ifelse(value %in% c("key", "val"), "zzz", label),
    .text_color = ifelse(grepl("label", .id), "black", "white"),
    .text_size = ifelse(grepl("label", .id), 8, 12),
    .text_color = case_when(
      frame != 1 | grepl("label", .id) ~ .text_color,
      .y == 0 ~ color,
      TRUE ~ .text_color
    ),
    # hide "key" and "val" text in first frame
    .text_alpha = ifelse(value %in% c("key", "val") & frame == 1, 0, 1),
    # hide background of x,y,z column names in first frame
    alpha = ifelse(value %in% c("x", "y", "z") & frame == 1, 0, alpha)
  ) %>%
  mutate(frame = factor(frame, levels = c(1, 1.5, 2))) %>%
  select(.x, .y, everything())

pv_static <-
  pv_data %>%
  split(.$frame) %>%
  imap(~ plot_data(.x, .y) +
         ylim(-6.5, 0.5) +
         labs(subtitle = "returns") +
         theme(
           plot.subtitle = element_text(family = "Fira Sans", size = 14, color = "grey50", hjust = 0.5, margin = margin(25))
         )
  ) %>%
  map2(titles, ~ .x + ggtitle(.y))

save_static_plot(pv_static[[3]], "tidyr-pivot_longer", width = 4, height = 8)
save_static_plot(pv_static[[1]], "tidyr-pivot_wider", width = 4, height = 8)

# Custom titles by frame
animated_titles <- as.character(cut(
  1:120,
  breaks = 6,
  labels = c("wide", titles$longer, titles$longer, titles$longer, "long", titles$wider)
))
animated_titles[1:23] <- "wide"

# Calculate min/max x and y for the view port (for view_zoom_manual() below)
views <-
  pv_data %>%
  group_by(frame) %>%
  summarize(across(c(.x, .y), list(min = min, max = max))) %>%
  mutate(
    across(ends_with("min"), ~ .x - 0.5),
    across(ends_with("max"), ~ .x + 0.5)
  )

pv_anim_plot <-
  plot_data(pv_data) +
    aes(group = value) +
    theme(
      plot.title = element_text(family = "Fira Mono", size = 20, lineheight = 1.3, margin = margin(b = 50)),
      plot.margin = margin(t = 75, unit = "pt")
    )

pv_anim <-
  animate_plot(pv_anim_plot, transition_length = 1) +
  # zoom with smooth transitions: fixed y but make space in the x axis
  view_zoom_manual(
    xmin = views$.x_min,
    xmax = views$.x_max,
    ymin = rep(min(views$.y_min), times = nrow(views)),
    ymax = rep(min(views$.y_max), times = nrow(views)),
    ease = "quintic-out"
  ) +
  labs(title = '{case_when(frame < 24 ~ "wide", frame < 41 ~ titles$longer, frame < 61 ~ titles$longer_int, frame < 81 ~ titles$longer, frame < 101 ~ "long", TRUE ~ titles$wider)}') +
  labs(title = '{animated_titles[frame]}') +
  ease_aes("sine-in-out", x = "exponential-in-out", y = "exponential-in-out", alpha = "circular-in-out")

pv_anim <- animate(pv_anim, width = 580, height = 700, nframes = 120)
anim_save(here::here("images", "tidyr-pivoting.gif"), pv_anim)
