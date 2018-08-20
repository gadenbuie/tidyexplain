proc_data <- function(x, .id = "x", color_fun = colorize_keys, color_when = c("after", "before"), ...) {
  color_when <- match.arg(color_when)
  n_colors <- max(x$id)

  if (color_when == "before") x <- color_fun(x, n_colors, ...)

  x <- x %>%
    mutate(.y = -row_number()) %>%
    tidyr::gather("label", "value", setdiff(colnames(x), c(".y", "color"))) %>%
    mutate(value = as.character(value)) %>%
    group_by(.y) %>%
    mutate(
      .x = 1:n(),
      .id = .id,
      .width = 1
    ) %>%
    ungroup(.y)

  if (color_when == "after") x <- color_fun(x, n_colors, ...)
  x
}

colorize_keys <- function(df, n_colors, key_col = "id", color_other = "#d0d0d0", color_missing = "#ffffff") {
  # Assumes that key_col is integer
  colors <- scales::brewer_pal(type = "qual", "Set1")(n_colors)
  mutate(
    df,
    color = ifelse(label == key_col, value, n_colors + 1),
    color = colors[as.integer(color)],
    color = ifelse(is.na(color), "#d0d0d0", color),
    color = ifelse(is.na(value), "#ffffff", color)
  )
}

colorize_row_id <- function(df, n_colors, key_col = "id") {
  # Assumes that key_col is integer
  colors <- scales::brewer_pal(type = "qual", "Set1")(n_colors)
  df$color <- colors[df[[key_col]]]
  df
}

plot_data <- function(x, title = "") {
  if (!"alpha" %in% colnames(x)) x$alpha <- 1
  if (!".width" %in% colnames(x)) x$`.width` <- 1
  ggplot(x) +
    aes(.x, .y, fill = color, label = value) +
    geom_tile(aes(width = .width, alpha = alpha), color = "white", size = 3) +
    geom_text(aes(x = .x), hjust = 0.5, size = 12, family = "Fira Sans", color = "white") +
    scale_fill_identity() +
    scale_alpha_identity() +
    coord_equal() +
    ggtitle(title) +
    theme_void() +
    theme(plot.title = element_text(family = "Fira Mono", hjust = 0.5, size = 24)) +
    guides(fill = FALSE)
}

animate_plot <- function(x, transition_length = 2, state_length = 1) {
  x +
    transition_states(frame, transition_length, state_length) +
    enter_fade() +
    exit_fade() +
    ease_aes("sine-in-out")
}

save_static_plot <- function(g, filename, formats = c("png", "svg")) {
  filenames <- formats %>%
    purrr::set_names() %>%
    purrr::map_chr(static_plot_filename, x = filename) %>%
    purrr::iwalk(
      ~ ggsave(filename = .x, plot = g, dev = .y)
    )
}

static_plot_filename <- function(x, ext) {
  here::here("images", "static", ext, paste0(x, ".", ext))
}

options(tidy_verb_anim.functions_loaded = TRUE)
