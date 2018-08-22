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

colorize_wide_tidyr <- function(df, n_colors, key_col = "id") {
  n_colors <- n_colors + length(setdiff(unique(df$label), key_col))
  colors <- scales::brewer_pal(type = "qual", "Set1")(n_colors)

  df$value_int <- as.integer(gsub("[a-zA-Z]", "0", df$value))
  max_id_color <- max(df$value_int)

  df %>%
    bind_rows(
      filter(df, .y == "-1") %>% mutate(.y = 0)
    ) %>%
    mutate(
      idcp = max_id_color - 1L,
      idc = case_when(
        label == "id" ~ value_int,
        TRUE ~ map_int(label, ~which(. == unique(label))) + idcp
      )
    ) %>%
    select(-idcp, -value_int) %>%
    mutate(
      idc   = ifelse(.y == 0 & label == "id", 100, idc),
      value = ifelse(.y == 0, label, value),
      .id   = ifelse(.y == 0, "n", .id),
      color = colors[idc],
    ) %>%
    filter(!is.na(color)) %>%
    mutate(alpha = ifelse(label != "id" & .y < 0, 0.6, 1.0)) %>%
    select(-idc)
}

plot_data <- function(x, title = "") {
  if (!"alpha" %in% colnames(x)) x$alpha <- 1
  if (!".text_color" %in% colnames(x)) x$`.text_color` <- "white"
  if (!".text_size" %in% colnames(x)) x$`.text_size` <- 12
  ggplot(x) +
    aes(.x, .y, fill = color, label = value) +
    geom_tile(aes(alpha = alpha), width = 0.9, height = 0.9) +
    geom_text(aes(x = .x, color = .text_color, size = .text_size), hjust = 0.5, family = "Fira Sans") +
    scale_fill_identity() +
    scale_alpha_identity() +
    scale_color_identity() +
    scale_size_identity() +
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
