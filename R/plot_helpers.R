
#' Animates a plot
#'
#' @param d a preprocessed dataset
#' @param title the plot title
#' @param ... further arguments passed to base_plot
#'
#' @return a gif
#'
#' @examples
#' NULL
animate_plot <- function(d, title = "", ...) {
  base_plot(d, title, ...) +
    transition_states(.frame, 2, 1) +
    enter_fade() +
    exit_fade() +
    ease_aes("sine-in-out")
}


#' Prints the tiles for a processed dataset
#'
#' @param d a processed dataset
#' @param title the title of the plot
#' @param ... further arguments
#'
#' @return a ggplot
#'
#' @examples
#' NULL
base_plot <- function(d, title = "", ...) {
  dots <- list(...)

  if ("text_family" %in% names(dots)) {
    text_family <- dots$text_family
  } else {
    text_family <- "Fira Sans"
  }

  if ("title_family" %in% names(dots)) {
    title_family <- dots$title_family
  } else {
    title_family <- "Fira Mono"
  }

  if ("title_size" %in% names(dots)) {
    title_size <- dots$title_size
  } else {
    title_size <- 20
  }
  if ("text_size" %in% names(dots)) {
    text_size <- dots$text_size
  } else {
    text_size <- 10
  }

  if (!".alpha" %in% names(d)) d <- d %>% mutate(.alpha = 1)
  ggplot(d, aes(x = .x, group = .id_long, y = .y, fill = .color, alpha = .alpha)) +
    geom_tile(width = 0.9, height = 0.9) +
    coord_equal() +
    geom_text(data = d %>% filter(!is.na(val)), aes(label = val), color = "white",
              family = text_family, size = text_size) +
    scale_fill_identity() +
    scale_alpha_identity() +
    labs(title = title) +
    theme_void() +
    theme(plot.title = element_text(family = title_family, hjust = 0.5, size = title_size))
}
