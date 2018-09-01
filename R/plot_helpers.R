#' Animates a plot
#'
#' @param d a preprocessed dataset
#' @param title the plot title
#' @param transition_length see transition_states
#' @param state_length see transition_states
#' @param ... further arguments passed to static_plot
#'
#' @return a gif
#'
#' @examples
#' NULL
animate_plot <- function(d, title = "", transition_length = 2, state_length = 1, ...) {
  static_plot(d, title, ...) +
    transition_states(.frame, transition_length, state_length) +
    enter_fade() +
    exit_fade() +
    ease_aes("sine-in-out")
}


#' Prints the tiles for a processed dataset statically
#'
#' @param d a processed dataset
#' @param title the title of the plot
#' @param text_family the font for the text
#' @param title_family the font for the title
#' @param text_size the size of the text
#' @param title_size the size of the title
#' @param ... further arguments
#'
#' @return a ggplot
#'
#' @examples
#' NULL
static_plot <- function(
  d,
  title = "",
  text_family = "Fira Sans", title_family = "Fira Mono",
  text_size = NULL, title_size = NULL,
  ...
) {
  text_size <- get_text_size(text_size, default = 5)
  title_size <- get_title_size(title_size, default = 17)

  if (!".alpha" %in% names(d)) d <- d %>% mutate(.alpha = 1)
  if (!".textcolor" %in% names(d))
    d <- d %>% mutate(.textcolor = choose_text_color(.color))

  if (".id_long" %in% names(d)) {
    d <- d %>% mutate(.item_id = paste(.id_long, .col, sep = "-"))
  } else {
    # tidyr
    d <- d %>% mutate(.item_id = .id)
  }

  ggplot(d, aes(x = .x, y = .y, fill = .color, alpha = .alpha, group = .item_id)) +
    geom_tile(width = 0.9, height = 0.9) +
    coord_equal() +
    geom_text(data = d %>% filter(!is.na(.val)), aes(label = .val, color = .textcolor),
              family = text_family, size = text_size) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_alpha_identity() +
    labs(title = title) +
    theme_void() +
    theme(plot.title = element_text(family = title_family, hjust = 0.5, size = title_size))
}

#' Set Default Text Sizes for Animation Plots
#'
#' Sets the default text sizes for the animated and static plots produced by
#' this package during the current session.
#'
#' @param text_size Font size of value labels inside the data frame squares
#' @param title_size Font size of the function call or plot title
#' @export
set_font_size <- function(text_size = NULL, title_size = NULL) {
  old <- list()
  if (!is.null(text_size)) old$text_size <- set_text_size(text_size)
  if (!is.null(title_size)) old$title_size <- set_title_size(title_size)
  invisible(old)
}

set_text_size <- function(size) {
  old <- plot_settings$text_size
  plot_settings$text_size <- size
  invisible(old)
}

set_title_size <- function(size) {
  old <- plot_settings$title_size
  plot_settings$title_size <- size
  invisible(old)
}

get_text_size <- function(x = NULL, default = 5) {
  if (!is.null(x)) return(x)
  plot_settings$text_size %||%
    getFromNamespace("theme_env", "ggplot2")$current$text$size %||%
    default
}

get_title_size <- function(x = NULL, default = 17) {
  if (!is.null(x)) return(x)
  plot_settings$title_size %||%
    getFromNamespace("theme_env", "ggplot2")$current$plot.title$size %||%
    default
}

