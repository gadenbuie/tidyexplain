

set_text_color <- function(a) ifelse(apply(col2rgb(a), 2, mean) > 127, "black", "white")

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
static_plot <- function(d, title = "",
                        text_family = "Fira Sans", title_family = "Fira Mono",
                        text_size = 7, title_size = 25, ...) {

  if (!".alpha" %in% names(d)) d <- d %>% mutate(.alpha = 1)
  if (!".textcolor" %in% names(d))
    d <- d %>% mutate(.textcolor = set_text_color(.color))

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

