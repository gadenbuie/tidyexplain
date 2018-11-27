#' Animate a Plot
#'
#' @param d a processed dataset
#' @param title the title of the plot
#' @param anim_opts Animation options generated with [anim_options()]. Overrides
#'   any options set in `...`.
#' @return a `gganim` object
#' @examples
#' NULL
animate_plot <- function(
  d,
  title = "",
  ...,
  anim_opts = anim_options(...)
) {
  ao <- validate_anim_opts(anim_opts)
  ease_opts <- if (!is.null(ao$ease_other)) {
    ao$ease_other$default <- ao$ease_default
    ao$ease_other
    } else list(default = ao$ease_default)
  ao_ease_aes <- do.call(ease_aes, ease_opts)

  static_plot(d, title, anim_opts = ao) +
    transition_states(.frame, ao$transition_length, ao$state_length) +
    ao$enter[[1]] +
    ao$exit[[1]] +
    ao_ease_aes
}


#' Prints the tiles for a processed dataset statically
#'
#' @inheritParams animate_plot
#' @inheritDotParams anim_options
#'
#' @return a ggplot
#'
#' @examples
#' NULL
static_plot <- function(
  d,
  title = "",
  ...,
  anim_opts = anim_options(...)
) {
  ao <- validate_anim_opts(anim_opts)
  text_size <- get_text_size(ao$text_size)
  title_size <- get_title_size(ao$title_size)

  if (!".alpha" %in% names(d)) d <- d %>% mutate(.alpha = 1)
  if (!".textcolor" %in% names(d))
    d <- d %>% mutate(.textcolor = choose_text_color(.color))

  if (".id_long" %in% names(d)) {
    d <- d %>% mutate(.item_id = paste(.id_long, .col, sep = "-"))
  } else {
    # tidyr
    d <- d %>% mutate(.item_id = .id)
  }

  width <- ao$cell_width %||% 1
  height <- ao$cell_height %||% 1

  ggplot(d, aes(x = .x * width, y = .y * height, fill = .color, alpha = .alpha,
                group = .item_id)) +
    geom_tile(width = 0.9 * width, height = 0.9 * height) +
    coord_equal() +
    geom_text(data = d %>% filter(!is.na(.val)), aes(label = .val, color = .textcolor),
              family = ao$text_family, size = text_size) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_alpha_identity() +
    labs(title = title) +
    theme_void() +
    theme(plot.title = element_text(family = ao$title_family, hjust = 0.5, size = title_size))
}
