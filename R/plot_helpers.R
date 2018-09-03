#' Animation Options
#'
#' Helper function to set animation and plotting options to be passed to
#' [animate_plot()] and [static_plot()].
#'
#' @param text_family Font family for the plot text
#' @param title_family Font family for the plot title
#' @param text_size Font size of the plot text
#' @param title_size Font size of the plot title
#' @param ease_default Default aes easing function. See [tweenr::display_ease()]
#'   for more options.
#' @param ease_other Additional aes easing options, specified as a named list.
#'   List entries are named with the aesthetic to which the easeing should be
#'   applied, consistent with [gganimate::ease_aes()].
#'   E.g. `list(color = "sine")`.
#' @param enter Enter fading function applied to objects in the animation. See
#'   [gganimate::enter_exit] for a complete list of options.
#' @param exit Exit fading function applied to objects in the animation. See
#'   [gganimate::enter_exit] for a complete list of options.
#' @inheritParams gganimate::transition_states
#' @export
anim_options <- function(
  transition_length = 2,
  state_length = 1,
  ease_default = "sine-in-out",
  ease_other   = NULL,
  enter        = enter_fade(),
  exit         = exit_fade(),
  text_family  = "Fira Sans",
  title_family = "Fira Mono",
  text_size    = NULL,
  title_size   = NULL,
  ...
){
  enter_name <- rlang::quo_name(rlang::enquo(enter))
  exit_name <- rlang::quo_name(rlang::enquo(exit))
  structure(
    list(
      transition_length = transition_length,
      state_length = state_length,
      ease_default = ease_default,
      ease_other   = ease_other,
      enter        = setNames(list(enter), enter_name),
      exit         = setNames(list(exit), exit_name),
      text_family  = text_family,
      text_size    = text_size,
      title_family = title_family,
      title_size   = title_size,
      ...
    ),
    class = "anim_opts"
  )
}

print.anim_opts <- function(ao) {
  aop <- ao
  # Replace ggproto (enter/exit functions) with their names
  aop$enter <- paste("ggproto:", names(ao$enter))
  aop$exit  <- paste("ggproto:", names(ao$exit))
  str(aop)
  invisible(ao)
}

validate_anim_opts <- function(ao, quiet = FALSE, strict = getOption("tidyexplain.strict_dots", FALSE)) {
  if (!inherits(ao, "anim_opts")) {
    rlang::warn("Use `anim_options()` to set `anim_opts`")
  }
  stopifnot(is.ggproto(ao$enter[[1]]), is.ggproto(ao$exit[[1]]))
  extra_names <- setdiff(names(ao), names(formals(anim_options)))
  if (!quiet && length(extra_names)) {
    extra_names <- paste0(sprintf("`%s`", extra_names), collapse = ", ")
    msg <- paste("Unknown animation options will be ignored:", extra_names)
    if (isTrue(strict)) rlang::abort(msg) else rlang::warn(msg)
  }
  invisible(ao)
}

merge.anim_opts <- function(ao_new, ao_base = anim_options()) {
  ao_new <- remove_default_anim_opts(ao_new)
  utils::modifyList(ao_base, ao_new, TRUE)
}

remove_default_anim_opts <- function(ao) {
  ao_default <- anim_options()
  same_names <- purrr::map2_lgl(ao, ao_default, ~ identical(names(.x), names(.y)))
  same <- purrr::map2_lgl(ao, ao_default, ~ identical(.x, .y))
  same[["enter"]] <- same_names[["enter"]]
  same[["exit"]]  <- same_names[["exit"]]
  ao[!same]
}

#' Animates a plot
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
  text_size <- get_text_size(ao$text_size, default = 5)
  title_size <- get_title_size(ao$title_size, default = 17)

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
              family = ao$text_family, size = text_size) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_alpha_identity() +
    labs(title = title) +
    theme_void() +
    theme(plot.title = element_text(family = ao$title_family, hjust = 0.5, size = title_size))
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

