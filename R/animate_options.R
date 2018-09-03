#' Animation Options
#'
#' Helper function to set animation and plotting options to be passed to
#' [animate_plot()] and [static_plot()].
#'
#' @param text_family Font family for the plot text, default is "Fira Mono". Use
#'   [set_font_size()] to set global default font sizes.
#' @param title_family Font family for the plot title, default is "Fira Mono".
#'   Use [set_font_size()] to set global default font sizes.
#' @param text_size Font size of the plot text, default is 5.
#' @param title_size Font size of the plot title, default is 17.
#' @param ease_default Default aes easing function. See [tweenr::display_ease()]
#'   for more options. The tidyexplain default value is `sine-in-out`.
#' @param ease_other Additional aes easing options, specified as a named list.
#'   List entries are named with the aesthetic to which the easeing should be
#'   applied, consistent with [gganimate::ease_aes()]. E.g. `list(color =
#'   "sine")`.
#' @param enter Enter fading function applied to objects in the animation. See
#'   [gganimate::enter_exit] for a complete list of options. The tidyexplain
#'   default is [gganimate::enter_fade()].
#' @param exit Exit fading function applied to objects in the animation. See
#'   [gganimate::enter_exit] for a complete list of options. The tidyexplain
#'   default is [gganimate::exit_fade()].
#' @inheritParams gganimate::transition_states
#' @export
anim_options <- function(
  transition_length = NULL,
  state_length = NULL,
  ease_default = NULL,
  ease_other   = NULL,
  enter        = NULL,
  exit         = NULL,
  text_family  = NULL,
  title_family = NULL,
  text_size    = NULL,
  title_size   = NULL,
  ...
){
  enter_name <- if (!missing(enter)) rlang::quo_name(rlang::enquo(enter))
  exit_name <- if (!missing(exit))   rlang::quo_name(rlang::enquo(exit))
  ao <- list(
    transition_length = transition_length,
    state_length = state_length,
    ease_default = ease_default,
    ease_other   = ease_other,
    enter        = if (!is.null(enter)) setNames(list(enter), enter_name),
    exit         = if (!is.null(exit))  setNames(list(exit), exit_name),
    text_family  = text_family,
    text_size    = text_size,
    title_family = title_family,
    title_size   = title_size,
    ...
  )
  ao <- purrr::compact(ao)
  structure(ao, class = "anim_opts")
}


# Global Animation Options Setters and Getters ----------------------------

#' @describeIn anim_options Set default animation options for the current session.
#' @param anim_opts An [anim_options()] options list.
#' @export
anim_options_set <- function(anim_opts) {
  stopifnot(is.anim_opts(anim_opts))
  ao_old <- plot_settings$anim_opts
  plot_settings$anim_opts <- merge(anim_opts, plot_settings$anim_opts)
  invisible(ao_old)
}

get_anim_opt <- function(anim_opt = NULL) {
  if (is.null(anim_opt)) return(plot_settings$anim_opts)
  if (anim_opt %in% c("text_size", "title_size")) rlang::abort(
    "Use get_text_size() or get_title_size()"
  )
  plot_settings$anim_opts[[anim_opt]] %||% plot_settings$default[[anim_opt]]
}


# Animation Options Methods -----------------------------------------------

print.anim_opts <- function(ao) {
  aop <- purrr::discard(ao, is.null)
  # Replace ggproto (enter/exit functions) with their names
  if ("enter" %in% names(aop)) aop$enter <- paste("ggproto:", names(ao$enter))
  if ("exit" %in% names(aop))  aop$exit  <- paste("ggproto:", names(ao$exit))
  str(aop)
  invisible(ao)
}

is.anim_opts <- function(ao) inherits(ao, "anim_opts")


# Fill, Validate, Merge Animation Options ---------------------------------

# Fills in default animation options
fill_anim_opts <- function(ao) {
  ao$transition_length <- ao$transition_length %||% get_anim_opt("transition_length")
  ao$state_length <- ao$state_length %||% get_anim_opt("state_length")
  ao$ease_default <- ao$ease_default %||% get_anim_opt("ease_default")
  ao$ease_other   <- ao$ease_other   %||% get_anim_opt("ease_other")
  ao$enter        <- ao$enter        %||% get_anim_opt("enter")
  ao$exit         <- ao$exit         %||% get_anim_opt("exit")
  ao$text_family  <- ao$text_family  %||% get_anim_opt("text_family")
  ao$title_family <- ao$title_family %||% get_anim_opt("title_family")
  ao
}

validate_anim_opts <- function(ao, quiet = FALSE, strict = getOption("tidyexplain.strict_dots", FALSE)) {
  if (!inherits(ao, "anim_opts")) {
    rlang::warn("Use `anim_options()` to set `anim_opts`")
  }
  ao <- fill_anim_opts(ao)
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
  ao_new  <- purrr::discard(ao_new, is.null)
  ao_base <- purrr::discard(ao_base, is.null)
  unique_base <- setdiff(names(ao_base), names(ao_new))
  ao <- append(ao_new, ao_base[unique_base])
  ao <- ao[names(formals(anim_options))]
  ao <- purrr::discard(ao, is.null)
  class(ao) <- "anim_opts"
  ao
}


# Default Animation Options for Verb Families -----------------------------

default_anim_opts <- function(family, ao_custom = NULL) {
  family_options <- c("join", "set", "gather", "spread")
  family <- match.arg(family, family_options, several.ok = FALSE)
  ao_default <- switch(
    family,
    "gather" = anim_options(enter = enter_fade(), exit = exit_fade(),
                            ease_default = "sine-in-out",
                            ease_other = list(y = "cubic-out", x = "cubic-in")),
    "spread" = anim_options(enter = enter_fade(), exit = exit_fade(),
                            ease_default = "sine-in-out",
                            ease_other = list(y = "cubic-out", x = "cubic-in")),
    anim_options()
  )
  if (is.null(ao_custom)) ao_default else merge(ao_custom, ao_default)
}

# Font Size Setters and Getters -------------------------------------------

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

#' @describeIn set_font_size Get current global font sizes
#' @export
get_font_size <- function() {
  list("text_size"  = get_text_size(), "title_size" = get_title_size())
}

set_text_size <- function(size) {
  old <- plot_settings$text_size
  anim_options_set(anim_options(text_size = size))
  invisible(old)
}

set_title_size <- function(size) {
  old <- plot_settings$title_size
  anim_options_set(anim_options(title_size = size))
  invisible(old)
}

get_text_size <- function(x = NULL) {
  if (!is.null(x)) return(x)
  plot_settings$anim_opts$text_size %||%
    getFromNamespace("theme_env", "ggplot2")$current$text$size %||%
    plot_settings$default$text_size
}

get_title_size <- function(x = NULL) {
  if (!is.null(x)) return(x)
  plot_settings$anim_opts$text_size %||%
    getFromNamespace("theme_env", "ggplot2")$current$plot.title$size %||%
    plot_settings$default$title_size
}
