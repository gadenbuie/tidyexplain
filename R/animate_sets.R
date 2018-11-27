#' Animates a set operation
#'
#' Functions to visualise the set operations either static as a ggplot, or
#' dynamic as a gif.
#'
#' @param x the x dataset
#' @param y the y dataset
#' @param export the export type, either gif, first or last. The latter two
#'              export ggplots of the first/last state of the join
#' @param type type of the set, i.e., intersect, setdiff, etc.
#' @param ... further argument passed to anim_options()
#'
#' @return either a gif or a ggplot
#'
#' @seealso \code{\link[dplyr]{setops}}
#'
#' @examples
#' x <- data_frame(x = c(1, 1, 2), y = c("a", "b", "a"))
#' y <- data_frame(x = c(1, 2), y = c("a", "b"))
#'
#' # Animate the first or last state of the set
#' animate_union(x, y, export = "first")
#' animate_union(x, y, export = "last")
#'
#' # animate the transition as a gif (default)
#' \donttest{
#' animate_union(x, y, export = "gif")
#' }
#'
#' # different options include
#' \donttest{
#' animate_union(x, y)
#' animate_union_all(x, y)
#' animate_intersect(x, y)
#' animate_setdiff(x, y)
#'
#' # further arguments can be passed to all animate_* functions
#' animate_union(
#'   x, y,
#'   text_size = 5, title_size = 25,
#'   color_header = "black",
#'   color_fun = viridis::viridis
#' )
#' }
#'
#' # Save the results
#' \dontrun{
#' # to save the ggplot, use
#' un <- animate_union(x, y, by = "id", export = "last")
#' ggsave("union.pdf", un)
#'
#' animate_union(x, y, by = "id", export = "gif")
#' # to save the gif, use
#' un <- animate_union(x, y, by = "id", export = "gif")
#' anim_save(un, "union.gif")
#' }
animate_set <- function(
  x, y,
  type = c("union", "union_all", "intersect", "setdiff"),
  export = c("gif", "first", "last"),
  ...
) {
  type   <- match.arg(type)
  export <- match.arg(export)
  x_name <- get_input_text(x)
  y_name <- get_input_text(y)
  data   <- make_named_data(x, y)

  col_names <- purrr::map(data, names)

  if (!all(names(data$x) %in% names(data$y)) && ncol(data$x) == ncol(data$y))
    stop("x and y must have the same variables/column-names")

  title <- sprintf(paste0(type, "(%s, %s)"), x_name, y_name)

  if (type %in% c("union", "intersect", "setdiff")) {
    data <- purrr::map(data, dplyr::distinct)
  }

  if (type == "union_all") {
    ll <- process_join(data$x, data$y, by = names(data$x), fill = FALSE, ...)
    ll <- purrr::map(ll, ~ mutate(., .id_long = paste(.id_long, .side, sep = "-")))
  } else {
    ll <- process_join(data$x, data$y, by = names(data$x), ...)
  }

  step0 <- bind_rows(ll$x, ll$y) %>% mutate(.frame = 0, .alpha = 1)

  step1 <- move_together(ll$x, ll$y, type) %>% mutate(.frame = 1)

  all <- bind_rows(step0, step1)

  if (export == "gif") {
    animate_plot(all, title, ...) %>% animate()
  } else if (export == "first") {
    title <- ""
    static_plot(step0, title, ...)
  } else if (export == "last") {
    static_plot(step1, title, ...)
  }
}

#' @rdname animate_set
#' @export
animate_union <- function(x, y, export = "gif", ...) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  animate_set(x, y, type = "union", export = export, ...)
}

#' @rdname animate_set
#' @export
animate_union_all <- function(x, y, export = "gif", ...) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  animate_set(x, y, type = "union_all", export = export, ...)
}

#' @rdname animate_set
#' @export
animate_intersect <- function(x, y, export = "gif", ...) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  animate_set(x, y, type = "intersect", export = export, ...)
}

#' @rdname animate_set
#' @export
animate_setdiff <- function(x, y, export = "gif", ...) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  animate_set(x, y, type = "setdiff", export = export, ...)
}
