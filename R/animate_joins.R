#' Animates a join operation
#'
#' Functions to visualise the join operations either static as a ggplot, or
#' dynamic as a gif.
#'
#' @param x the x dataset
#' @param y the y dataset
#' @param by the by arguments for the join
#' @param export the export type, either gif, first or last. The latter two
#'              export ggplots of the first/last state of the join
#' @param ... further arguments passed to static_plot
#'
#' @return either a gif or a ggplot
#'
#' @seealso \code{\link[dplyr]{join}}
#'
#' @name animate_join
#' @examples
#' x <- data_frame(id = 1:3, x = paste0("x", 1:3))
#' y <- data_frame(id = (1:4)[-3], y = paste0("y", (1:4)[-3]))
#'
#' # Animate the first or last state of the join
#' animate_full_join(x, y, by = "id", export = "first")
#' animate_full_join(x, y, by = "id", export = "last")
#'
#' # animate the transition as a gif (default)
#' \donttest{
#' animate_full_join(x, y, by = "id", export = "gif")
#' }
#'
#' # different options include
#' \donttest{
#' animate_full_join(x, y, by = "id")
#' animate_inner_join(x, y, by = "id")
#' animate_left_join(x, y, by = "id")
#' animate_right_join(x, y, by = "id")
#' animate_semi_join(x, y, by = "id")
#' animate_anti_join(x, y, by = "id")
#'
#' # further arguments can be passed to all animate_* functions
#' animate_full_join(
#'   x, y, by = "id", export = "last",
#'   text_size = 5, title_size = 25,
#'   color_header = "black",
#'   color_other = "lightblue",
#'   color_fun = viridis::viridis
#' )
#' }
#'
#' # Save the results
#' \donttest{
#' # to save the ggplot, use
#' fj <- animate_full_join(x, y, by = "id", export = "last")
#' ggsave("full-join.pdf", fj)
#'
#' # to save the gif, use
#' fj <- animate_full_join(x, y, by = "id", export = "gif")
#' anim_save(fj, "full-join.gif")
#' }
animate_join <- function(
  x,
  y,
  by,
  type = c("full_join", "inner_join", "left_join", "right_join",
           "semi_join", "anti_join"),
  export = c("gif", "first", "last"),
  ...
) {
  type   <- match.arg(type)
  export <- match.arg(export)
  x_name <- get_input_text(x)
  y_name <- get_input_text(y)
  data   <- make_named_data(x, y)

  by_args <- if (length(by) == 1) sprintf("\"%s\"", by) else
    sprintf("c(\"%s\")", paste(by, collapse = "\", \""))

  title <- sprintf(paste0(type, "(%s, %s, by = %s)"), x_name, y_name, by_args)

  if (type %in% c("semi_join", "anti_join")) {
    # for semi and anti_joins, there is no adding of multiple rows
    data$y <- dplyr::distinct(data$y)
  }

  ll <- process_join(data$x, data$y, by, ...)

  step0 <- bind_rows(ll$x, ll$y) %>% mutate(.frame = 0, .alpha = 1)

  step1 <- move_together(ll$x, ll$y, type) %>% mutate(.frame = 1)

  all <- bind_rows(step0, step1)

  if (export == "gif") {
    animate_plot(all, title, ...)
  } else if (export == "first") {
    title <- ""
    static_plot(step0, title, ...)
  } else if (export == "last") {
    static_plot(step1, title, ...)
  }
}


#' @rdname animate_join
#' @export
animate_full_join <- function(x, y, by, export = "gif", ...) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  animate_join(x, y, by, type = "full_join", export = export, ...)
}

#' @rdname animate_join
#' @export
animate_inner_join <- function(x, y, by, export = "gif", ...) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  animate_join(x, y, by, type = "inner_join", export = export, ...)
}

#' @rdname animate_join
#' @export
animate_left_join <- function(x, y, by, export = "gif", ...) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  animate_join(x, y, by, type = "left_join", export = export, ...)
}

#' @rdname animate_join
#' @export
animate_right_join <- function(x, y, by, export = "gif", ...) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  animate_join(x, y, by, type = "right_join", export = export, ...)
}

#' @rdname animate_join
#' @export
animate_semi_join <- function(x, y, by, export = "gif", ...) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  animate_join(x, y, by, type = "semi_join", export = export, ...)
}

#' @rdname animate_join
#' @export
animate_anti_join <- function(x, y, by, export = "gif", ...) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  animate_join(x, y, by, type = "anti_join", export = export, ...)
}
