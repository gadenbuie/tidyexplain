
#' Animates a set - wrapper function
#'
#' @param x left dataset
#' @param y right dataset
#' @param type type of the set, i.e., intersect, setdiff, etc.
#' @param export if the function exports a gif, the first, or last picture
#' @param ... further arguments passed to base_plot or to add_color
#'
#'
#' @name animate_set_function
#' @return either a gif or a ggplot
#'
#' @examples
#' NULL
animate_set <- function(x, y, type, export = "gif", ...) {

  if (!all(names(x) %in% names(y)) && ncol(x) == ncol(y))
    stop("x and y must have the same variables/column-names")

  if (!type %in% c("union", "union_all", "intersect", "setdiff"))
    stop("type has to be a dplyr-set operation")

  if (!export %in% c("gif", "first", "last"))
    stop("export must be either gif, first, or last")

  title <- sprintf(paste0(type, "(%s, %s)"),
                   deparse(substitute(x)),
                   deparse(substitute(y)))

  if (type %in% c("union", "intersect", "setdiff")) {
    x <- dplyr::distinct(x)
    y <- dplyr::distinct(y)
  }

  if (type == "union_all") {
    ll <- process_join(x, y, by = names(x), fill = FALSE, ...)
    ll <- lapply(ll, function(a)
      a %>% mutate(.id_long = paste(.id_long, .side, sep = "-"))
    )
  } else {
    ll <- process_join(x, y, by = names(x), ...)
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

#' Animates a join - wrapper function
#'
#' @param x left dataset
#' @param y right dataset
#' @param by by arguments for the join
#' @param type type of the join, i.e., left_join, right_join, etc.
#' @param export if the function exports a gif, the first, or last picture
#' @param ... further arguments passed to base_plot or to add_color
#'
#' @return either a gif or a ggplot
#'
#' @name animate_join_function
#' @examples
#' NULL
animate_join <- function(x, y, by, type, export = "gif", ...) {

  if (!type %in% c("full_join", "inner_join", "left_join", "right_join",
                   "semi_join", "anti_join"))
    stop("type has to be a dplyr-join")

  if (!export %in% c("gif", "first", "last"))
    stop("export must be either gif, first, or last")

  by_args <- ifelse(length(by) == 1,
                    sprintf("\"%s\"", by),
                    sprintf("c(\"%s\")", paste(by, collapse = "\", \""))
                    )

  title <- sprintf(paste0(type, "(%s, %s, by = %s)"),
                   deparse(substitute(x)),
                   deparse(substitute(y)),
                   by_args)

  if (type %in% c("semi_join", "anti_join")) {
    # for semi and anti_joins, there is no adding of multiple rows
    y <- dplyr::distinct(y)
  }

  ll <- process_join(x, y, by, ...)

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

