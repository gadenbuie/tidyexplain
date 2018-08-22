
#' Animates a set - wrapper function
#'
#' @param x left dataset
#' @param y right dataset
#' @param type type of the set, i.e., intersect, setdiff, etc.
#' @param export if the function exports a gif, the first, or last picture
#' @param ... further arguments passed to base_plot
#'
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

  ll <- preprocess_data(x, y, by = names(x))

  step0 <- bind_rows(ll$x, ll$y) %>% mutate(.frame = 0, .alpha = 1)

  step1 <- tidyAnimatedVerbs:::combine(ll$x, ll$y, type) %>% mutate(.frame = 1)

  all <- bind_rows(step0, step1)

  if (export == "gif") {
    animate_plot(all, title, ...) %>% animate()
  } else if (export == "first") {
    base_plot(step0, title, ...)
  } else if (export == "last") {
    base_plot(step1, title, ...)
  }
}

#' Animates a join - wrapper function
#'
#' @param x left dataset
#' @param y right dataset
#' @param by by arguments for the join
#' @param type type of the join, i.e., left_join, right_join, etc.
#' @param export if the function exports a gif, the first, or last picture
#' @param ... further arguments passed to base_plot
#'
#' @return either a gif or a ggplot
#'
#' @examples
#' NULL
animate_join <- function(x, y, by, type, export = "gif", ...) {

  if (!type %in% c("full_join", "inner_join", "left_join", "right_join",
                   "semi_join", "anti_join"))
    stop("type has to be a dplyr-join")

  if (!export %in% c("gif", "first", "last"))
    stop("export must be either gif, first, or last")

  title <- sprintf(paste0(type, "(%s, %s, by = c(\"%s\"))"),
                   deparse(substitute(x)),
                   deparse(substitute(y)),
                   paste(by, collapse = "\", \""))

  ll <- preprocess_data(x, y, by)

  step0 <- bind_rows(ll$x, ll$y) %>% mutate(.frame = 0, .alpha = 1)

  step1 <- tidyAnimatedVerbs:::combine(ll$x, ll$y, type) %>% mutate(.frame = 1)

  all <- bind_rows(step0, step1)


  if (export == "gif") {
    animate_plot(all, title, ...) %>% animate()
  } else if (export == "first") {
    base_plot(step0, title, ...)
  } else if (export == "last") {
    base_plot(step1, title, ...)
  }
}

