#' Animates a set operation
#'
#' Functions to visualise the set operations either static as a ggplot, or
#' dynamic as a gif.
#'
#' @param x the x dataset
#' @param y the y dataset
#' @param export the export type, either gif, first or last. The latter two
#'              export ggplots of the first/last state of the join
#' @param ... further argument passed to static_plot
#'
#' @return either a gif or a ggplot
#'
#' @seealso \code{\link[dplyr]{setops}}
#'
#' @name animate_set
#' @examples
#' x <- data_frame(
#'   x = c(1, 1, 2),
#'   y = c("a", "b", "a")
#' )
#' y <- data_frame(
#'   x = c(1, 2),
#'   y = c("a", "b")
#' )
#'
#' # Animate the first or last state of the set
#' animate_union(x, y, export = "first")
#' animate_union(x, y, export = "last")
#'
#' # animate the transition as a gif (default)
#' \donttest{
#'   animate_union(x, y, export = "gif")
#' }
#'
#' # different options include
#' \donttest{
#'   animate_union(x, y)
#'   animate_union_all(x, y)
#'   animate_intersect(x, y)
#'   animate_setdiff(x, y)
#'
#'   # further arguments can be passed to all animate_* functions
#'   animate_union(
#'     x, y,
#'     text_size = 5, title_size = 25,
#'     color_header = "black",
#'     color_other = "lightblue",
#'     color_fun = viridis::viridis
#'   )
#' }
#'
#' # Save the results
#' \dontrun{
#'   # to save the ggplot, use
#'   un <- animate_union(x, y, by = "id", export = "last")
#'   ggsave("union.pdf", un)
#'
#'   animate_union(x, y, by = "id", export = "gif")
#'   # to save the gif, use
#'   un <- animate_union(x, y, by = "id", export = "gif")
#'   anim_save(un, "union.gif")
#' }
NULL

#' @rdname animate_set
#' @export
animate_union <- function(x, y, export = "gif", ...) {
  animate_set(x, y, type = "union", export = export, ...)
}

#' @rdname animate_set
#' @export
animate_union_all <- function(x, y, export = "gif", ...) {
  animate_set(x, y, type = "union_all", export = export, ...)
}

#' @rdname animate_set
#' @export
animate_intersect <- function(x, y, export = "gif", ...) {
  animate_set(x, y, type = "intersect", export = export, ...)
}

#' @rdname animate_set
#' @export
animate_setdiff <- function(x, y, export = "gif", ...) {
  animate_set(x, y, type = "setdiff", export = export, ...)
}
