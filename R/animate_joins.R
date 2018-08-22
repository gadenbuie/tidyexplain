#' Animates a join operations
#'
#' Functions to visualise the join operations either static as a ggplot, or
#' dynamic as a gif.
#'
#' @param x the x dataset
#' @param y the y dataset
#' @param by the by arguments for the join
#' @param export the export type, either gif, first or last. The latter two
#'              export ggplots of the first/last state of the join
#' @param ... further arguments passed to base_plot
#'
#' @return either a gif or a ggplot
#'
#' @seealso \code{\link[dplyr]{join}}
#'
#' @name animate_join
#' @examples
#' x <- data_frame(
#'   id = 1:3,
#'   x = paste0("x", 1:3)
#' )
#' y <- data_frame(
#'   id = (1:4)[-3],
#'   y = paste0("y", (1:4)[-3])
#' )
#'
#' # Animate the first or last state of the join
#' animate_full_join(x, y, by = "id", export = "first")
#' animate_full_join(x, y, by = "id", export = "last")
#'
#' # animate the transition as a gif (default)
#' \donttest{
#'   animate_full_join(x, y, by = "id", export = "gif")
#' }
#'
#' # different options include
#' \donttest{
#'   animate_full_join(x, y, by = "id")
#'   animate_inner_join(x, y, by = "id")
#'   animate_left_join(x, y, by = "id")
#'   animate_right_join(x, y, by = "id")
#'   animate_semi_join(x, y, by = "id")
#'   animate_anti_join(x, y, by = "id")#'
#' }
#'
#' # Save the results
#' \donttest{
#'   # to save the ggplot, use
#'   fj <- animate_full_join(x, y, by = "id", export = "last")
#'   ggsave("full-join.pdf", fj)
#'
#'   # to save the gif, use
#'   fj <- animate_full_join(x, y, by = "id", export = "gif")
#'   anim_save(fj, "full-join.gif")
#' }
NULL

#' @rdname animate_join
#' @export
animate_full_join <- function(x, y, by, export = "gif", ...) {
  animate_join(x, y, by, type = "full_join", export = export, ...)
}

#' @rdname animate_join
#' @export
animate_inner_join <- function(x, y, by, export = "gif", ...) {
  animate_join(x, y, by, type = "inner_join", export = export, ...)
}

#' @rdname animate_join
#' @export
animate_left_join <- function(x, y, by, export = "gif", ...) {
  animate_join(x, y, by, type = "left_join", export = export, ...)
}

#' @rdname animate_join
#' @export
animate_right_join <- function(x, y, by, export = "gif", ...) {
  animate_join(x, y, by, type = "right_join", export = export, ...)
}

#' @rdname animate_join
#' @export
animate_semi_join <- function(x, y, by, export = "gif", ...) {
  animate_join(x, y, by, type = "semi_join", export = export, ...)
}

#' @rdname animate_join
#' @export
animate_anti_join <- function(x, y, by, export = "gif", ...) {
  animate_join(x, y, by, type = "anti_join", export = export, ...)
}
