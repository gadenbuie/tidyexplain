
#' Animates a union set
#'
#' @param x the x dataset
#' @param y the y dataset
#' @param export the export type, either gif, first or last. The latter two
#'              export ggplots of the first/last state of the join
#' @param ... further argument passed to base_plot
#'
#' @return either a gif or a ggplot
#' @export
#'
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
#' animate_union(x, y, by = "id", export = "first")
#' animate_union(x, y, by = "id", export = "last")
#'
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
#'
animate_union <- function(x, y, export = "gif", ...) {
  animate_set(x, y, type = "union", export = export, ...)
}

#' Animates a union-all set
#'
#' @inheritParams animate_union
#'
#' @return either a gif or a ggplot
#' @export
#'
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
#' animate_union_all(x, y, by = "id", export = "first")
#' animate_union_all(x, y, by = "id", export = "last")
#'
#' \dontrun{
#'   # to save the ggplot, use
#'   un <- animate_union_all(x, y, by = "id", export = "last")
#'   ggsave("union-all.pdf", un)
#'
#'   animate_union_all(x, y, by = "id", export = "gif")
#'   # to save the gif, use
#'   un <- animate_union_all(x, y, by = "id", export = "gif")
#'   anim_save(un, "union-all.gif")
#' }
#'
animate_union_all <- function(x, y, export = "gif", ...) {
  animate_set(x, y, type = "union_all", export = export, ...)
}

#' Animates an intersect set
#'
#' @inheritParams animate_union
#'
#' @return either a gif or a ggplot
#' @export
#'
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
#' animate_intersect(x, y, by = "id", export = "first")
#' animate_intersect(x, y, by = "id", export = "last")
#'
#' \dontrun{
#'   # to save the ggplot, use
#'   ints <- animate_intersect(x, y, by = "id", export = "last")
#'   ggsave("intersect.pdf", ints)
#'
#'   animate_intersect(x, y, by = "id", export = "gif")
#'   # to save the gif, use
#'   ints <- animate_union_all(x, y, by = "id", export = "gif")
#'   anim_save(ints, "intersect.gif")
#' }
#'
animate_intersect <- function(x, y, export = "gif", ...) {
  animate_set(x, y, type = "intersect", export = export, ...)
}

#' Animates a setdiff set
#'
#' @inheritParams animate_union
#'
#' @return either a gif or a ggplot
#' @export
#'
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
#' animate_setdiff(x, y, by = "id", export = "first")
#' animate_setdiff(x, y, by = "id", export = "last")
#'
#' \dontrun{
#'   # to save the ggplot, use
#'   setd <- animate_setdiff(x, y, by = "id", export = "last")
#'   ggsave("setdiff.pdf", setd)
#'
#'   animate_setdiff(x, y, by = "id", export = "gif")
#'   # to save the gif, use
#'   setd <- animate_union_all(x, y, by = "id", export = "gif")
#'   anim_save(setd, "setdiff.gif")
#' }
#'
animate_setdiff <- function(x, y, export = "gif", ...) {
  animate_set(x, y, type = "setdiff", export = export, ...)
}
