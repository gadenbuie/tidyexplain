
#' Animates a full join
#'
#' @param x the x dataset
#' @param y the y dataset
#' @param by the by arguments for the join
#' @param export the export type, either gif, first or last. The latter two
#'              export ggplots of the first/last state of the join
#' @param ... further arguments passed to base_plot
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
#' animate_full_join(x, y, by = "id", export = "first")
#' animate_full_join(x, y, by = "id", export = "last")
#'
#' \dontrun{
#'   # to save the ggplot, use
#'   fj <- animate_full_join(x, y, by = "id", export = "last")
#'   ggsave("full-join.pdf", fj)
#'
#'   animate_full_join(x, y, by = "id", export = "gif")
#'   # to save the gif, use
#'   fj <- animate_full_join(x, y, by = "id", export = "gif")
#'   anim_save(fj, "full-join.gif")
#' }
#'
animate_full_join <- function(x, y, by, export = "gif", ...) {
  animate_join(x, y, by, type = "full_join", export = export, ...)
}

#' Animates an inner join
#'
#' @inheritParams animate_full_join
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
#' animate_inner_join(x, y, by = "id", export = "first")
#' animate_inner_join(x, y, by = "id", export = "last")
#'
#' \dontrun{
#'   # to save the ggplot, use
#'   ij <- animate_inner_join(x, y, by = "id", export = "last")
#'   ggsave("inner-join.pdf", ij)
#'
#'   animate_inner_join(x, y, by = "id", export = "gif")
#'   # to save the gif, use
#'   ij <- animate_inner_join(x, y, by = "id", export = "gif")
#'   anim_save(ij, "inner-join.gif")
#' }
#'
animate_inner_join <- function(x, y, by, export = "gif", ...) {
  animate_join(x, y, by, type = "inner_join", export = export, ...)
}

#' Animates a left join
#'
#' @inheritParams animate_full_join
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
#' animate_left_join(x, y, by = "id", export = "first")
#' animate_left_join(x, y, by = "id", export = "last")
#'
#' \dontrun{
#'   # to save the ggplot, use
#'   lj <- animate_left_join(x, y, by = "id", export = "last")
#'   ggsave("left-join.pdf", lj)
#'
#'   animate_left_join(x, y, by = "id", export = "gif")
#'   # to save the gif, use
#'   lj <- animate_left_join(x, y, by = "id", export = "gif")
#'   anim_save(lj, "left-join.gif")
#' }
#'
animate_left_join <- function(x, y, by, export = "gif", ...) {
  animate_join(x, y, by, type = "left_join", export = export, ...)
}

#' Animates a right join
#'
#' @inheritParams animate_full_join
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
#' animate_right_join(x, y, by = "id", export = "first")
#' animate_right_join(x, y, by = "id", export = "last")
#'
#' \dontrun{
#'   # to save the ggplot, use
#'   rj <- animate_right_join(x, y, by = "id", export = "last")
#'   ggsave("right-join.pdf", rj)
#'
#'   animate_right_join(x, y, by = "id", export = "gif")
#'   # to save the gif, use
#'   rj <- animate_right_join(x, y, by = "id", export = "gif")
#'   anim_save(rj, "right-join.gif")
#' }
#'
animate_right_join <- function(x, y, by, export = "gif", ...) {
  animate_join(x, y, by, type = "right_join", export = export, ...)
}

#' Animates a semi join
#'
#' @inheritParams animate_full_join
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
#' animate_semi_join(x, y, by = "id", export = "first")
#' animate_semi_join(x, y, by = "id", export = "last")
#'
#' \dontrun{
#'   # to save the ggplot, use
#'   sj <- animate_semi_join(x, y, by = "id", export = "last")
#'   ggsave("semi-join.pdf", sj)
#'
#'   animate_semi_join(x, y, by = "id", export = "gif")
#'   # to save the gif, use
#'   sj <- animate_semi_join(x, y, by = "id", export = "gif")
#'   anim_save(sj, "semi-join.gif")
#' }
#'
animate_semi_join <- function(x, y, by, export = "gif", ...) {
  animate_join(x, y, by, type = "semi_join", export = export, ...)
}

#' Animates an anti join
#'
#' @inheritParams animate_full_join
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
#' animate_anti_join(x, y, by = "id", export = "first")
#' animate_anti_join(x, y, by = "id", export = "last")
#'
#' \dontrun{
#'   # to save the ggplot, use
#'   aj <- animate_anti_join(x, y, by = "id", export = "last")
#'   ggsave("anti-join.pdf", aj)
#'
#'   animate_anti_join(x, y, by = "id", export = "gif")
#'   # to save the gif, use
#'   aj <- animate_anti_join(x, y, by = "id", export = "gif")
#'   anim_save(aj, "anti-join.gif")
#' }
#'
animate_anti_join <- function(x, y, by, export = "gif", ...) {
  animate_join(x, y, by, type = "anti_join", export = export, ...)
}
