
#' Animates the gather function
#'
#' @param w a data_frame in the wide format
#' @param key the key
#' @param value the value
#' @param ... further arguments passed to [tidyr::gather()], [process_wide()],
#'   or [process_long()]
#' @param detailed boolean value if the animation should show one step for each
#'   key value
#' @inheritParams animate_join
#' @inheritParams anim_options
#'
#' @return a gif or a ggplot
#' @export
#'
#' @examples
#' wide <- data_frame(
#'   year = 2010:2011,
#'   Alice = c(105, 110),
#'   Bob = c(100, 97),
#'   Charlie = c(90, 95)
#' )
#' animate_gather(wide, "person", "sales", -year, export = "first")
#' animate_gather(wide, "person", "sales", -year, export = "last")
#'
#' \donttest{
#'   animate_gather(wide, "person", "sales", -year, export = "gif")
#'   # if you want to have a less detailed animation, you can also use
#'   animate_gather(wide, "person", "sales", -year, export = "gif", detailed = FALSE)
#' }
animate_gather <- function(w, key, value, ..., export = "gif", detailed = TRUE, anim_opts = anim_options()) {
  lhs <- w
  rhs <- tidyr::gather(w, !!key, !!value, ...)

  # construct the title sequence
  wname <- deparse(substitute(w))
  tidyr_selection <- get_quos_names(...)
  ids <- setdiff(colnames(w), tidyselect::vars_select(colnames(w), ...))

  id_string <- paste0(", ", paste(sprintf("%s", tidyr_selection), collapse = ", "))

  sequence <- c(
    current_state = "wide",
    final_state = "long",
    operation = sprintf("gather(%s, %s, %s%s)",
                        wname,
                        dput_parser(key),
                        dput_parser(value),
                        id_string),
    reverse_operation = sprintf("spread(%s, %s, %s)",
                                "long",
                                dput_parser(key),
                                dput_parser(value))
  )

  key_values <- rhs %>% pull(key) %>% unique()
  lhs_proc <- process_wide(lhs, ids, key, key_values, value, ...)
  rhs_proc <- process_long(rhs, ids, key, value, ...)

  gather_spread(lhs_proc, rhs_proc, sequence = sequence, key_values = key_values,
                export = export, detailed = detailed, ..., anim_opts = anim_opts)
}


#' Animates the spread function
#'
#' @param l a data_frame in the long/tidy format
#' @param ... further arguments passed to [process_long] or [process_wide]
#' @inheritParams animate_gather
#' @inheritParams animate_join
#' @inheritParams anim_options
#'
#' @return a ggplot or a gif
#' @export
#'
#' @examples
#' long <- data_frame(
#'   year = c(2010L, 2011L, 2010L, 2011L, 2010L, 2011L),
#'   person = c("Alice", "Alice", "Bob", "Bob", "Charlie", "Charlie"),
#'   sales = c(105, 110, 100, 97, 90, 95)
#' )
#' animate_spread(long, key = "person", value = "sales", export = "first")
#' animate_spread(long, key = "person", value = "sales", export = "last")
#'
#' \donttest{
#'   animate_spread(long, key = "person", value = "sales", export = "gif")
#'   # if you want to have a less detailed animation, you can also use
#'   animate_spread(long, key = "person", value = "sales", export = "gif", detailed = FALSE)
#' }
animate_spread <- function(l, key, value, export = "gif", detailed = TRUE, ..., anim_opts = anim_options()) {

  lhs <- l
  rhs <- tidyr::spread(l, key = key, value = value)

  # construct the title sequence
  lname <- deparse(substitute(l))
  ids <- names(lhs)
  ids <- ids[!ids %in% c(key, value)]

  id_string <- paste0(", ", paste(sprintf("-%s", ids), collapse = ", "))

  sequence <- c(
    current_state = "long",
    final_state = "wide",
    operation = sprintf("spread(%s, %s, %s)",
                        lname,
                        dput_parser(key),
                        dput_parser(value)),
    reverse_operation = sprintf("gather(%s, %s, %s%s)",
                                "wide",
                                dput_parser(key),
                                dput_parser(value),
                                id_string)
  )

  lhs_proc <- process_long(lhs, ids, key, value, ...)
  rhs_proc <- process_wide(rhs, ids, key, value, ...)

  key_values <- lhs %>% pull(key) %>% unique()
  gather_spread(lhs_proc, rhs_proc, sequence, key_values, export, detailed, ..., anim_opts = anim_opts)
}
