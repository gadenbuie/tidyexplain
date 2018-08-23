
#' Animates the gather function
#'
#' @param w a data_frame in the wide format
#' @param key the key
#' @param value the value
#' @param ... further arguments passed to gather, static_plot, or animate_plot
#' @param export the export type, either gif, first or last. The latter two
#'          export ggplots of the first/last state of the gather function
#' @param detailed boolean value if the animation should show one step for each
#'              key value
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
animate_gather <- function(w, key, value, ..., export = "gif", detailed = TRUE) {
  lhs <- w
  rhs <- tidyr::gather(w, !!key, !!value, ...)

  # construct the title sequence
  lname <- deparse(substitute(w))
  ids <- get_quos_names(...)
  # ids <- ""
  # what happens if ids := -year or ids := x:y

  # the case that ... contains two -arguments. i.e., -year, -region
  ids <- ids[2, ]
  ids <- ids[!ids %in% c(key, value)]
  ids <- ids[ids != "-"]

  id_string <- paste0(", ", paste(sprintf("-%s", ids), collapse = ", "))

  sequence <- c(
    current_state = "Wide",
    final_state = "Long",
    operation = sprintf("gather(%s, %s, %s%s)",
                        lname,
                        dput_parser(key),
                        dput_parser(value),
                        id_string),
    reverse_operation = sprintf("spread(%s, %s, %s)",
                                "long_df",
                                dput_parser(key),
                                dput_parser(value))
  )

  key_values <- rhs %>% pull(key) %>% unique()
  lhs_proc <- process_wide(lhs, ids, key, key_values, value, ...)
  rhs_proc <- process_long(rhs, ids, key, value, ...)

  gather_spread(lhs_proc, rhs_proc, sequence = sequence, key_values = key_values,
                export = export, detailed = detailed, ...)
}


#' Animates the spread function
#'
#' @param l a data_frame in the long/tidy format
#' @param key the key
#' @param value the values
#' @param export the export type, either gif, first or last. The latter two
#'              export ggplots of the first/last state of the spread function
#' @param detailed boolean value if the animation should show one step for each
#'              key value
#' @param ... further arguments passed to static_plot
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
animate_spread <- function(l, key, value, export = "gif", detailed = TRUE, ...) {

  lhs <- l
  rhs <- tidyr::spread(l, key = key, value = value)

  # construct the title sequence
  lname <- deparse(substitute(l))
  ids <- names(lhs)
  ids <- ids[!ids %in% c(key, value)]

  id_string <- paste0(", ", paste(sprintf("-%s", ids), collapse = ", "))

  sequence <- c(
    current_state = "Long",
    final_state = "Wide",
    operation = sprintf("spread(%s, %s, %s)",
                        "long_df",
                        dput_parser(key),
                        dput_parser(value)),
    reverse_operation = sprintf("gather(%s, %s, %s%s)",
                                lname,
                                dput_parser(key),
                                dput_parser(value),
                                id_string)
  )

  lhs_proc <- process_long(lhs, ids, key, value, ...)
  rhs_proc <- process_wide(rhs, ids, key, value, ...)

  key_values <- lhs %>% pull(key) %>% unique()
  gather_spread(lhs_proc, rhs_proc, sequence, key_values, export, detailed, ...)
}
