
#' Preprocess data
#'
#' @param x a left dataset
#' @param y a right dataset
#' @param by a by argument for joins / set operations
#'
#' @return a preprocessed dataset
#'
#' @examples
#' NULL
preprocess_data <- function(x, y, by) {

  xvars <- names(x) %>% str_subset("^[^\\.]")
  yvars <- names(y) %>% str_subset("^[^\\.]")

  x <- x %>%
    unite(one_of(by), col = ".id", remove = FALSE) %>%
    unite(one_of(xvars), col = ".id_long", remove = FALSE)

  y <- y %>%
    unite(one_of(by), col = ".id", remove = FALSE) %>%
    unite(one_of(yvars), col = ".id_long", remove = FALSE)

  ids <- unique(c(x$.id, y$.id))

  x_ <- process_data(x, ids, by) %>%
    mutate(.id_long = paste(.id_long, .side, .r, sep = "_"))
  y_ <- process_data(y, ids, by) %>%
    mutate(.x = .x + ncol(x),
           .id_long = paste(.id_long, .side, .r, sep = "_"))

  return(list(x = x_, y = y_))
}


#' Processes the data
#'
#' @param x a preprocessed dataset
#' @param ids a vector of ids
#' @param by a vector of by-arguments
#' @param width the width of the tiles
#' @param side the side (x or y, lhs or rhs, etc)
#'
#' @return a data_frame including all necessary information
#'
#' @examples
#' NULL
process_data <- function(x, ids, by, width = 1, side = NA) {
  if (is.na(side)) side <- deparse(substitute(x))

  x_names <- names(x) %>% str_subset("^[^\\.]")
  x_keys <- 1:length(x_names)
  names(x_keys) <- x_names

  special_vars <- names(x) %>% str_subset("^\\.")

  x <- x %>%
    mutate(.r = row_number()) %>%
    gather_(key = "col", value = "val", names(x) %>% str_subset("^[^.]")) %>%
    mutate(.x = x_keys[col],
           .y = -.r) %>%
    bind_rows(data_frame(.id = ".header",
                         .id_long = paste(".header", x_names, sep = "_"),
                         .r = 0, col = x_names, val = x_names,
                         .x = x_keys, .y = 0), .) %>%
    mutate(.width = width,
           .side = side)

  add_color(x, ids, by)
}

#' Adds Color to a processed data_frame
#'
#' @param x a processed data_frame
#' @param ids a vector of ids for the color-matching
#' @param by a vector of column names that constitute the by-argument of joins/sets
#' @param color_header color for the header
#' @param color_other color for "inactive" values
#' @param color_missing color for missing values
#'
#' @return the processed data_frame with a new column .color
#'
#' @examples
#' NULL
add_color <- function(x, ids, by, color_header = "#bdbdbd", color_other = "#d0d0d0", color_missing = "#ffffff") {
  colors <- c(color_header, scales::brewer_pal(type = "qual", "Set1")(length(ids)))
  names(colors) <- c(".header", ids)

  x %>%
    mutate(.color = ifelse(is.na(val), color_missing, colors[.id]),
           .color = ifelse(col %in% by, .color, color_other))
}
