
#' Preprocess data
#'
#' @param x a left dataset
#' @param y a right dataset
#' @param by a by argument for joins / set operations
#' @param fill if missing ids should be filled
#' @param ... further arguments passed to add_color
#'
#' @return a preprocessed dataset
#'
#' @examples
#' NULL
preprocess_data <- function(x, y, by, fill = TRUE, ...) {

  #' test for
  #' a <- c("unique", "mult", "mult", "also unique")
  #' add_duplicate_number(a)
  add_duplicate_number <- function(a) {
    data_frame(v = a) %>%
      group_by(v) %>%
      mutate(id = paste(v, 1:n(), sep = "-")) %>%
      pull(id)
  }

  x <- x %>%
    unite(one_of(by), col = ".id", remove = FALSE) %>%
    mutate(.id_long = add_duplicate_number(.id))

  y <- y %>%
    unite(one_of(by), col = ".id", remove = FALSE)  %>%
    mutate(.id_long = add_duplicate_number(.id))

  ids <- dplyr::union(x %>% dplyr::select(.id, .id_long),
                      y %>% dplyr::select(.id, .id_long))

  x_ <- process_data(x, ids, by, fill = fill, ...)
  y_ <- process_data(y, ids, by, fill = fill, ...) %>%
    mutate(.x = .x + ncol(x) - 1)

  return(list(x = x_, y = y_))
}


#' Processes the data
#'
#' @param x a preprocessed dataset
#' @param ids a data_frame of ids (.id and .id_long)
#' @param by a vector of by-arguments
#' @param width the width of the tiles
#' @param side the side (x or y, lhs or rhs, etc)
#' @param fill if missing ids should be filled
#' @param ... further arguments passed to add_color
#'
#' @return a data_frame including all necessary information
#'
#' @examples
#' NULL
process_data <- function(x, ids, by, width = 1, side = NA, fill = TRUE, ...) {
  if (is.na(side)) side <- deparse(substitute(x))

  x_names <- names(x) %>% str_subset("^[^\\.]")
  x_keys <- 1:length(x_names)
  names(x_keys) <- x_names

  special_vars <- names(x) %>% str_subset("^\\.")

  x <- x %>%
    mutate(.r = row_number()) %>%
    gather_(key = ".col", value = ".val", names(x) %>% str_subset("^[^.]")) %>%
    mutate(.x = x_keys[.col],
           .y = -.r) %>%
    bind_rows(data_frame(.id = ".header",
                         .id_long = paste(".header", x_names, sep = "_"),
                         .r = 0,
                         .col = x_names,
                         .val = x_names,
                         .x = x_keys, .y = 0), .) %>%
    mutate(.width = width,
           .side = side)

  # if there are multiple values in the ids (-2, -3 etc) but they are not present
  # in x, because it is in the second/other dataset, add these values here
  id_long <- ids$.id_long
  mis_ids <- id_long[!id_long %in% x$.id_long]
  # if the missing value is a -1, that means the missing value comes not from
  # missing dublicate ids
  mis_ids <- str_subset(mis_ids, "[^-1]$")
  if (length(mis_ids) > 0 && fill) {
    mis_ids_short <- str_replace(mis_ids, "-[0-9]+$", "")

    # insert the missing ids at the right place
    for (i in mis_ids_short) {
      irow <- (1:nrow(x))[x$.id == i]
      irow <- irow[1]
      x <- bind_rows(
        x %>% slice(1:irow),
        x %>% filter(.id %in% mis_ids_short) %>% mutate(.id_long = mis_ids),
        x %>% slice((irow + 1):nrow(x))
      )
    }
  }

  res <- add_color(x, rev(ids$.id), by, ...)
  return(res)
}

#' Adds Color to a processed data_frame
#'
#' @param x a processed data_frame
#' @param ids a vector of ids for the color-matching
#' @param by a vector of column names that constitute the by-argument of joins/sets
#' @param color_header color for the header
#' @param color_other color for "inactive" values
#' @param color_missing color for missing values
#' @param color_fun the function to generate the colors
#' @param ...
#'
#' @return the processed data_frame with a new column .color
#'
#' @examples
#' NULL
add_color <- function(x, ids, by,
                      color_header = "#737373", color_other = "#d0d0d0",
                      color_missing = "#ffffff",
                      color_fun = scales::brewer_pal(type = "qual", "Set1"), ...) {
  colors <- c(color_header, color_fun(length(ids)))
  names(colors) <- c(".header", ids)

  res <- x %>%
    mutate(
      .color = ifelse(is.na(.val),
                      color_missing,
                      ifelse(.col %in% by,
                             colors[.id],
                             color_other)),
      .color = ifelse(.id == ".header", color_header, .color))
  return(res)
}
