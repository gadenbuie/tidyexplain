
#' Combines two processed datasets and combines them for a given method
#'
#' @param lhs the left-hand side dataset
#' @param rhs the righ-hand side dataset
#' @param type a string of the desired combination method, allowed are all dplyr
#'             joins or sets
#'
#' @return  processed dataset of the combined values
#'
#' @examples
#' NULL
move_together <- function(lhs, rhs, type) {

  all <- bind_rows(lhs, rhs)

  # separate column and row-filter (ids)
  x_cols <- dplyr::distinct(lhs, .col)
  y_cols <- dplyr::distinct(rhs, .col)

  # separate header columns from ids and treat them as columns
  x_ids <- dplyr::distinct(lhs, .id, .id_long)
  y_ids <- dplyr::distinct(rhs, .id, .id_long)

  x_headers <- filter(x_ids, grepl("^\\.header", .id_long))
  y_headers <- filter(y_ids, grepl("^\\.header", .id_long))

  x_ids <- x_ids %>% filter(!grepl("^\\.header", .id_long))
  y_ids <- y_ids %>% filter(!grepl("^\\.header", .id_long))

  # assign two combiner functions depending on the type
  # one for combining the columns (col_combiner)
  # one for combining the rows (row_combiner)
  if (type == "full_join") {
    col_combiner <- dplyr::full_join
    row_combiner <- dplyr::full_join
  } else if (type == "inner_join") {
    col_combiner <- dplyr::full_join
    row_combiner <- dplyr::inner_join
  } else if (type == "left_join") {
    col_combiner <- dplyr::full_join
    row_combiner <- dplyr::left_join
  } else if (type == "right_join") {
    col_combiner <- dplyr::full_join
    row_combiner <- dplyr::right_join
  } else if (type == "semi_join") {
    col_combiner <- dplyr::left_join
    row_combiner <- dplyr::semi_join
  } else if (type == "anti_join") {
    col_combiner <- dplyr::left_join
    row_combiner <- dplyr::anti_join
  } else if (type == "union") {
    col_combiner <- dplyr::full_join
    row_combiner <- dplyr::union
  } else if (type == "union_all") {
    col_combiner <- dplyr::full_join
    row_combiner <- dplyr::union_all
  } else if (type == "intersect") {
    col_combiner <- dplyr::full_join
    row_combiner <- dplyr::intersect
  } else if (type == "setdiff") {
    col_combiner <- dplyr::full_join
    row_combiner <- dplyr::anti_join
  } else if (type == "bind_rows") {
    col_combiner <- dplyr::full_join
    row_combiner <- dplyr::bind_rows
  } else if (type == "bind_cols") {
    col_combiner <- dplyr::full_join
    row_combiner <- dplyr::left_join
  } else {
    stop("Unknown func")
  }

  take_cols <- col_combiner(x_cols, y_cols, by = ".col")
  take_ids  <- row_combiner(x_ids, y_ids, by = c(".id", ".id_long"))
  take_headers <- col_combiner(x_headers, y_headers, by = c(".id", ".id_long"))

  take_ids <- bind_rows(take_headers, take_ids)

  take <- tidyr::crossing(take_ids, take_cols)

  mid <- (2 + length(unique(lhs$.col)) + length(unique(rhs$.col))) / 2
  xvals <- 1:nrow(take_cols)
  xvals <- xvals - mean(xvals) + mid
  names(xvals) <- pull(take_cols, .col)

  yvals <- cumsum(ifelse(grepl("^\\.header", take_ids$.id_long), 0, -1))
  names(yvals) <- pull(take_ids, .id_long)

  take_vals <- semi_join(all, take %>% select(".id", ".col"),
                         by = c(".id", ".col")) %>%
    mutate(.alpha = 1,
           .x = xvals[.col],
           .y = yvals[.id_long])

  bind_rows(
    # take,
    take_vals,
    # fade in place:
    all %>% filter(!.id_long %in% take_ids$.id_long) %>% mutate(.alpha = 0),
    # moving fade or fade in place as well:
    all %>% filter(.id_long %in% take_ids$.id_long & !.col %in% take_cols$.col) %>%
      mutate(.alpha = 0)
  )
}
