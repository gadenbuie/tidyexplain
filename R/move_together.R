
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
  x_cols <- lhs %>% distinct(.col)
  y_cols <- rhs %>% distinct(.col)

  # separate header columns from ids and treat them as columns
  x_ids <- lhs %>% distinct(.id, .id_long)
  y_ids <- rhs %>% distinct(.id, .id_long)

  x_headers <- x_ids %>% filter(str_detect(.id_long, "^\\.header"))
  y_headers <- y_ids %>% filter(str_detect(.id_long, "^\\.header"))

  x_ids <- x_ids %>% filter(!str_detect(.id_long, "^\\.header"))
  y_ids <- y_ids %>% filter(!str_detect(.id_long, "^\\.header"))

  if (type == "full_join") {
    col_combiner <- dplyr::full_join
    row_combiner <- dplyr::full_join
  } else if (type == "inner_join") {
    col_combiner <- dplyr::inner_join
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
    col_combiner <- dplyr::semi_join
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
  names(xvals) <- take_cols %>% pull(.col)

  yvals <- cumsum(ifelse(str_detect(take_ids$.id_long, "^\\.header"), 0, -1))
  names(yvals) <- take_ids %>% pull(.id_long)

  take_vals <- semi_join(all, take %>% select(".id", ".col"),
                         by = c(".id", ".col")) %>%
    mutate(.alpha = 1,
           .x = xvals[.col],
           .y = yvals[.id_long])

  res <- bind_rows(
    # take,
    take_vals,
    # fade in place:
    all %>% filter(!.id_long %in% take_ids$.id_long) %>% mutate(.alpha = 0),
    # moving fade or fade in place as well:
    all %>% filter(.id_long %in% take_ids$.id_long & !.col %in% take_cols$.col) %>%
      mutate(.alpha = 0)
  )
  return(res)
}
