#' Gets the ... names
#'
#' Used to get the -year
#'
#' @param ... arguments
#'
#' @return a vector of the names of ...
#'
#' @examples
#' x <- 1:10
#' y <- 1
#' get_quos_names(-x)
#' get_quos_names(x:y)
get_quos_names <- function(...) {
  q <- rlang::quos(...)
  purrr::map_chr(q, rlang::quo_name)
}

#' Parses a simple vector so that it looks like its input
#'
#' @param x a vector
#'
#' @return a string
#'
#' @examples
#' dput_parser("x")
#' dput_parser(c("x", "y"))
dput_parser <- function(x) UseMethod("dput_parser")

dput_parser.character <- function(x) {
  if (length(x) == 1) {
    sprintf('"%s"', x)
  } else {
    x <- capture.output(dput(x))
    paste(x, collapse = "")
  }
}

#' Adds color to processed tidy data
#'
#' @param x a processed data-frame as outputted by process_long or process_wide
#' @param key_values the unique key-values
#' @param color_fun the color function
#' @param color_header the color for the header
#' @param ... not used
#'
#' @return a data-frame with the colors
#'
#' @examples
#' NULL
add_color_tidyr <- function(x, key_values,
                            color_fun = scales::brewer_pal(type = "qual", "Set1"),
                            color_header = "#737373",
                            color_id = "#d0d0d0") {

  color_dict <- color_fun(3)
  names(color_dict) <- c("id", "key", "value")

  x %>% mutate(.color = color_dict[.type])
}

#' Processes a wide dataframe and converts it into a dataset that can be plotted
#'
#' @param x a wide data frame
#' @param ids a vector of id-variables that are already in the tidy-format
#' @param key a vector of key-variables
#' @param color_id the color for the id-body
#' @param ...
#'
#' @return TODO
#'
#' @examples
#' wide <- data_frame(
#'   year = 2010:2011,
#'   Alice = c(105, 110),
#'   Bob = c(100, 97),
#'   Charlie = c(90, 95)
#' )
#' process_wide(wide, ids = "year", key = "person")
#' process_wide(wide, ids = "year", key = "person") %>% static_plot
process_wide <- function(x, ids, key, color_id = "lightgray", ...) {

  if (!all(ids %in% names(x)))
    stop("all ids must be in x")

  nr <- nrow(x)
  nc <- ncol(x)
  key_values <- names(x)
  key_values <- key_values[!key_values %in% ids]

  id_values <- x %>% select(dplyr::one_of(ids))
  id_values <- id_values %>% tidyr::gather(key = ".key_map", value = ".id_map")

  x <- x %>% mutate(.r = row_number()) %>%
    tidyr::unite(dplyr::one_of(ids), col = ".id_map", remove = F)

  x <- x %>%
    gather(key = ".col", value = ".val", names(x)[grepl("^[^\\.]", names(x))]) %>%
    mutate(.key_map = .col,
           .type = ifelse(.col %in% ids, "id", "value"),
           .val = as.character(.val),
           .x = rep(1:nc, each = nr),
           .y = -rep(1:nr, nc),
           .header = F)

  # make sure that we have one id value per key
  tmp <- x %>% filter(.key_map %in% ids)
  x <- bind_rows(
    left_join(tmp %>% select(-.key_map),
              tmp %>% select(.id_map) %>% tidyr::crossing(.key_map = key_values),
              by = ".id_map"),
    x %>% filter(!.key_map %in% ids)
  )

  # add header:
  crosser <- tidyr::crossing(.id_map = as.character(id_values$.id_map),
                      .key_map = key_values)
  key_header <- data_frame(
    .key_map = key_values,
    .r = 0,
    .col = key_values,
    .val = key_values,
    .type = "key",
    .x = length(ids) + 1:length(key_values),
    .y = 0,
    .header = TRUE) %>%
    left_join(crosser, by = ".key_map")

  id_header <- left_join(
    data_frame(.id_map = ids,
               .r = 0,
               .col = ids,
               .val = ids,
               .type = "id",
               .x = 1:length(ids),
               .y = 0,
               .header = TRUE),
    tidyr::crossing(.id_map = ids, .key_map = key_values),
    by = ".id_map"
  )

  x <- bind_rows(id_header, key_header, x)

  x <- x %>% tidyr::unite(.key_map, .id_map, .val, col = ".id", remove = F)

  x %>%
    add_color_tidyr(key_values = key_values) %>%
    mutate(.alpha = ifelse(.header == TRUE, 1, 0.6))
}

#' Processes a long dataframe and converts it into a dataset that can be plotted
#'
#' @param x a long data frame
#' @param ids a vector of id-variables that are already in the tidy-format
#' @param key a vector of key-variables
#' @param ...
#'
#' @return TODO
#'
#' @examples
#' long <- data_frame(
#'   year = c(2010L, 2011L, 2010L, 2011L, 2010L, 2011L),
#'   person = c("Alice", "Alice", "Bob", "Bob", "Charlie", "Charlie"),
#'   sales = c(105, 110, 100, 97, 90, 95)
#' )
#' process_long(long, ids = "year", key = "person", value = "sales")
#' process_long(long, ids = "year", key = "person", value = "sales") %>% static_plot
process_long <- function(x, ids, key, value, ...) {

  if (!all(c(ids, key, value) %in% names(x)))
    stop("all ids, key, and value must be names of x")

  nr <- nrow(x)
  nc <- ncol(x)
  xn <- names(x)

  x <- x %>% mutate(.r = row_number()) %>%
    tidyr::unite(ids, col = ".id_map", remove = F) %>%
    tidyr::unite(key, col = ".key_map", remove = F)

  key_values <- x %>% pull(key) %>% unique()

  type_dict <- c(rep("id", length(ids)), rep("key", length(key)), rep("value", length(value)))
  names(type_dict) <-  c(ids, key, value)

  x_dict <- 1:nc
  names(x_dict) <- xn

  x <- x %>%
    tidyr::gather(key = ".col", value = ".val", names(x)[grepl("^[^\\.]", names(x))]) %>%
    mutate(
      .x = x_dict[.col],
      .y = -rep(1:nr, nc),
      .type = type_dict[.col],
      .val = as.character(.val),
      .header = FALSE
    )

  # add headers:

  id_headers <- tidyr::crossing(.id_map = ids, # x$.id_map %>% unique()
                                .key_map = key_values,
  ) %>%
    mutate(
      .r = 0,
      .col = "id",
      .val = .id_map,
      .x = x_dict[.val],
      .y = 0,
      .type = "id",
      .header = TRUE
  )

  x <- x %>%
    dplyr::add_row(
      .before = T,
      .id_map = c(rep("key", length(key)), rep("value", length(value))),
      .key_map = c(rep("key", length(key)), rep("value", length(value))),
      .r = 0,
      .col = c(rep("key", length(key)), rep("value", length(value))),
      .val = c(key, value),
      .x = length(ids) + 1:length(c(key, value)),
      .y = 0,
      .type = c(rep("key", length(key)), rep("value", length(value))),
      .header = TRUE
    )

  x <- bind_rows(id_headers, x)

  x <- x %>%
    tidyr::unite(.key_map, .id_map, .val, col = ".id", remove = F)

  x %>% add_color_tidyr(key_values = key_values) %>%
    mutate(.alpha = ifelse(.header == TRUE, 1, 0.6))
}

#' Animates a gather or spread function
#'
#' internally used by animate_spread and animate_gather
#'
#' @param lhs the (processed) dataset on the left-side
#' @param rhs the (processed) dataset on the right-side
#' @param sequence a named vector of the sequence titles
#'          (current_state, final_state, operation, and reverse_operation)
#' @param key_values the unique key-values
#' @param export the export type, either gif, first or last. The latter two
#'              export ggplots of the first/last state of the join
#' @param detailed boolean value if the animation should show one step for each
#'              key value
#' @param ... further arguments passed to animate_plot
#'
#' @return the plot or the gif
#'
#' @examples
#' NULL
gather_spread <- function(lhs, rhs, sequence, key_values, export, detailed, ..., anim_opts = anim_options()) {
  # lhs is the one state of the df
  # rhs is the target state

  # animate the four steps: inital with sequence[["current_state]],
  # transformations by the unique key-values with sequence[["operation"]],
  # final with sequence[["final_state"]]
  # and back transformation with sequence[["reverse_operation]]

  # have lhs and rhs in the right format: preprocessed with ids, .x, .y etc.
  # have a color function that makes coloring easier
  # transformations: for each key-variable: respective ids "fly in", keys fly in and ids fly in (all in one step for one key. i.e., Alice)

  # how much is the rhs to the left of lhs?

  if (!detailed) {
    anim_df <- bind_rows(
      lhs %>% mutate(.frame = 0),
      rhs %>% mutate(.frame = 1)
    )
    frame_labels <- c(sequence[["operation"]], sequence[["reverse_operation"]])

    title_string <- "{ifelse(transitioning, previous_state, ifelse(grepl('gather', next_state), 'Wide', 'Long'))}"

    tl <- 2
    sl <- 1

  } else {
    xshift <- 2

    rhs <- rhs %>% mutate(.x = .x + max(lhs$.x) + xshift)
    # the header rows
    header_start <- lhs %>% filter(.header == TRUE, !.key_map %in% key_values)
    header_end <- rhs %>% filter(.header == TRUE)

    state_start <- lhs %>% mutate(.frame = 0)
    state_end <- rhs %>% mutate(.frame = length(key_values) + 2)

    step_0 <- lhs %>% mutate(.frame = 1)
    # for each unique key-value move the respective entries
    keys_remaining <- lhs %>% filter(.key_map %in% key_values)
    keys_shifted <- lhs[0, ]
    key_steps <- lhs[0, ]
    f <- 1
    ids_remaining <- lhs %>% filter(.type == "id" & .header == FALSE)

    for (keyval in key_values) {
      f <- f + 1
      move_rhs <- rhs %>% filter(.key_map == keyval)

      keys_remaining <- keys_remaining %>% filter(.key_map != keyval)

      if (keyval == key_values[length(key_values)]) {
        header_start <- NULL
      }
      hd <- header_end %>% filter(.key_map == keyval |
                                    (.type %in% c("key", "value") &
                                       .col %in% c("key", "value")))
      keys_shifted <- bind_rows(keys_shifted, move_rhs)
      round_n <- bind_rows(header_start, hd,
                           keys_remaining, keys_shifted) %>%
        mutate(.frame = f)

      key_steps <- bind_rows(key_steps, round_n)
    }

    anim_df <- bind_rows(state_start, step_0, key_steps, state_end)

    # form the .frame as proper factors
    frame_labels <- c(
      sequence[["current_state"]],
      paste(sequence[["operation"]], key_values),
      sequence[["final_state"]],
      sequence[["reverse_operation"]]
    )
    title_string <- "{gsub('\\\\) [a-zA-Z]+$', ')', previous_state)}"

    tl <- length(unique(anim_df$.frame)) * 2
    sl <- 1
  }

  frame_levels <- anim_df$.frame %>% unique()

  anim_df <- anim_df %>%
    mutate(.frame = factor(.frame,
                           levels = frame_levels,
                           labels = frame_labels))

  if (export == "gif") {
    animate_plot(anim_df, title = title_string, anim_opts = anim_opts)
  } else if (export == "first") {
    static_plot(state_start, anim_opts = anim_opts) #....
  } else if (export == "last") {
    static_plot(state_end, anim_opts = anim_opts) #....
  }

  # open issues: ... doesnt work properly.
  # especially if the id-arguments are passed in the gather-style, i.e., -year, or year:var
}
