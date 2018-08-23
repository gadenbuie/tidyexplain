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
  q <- quos(...)
  sapply(q, function(i) as.character(i[[2]]))
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
dput_parser <- function(x) {
  ifelse(length(x) == 1,
         sprintf("'%s'", x),
         paste0("c(",
                paste(sprintf("'%s'", x), collapse = ", "),
                ")"))
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

  x %>% mutate(.color = ifelse(.id_map == ".header" & !.val %in% key_values,
                               color_header,
                               color_dict[.type]))
}

#' Processes a wide dataframe and converts it into a dataset that can be plotted
#'
#' @param x a wide data frame
#' @param ids a vector of id-variables that are already in the tidy-format
#' @param key a vector of key-variables
#' @param color_id the color for the id-body
#' @param ...
#'
#' @return
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

  id_values <- x %>% select(one_of(ids))
  id_values <- id_values %>% gather(key = ".key_map", value = ".id_map")

  x <- x %>% mutate(.r = row_number()) %>%
    unite(one_of(ids), col = ".id_map", remove = F)

  x <- x %>%
    gather(key = ".col", value = ".val", names(x) %>% str_subset("^[^\\.]")) %>%
    mutate(.key_map = .col,
           .type = ifelse(.col %in% ids, "id", "value"),
           .val = as.character(.val),
           .x = rep(1:nc, each = nr),
           .y = -rep(1:nr, nc))

  # the .key_map == ids need to be redirected to the key-values and multiplied
  ids_key_map <- tidyr::crossing(.key_map = key_values, .col = ids)
  x <- bind_rows(
    x %>% filter(!.key_map %in% ids),
    x %>% filter(.key_map %in% ids) %>% select(-.key_map) %>% left_join(ids_key_map, by = ".col")
  )

  # due to the untidiness of the wide-data, we need to treat the keys in th header
  # specially
  key_mapper <- tidyr::crossing(id_values %>% select(.id_map),
                                .key_map = key_values) %>%
    mutate(.id_map = as.character(.id_map))

  key_frame <- data_frame(.r = 0, .col = key_values,
                          .val = key_values, .x = 1:length(key_values) + length(ids),
                          .y = 0, .type = "key", .key_map = key_values) %>%
    left_join(key_mapper, by = ".key_map")

  # add headers
  x <- x %>% bind_rows(
    data_frame(.id_map = ".header", .r = 0, .col = ids, .val = ids,
               .x = 1:length(ids), .y = 0, .type = "id", .key_map = key_values),
    key_frame,
    .
  ) %>%
    unite(.id_map, .key_map, .val, col = ".id", remove = F)

  x %>% add_color_tidyr(key_values = key_values) %>%
    mutate(.alpha = ifelse(.id_map == ".header", 1, 0.6))

}

#' Processes a long dataframe and converts it into a dataset that can be plotted
#'
#' @param x a long data frame
#' @param ids a vector of id-variables that are already in the tidy-format
#' @param key a vector of key-variables
#' @param ...
#'
#' @return
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
  x <- x %>% mutate(.r = row_number()) %>%
    unite(ids, col = ".id_map", remove = F) %>%
    unite(key, col = ".key_map", remove = F)

  key_values <- x %>% pull(key) %>% unique()

  type_dict <- c(rep("id", length(ids)), rep("key", length(key)), rep("value", length(value)))
  names(type_dict) <-  c(rep(ids, length(ids)), rep(key, length(key)), rep(value, length(value)))

  x <- x %>%
    gather(key = ".col", value = ".val", names(x) %>% str_subset("^[^\\.]")) %>%
    mutate(
      .x = rep(1:nc, each = nr),
      .y = -rep(1:nr, nc),
      .type = type_dict[.col],
      .val = as.character(.val)
    ) %>%
    bind_rows(
      tidyr::crossing(.id_map = ".header", .r = 0, .col = ids, .val = ids,
                      .x = 1:length(ids), .y = 0, .type = "id",
                      .key_map = key_values),
      data_frame(.id_map = ".header", .r = 0, .col = key, .val = key,
                 .x = 1 + 1:length(key), .y = 0, .type = "key",
                 .key_map = key_values),
      data_frame(.id_map = ".header", .r = 0, .col = value, .val = value,
                 .x = 1 + length(key) + 1:length(value), .y = 0, .type = "value",
                 .key_map = "value"),
      .
    ) %>%
    unite(.id_map, .key_map, .val, col = ".id", remove = F)

  x %>% add_color_tidyr(key_values = key_values) %>%
    mutate(.alpha = ifelse(.id_map == ".header", 1, 0.6))
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
gather_spread <- function(lhs, rhs, sequence, key_values, export, detailed, ...) {
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
  xshift <- 2

  state_start <- lhs %>% mutate(.frame = 0)
  step_0 <- lhs %>% mutate(.frame = 1)
  state_end <- rhs %>% mutate(.frame = length(key_values) + 2, .x = .x + max(lhs$.x) + xshift)

  if (detailed) {
    # take one instance of the first headers
    start_headers <- lhs %>% filter(.id_map == ".header" & !.val %in% key_values) %>%
      group_by(.col, .val) %>% slice(1) %>% ungroup()
    end_headers <- state_end %>% filter(.id_map == ".header")

    # for each unique key-value move the respective entries
    keys_to_shift <- lhs %>% filter(.key_map %in% key_values)
    keys_shifted <- lhs[0, ]
    key_steps <- lhs[0, ]
    i <- 1
    for (keyval in key_values) {
      i <- i + 1
      keys_shifted <- bind_rows(keys_shifted, filter(state_end, .key_map == keyval))

      keys_to_shift <- keys_to_shift %>% filter(.key_map != keyval)
      if (keyval == key_values[length(key_values)]) {
        # in the last round, we dont want to save the start headers
        start_headers <- NULL
      }
      round_n <- bind_rows(end_headers, start_headers,
                           keys_shifted, keys_to_shift) %>% mutate(.frame = i)

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

  } else {
    anim_df <- bind_rows(state_start, state_end)

    frame_labels <- c(
      sequence[["operation"]],
      sequence[["reverse_operation"]]
    )
    title_string <- "{ifelse(transitioning, previous_state, ifelse(grepl('gather', next_state), 'Wide', 'Long'))}"
  }

  frame_levels <- anim_df$.frame %>% unique()

  anim_df <- anim_df %>%
    mutate(.frame = factor(.frame,
                           levels = frame_levels,
                           labels = frame_labels))

  if (export == "gif") {
    animate_plot(anim_df, title = title_string) #...
  } else if (export == "first") {
    static_plot(state_start) #....
  } else if (export == "last") {
    static_plot(state_end) #....
  }

  # open issues: ... doesnt work properly.
  # especially if the id-arguments are passed in the gather-style, i.e., -year, or year:var
}
