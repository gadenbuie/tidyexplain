#' @importFrom dplyr left_join right_join full_join inner_join semi_join anti_join
#' @importFrom dplyr mutate select filter arrange bind_rows bind_cols group_by pull slice data_frame row_number
#' @importFrom tidyr gather spread
#' @keywords internal
"_PACKAGE"

plot_settings <- new.env(parent = emptyenv())
plot_settings$default <- list(
  transition_length = 2,
  state_length      = 1,
  ease_default      = "sine-in-out",
  ease_other        = NULL,
  enter             = setNames(list(enter_fade()), "enter_fade()"),
  exit              = setNames(list(exit_fade()), "exit_fade()"),
  text_family       = "Fira Mono",
  title_family      = "Fira Mono",
  text_size         = 5,
  title_size        = 17
)

