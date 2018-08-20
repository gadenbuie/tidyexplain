# Animated dplyr set opertaions with gganimate
# * Contributed by Tyler Grant Smith <https://github.com/TylerGrantSmith>
# * and Garrick Aden-Buie <https://www.garrickadenbuie.com>
# * MIT License: https://opensource.org/licenses/MIT

library(tidyverse)
library(gganimate)

if (!getOption("tidy_verb_anim.font_registered", FALSE)) {
  source(here::here("R", "01_register-fonts.R"))
}

if (!getOption("tidy_verb_anim.functions_loaded", FALSE)) {
  source(here::here("R", "02_functions.R"))
}

source(here::here("R", "03_check-folders.R"))

# Initialize data processing function ----

proc_data_set <- function(x, .id = "x") {
  proc_data(x, .id, colorize_row_id, "before")
}

plot_data_set <- function(x, title = "", xlims = xlim(1.5, 6.5), ylims = ylim(-3.5, -0.5)) {
  filter(x, label != "id") %>%
  plot_data(title) +
  xlims + ylims
}

# Data ----

x <- tibble::tribble(
  ~id,   ~x,    ~y,
  1,  "1",  "a",
  2,  "1",  "b",
  3,  "2",  "a"
)

y <- tibble::tribble(
  ~id, ~x, ~y,
  1,  "1",  "a",
  4,  "2",  "b"
)

initial_set_dfs <- bind_rows(
  proc_data_set(x, "x"),
  proc_data_set(y, "y") %>% mutate(.x = .x + 3)
) %>%
  mutate(frame = 1)
