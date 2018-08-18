# Animated dplyr joins with gganimate
# * Garrick Aden-Buie
# * garrickadenbuie.com
# * MIT License: https://opensource.org/licenses/MIT

# Note: I used Fira Sans and Fira Mono fonts.
#       Use search and replace to use a different font if Fira is not available.

library(tidyverse)
library(gganimate)

if (!getOption("tidy_verb_anim.font_registered", FALSE)) {
  source(here::here("R", "01_register-fonts.R"))
}

if (!getOption("tidy_verb_anim.functions_loaded", FALSE)) {
  source(here::here("R", "02_functions.R"))
}

if (!dir.exists(here::here("images"))) dir.create(here::here("images"))

# Data ----

x <- tibble::tribble(
  ~id,   ~x,    ~y,
  1,  "x1",  "y1",
  2,  "x1",  "y2",
  3,  "x2",  "y1"
)

y <- tibble::tribble(
  ~id, ~x, ~y,
  1,  "x1",  "y1",
  4,  "x2",  "y2"
)

initial_dfs <- proc_data2(x, "x") %>%
  bind_rows(mutate(proc_data2(y, "y"), .x = .x + 4)) %>%
  mutate(frame = 1)
