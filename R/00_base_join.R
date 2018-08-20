# Animated dplyr joins with gganimate
# * Garrick Aden-Buie
# * garrickadenbuie.com
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

plot_data_join <- function(x, title = "", xlims = xlim(0.5, 5.5), ylims = ylim(-3.5, -0.5)) {
  plot_data(x, title) +
    xlims + ylims
}

# Data ----
x <- data_frame(
  id = 1:3,
  x = paste0("x", 1:3)
)

y <- data_frame(
  id = (1:4)[-3],
  y = paste0("y", (1:4)[-3])
)

initial_join_dfs <- proc_data(x, "x") %>%
  bind_rows(mutate(proc_data(y, "y"), .x = .x + 3)) %>%
  mutate(frame = 1)
