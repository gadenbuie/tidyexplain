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

# Data ----
set.seed(42)
wide <- tibble(
  id = rep(1:2),
  x = letters[1:2],
  y = letters[3:4],
  z = letters[5:6]
)

long <- tidyr::gather(wide, key, val, x:z)
