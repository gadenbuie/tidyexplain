library(tidyexplain)
library(here)
library(stringr)
set_font_size(title_size = 20)

check_and_create <- function(ff) {
  if (!dir.exists(ff)) dir.create(ff, recursive = T)
}

check_and_create(here("images", "static", "png"))
check_and_create(here("images", "static", "svg"))
check_and_create(here("images", "gif"))

### Animate Joins

x <- dplyr::data_frame(
  id = 1:3,
  x = paste0("x", 1:3)
)

y <- dplyr::data_frame(
  id = (1:4)[-3],
  y = paste0("y", (1:4)[-3])
)

joins <- c(
  full_join = animate_full_join,
  inner_join = animate_inner_join,
  left_join = animate_left_join,
  right_join = animate_right_join,
  semi_join = animate_semi_join
)

a <- sapply(1:length(joins), function(i) {
  nam <- names(joins)[i]
  nam <- str_replace(nam, "_", "-")
  cat(nam, "\n")

  width <- 7
  height <- 7

  gif_ <- joins[[i]](x, y, by = "id")
  first_ <- joins[[i]](x, y, by = "id", export = "first")
  last_ <- joins[[i]](x, y, by = "id", export = "last")

  save_animation(animate(gif_), here("images", "gif", paste0(nam, ".gif")))
  ggsave(here("images", "static", "png", paste0(nam, "-first.png")), first_,
         height = height, width = width)
  ggsave(here("images", "static", "svg", paste0(nam, "-first.svg")), first_,
         height = height, width = width)
  ggsave(here("images", "static", "png", paste0(nam, "-last.png")), last_,
         height = height, width = width)
  ggsave(here("images", "static", "svg", paste0(nam, "-last.svg")), last_,
         height = height, width = width)
})

### Animate Sets

x <- tibble::tribble(
  ~x,    ~y,
  "1",  "a",
  "1",  "b",
  "2",  "a"
)

y <- tibble::tribble(
  ~x, ~y,
  "1",  "a",
  "2",  "b"
)

sets <- c(
  union = animate_union,
  union_all = animate_union_all,
  intersect = animate_intersect,
  setdiff = animate_setdiff
)

a <- sapply(1:length(sets), function(i) {
  nam <- names(sets)[i]
  nam <- str_replace(nam, "_", "-")

  cat(nam, "\n")

  width <- 7
  height <- 7

  gif_ <- sets[[i]](x, y)
  first_ <- sets[[i]](x, y, export = "first")
  last_ <- sets[[i]](x, y, export = "last")

  save_animation(animate(gif_), here("images", "gif", paste0(nam, ".gif")))
  ggsave(here("images", "static", "png", paste0(nam, "-first.png")), first_,
         height = height, width = width)
  ggsave(here("images", "static", "svg", paste0(nam, "-first.svg")), first_,
         height = height, width = width)
  ggsave(here("images", "static", "png", paste0(nam, "-last.png")), last_,
         height = height, width = width)
  ggsave(here("images", "static", "svg", paste0(nam, "-last.svg")), last_,
         height = height, width = width)
})


### Animate Gather Spread
set_font_size(text_size = 4)
set_anim_options(anim_options(cell_width = 2))

# Gather
wide <- dplyr::data_frame(
  year = 2010:2011,
  Alice = c(105, 110),
  Bob = c(100, 97),
  Charlie = c(90, 95)
)

nam <- "gather"
cat(nam, "\n")

width <- 7
height <- 7

gif_ <- animate_gather(wide, key = "person", value = "sales", -year, cell_width = 2)
first_ <- animate_gather(wide, key = "person", value = "sales", -year, export = "first")
last_ <- animate_gather(wide, key = "person", value = "sales", -year, export = "last")

save_animation(animate(gif_), here("images", "gif", paste0(nam, ".gif")))
ggsave(here("images", "static", "png", paste0(nam, "-first.png")), first_,
       height = height, width = width)
ggsave(here("images", "static", "svg", paste0(nam, "-first.svg")), first_,
       height = height, width = width)
ggsave(here("images", "static", "png", paste0(nam, "-last.png")), last_,
       height = height, width = width)
ggsave(here("images", "static", "svg", paste0(nam, "-last.svg")), last_,
       height = height, width = width)

# Spread
long <- dplyr::data_frame(
  year = c(2010, 2011, 2010, 2011, 2010, 2011),
  person = c("Alice", "Alice", "Bob", "Bob", "Charlie", "Charlie"),
  sales = c(105, 110, 100, 97, 90, 95)
)
nam <- "spread"
cat(nam, "\n")

width <- 7
height <- 7

gif_ <- animate_spread(long, key = "person", value = "sales")
first_ <- animate_spread(long, key = "person", value = "sales", export = "first")
last_ <- animate_spread(long, key = "person", value = "sales", export = "last")

save_animation(animate(gif_), here("images", "gif", paste0(nam, ".gif")))
ggsave(here("images", "static", "png", paste0(nam, "-first.png")), first_,
       height = height, width = width)
ggsave(here("images", "static", "svg", paste0(nam, "-first.svg")), first_,
       height = height, width = width)
ggsave(here("images", "static", "png", paste0(nam, "-last.png")), last_,
       height = height, width = width)
ggsave(here("images", "static", "svg", paste0(nam, "-last.svg")), last_,
       height = height, width = width)
