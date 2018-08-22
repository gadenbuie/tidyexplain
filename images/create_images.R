library(tidyAnimatedVerbs)
library(here)

check_and_create <- function(ff) {
  if (!dir.exists(ff)) dir.create(ff, recursive = T)
}

x <- data_frame(
  id = 1:3,
  x = paste0("x", 1:3)
)

y <- data_frame(
  id = (1:4)[-3],
  y = paste0("y", (1:4)[-3])
)


check_and_create(here("images", "static", "png"))

joins <- c(full_join = animate_full_join,
           inner_join = animate_inner_join,
           left_join = animate_left_join,
           right_join = animate_right_join,
           semi_join = animate_right_join)

a <- sapply(1:length(joins), function(i) {
  nam <- names(joins)[i]
  nam <- str_replace(nam, "_", "-")

  width <- 7
  height <- 7

  gif_ <- joins[[i]](x, y, by = "id")
  first_ <- joins[[i]](x, y, by = "id", export = "first")
  last_ <- joins[[i]](x, y, by = "id", export = "last")

  save_animation(gif_, here("images", paste0(nam, ".gif")))
  ggsave(here("images", "static", "png", paste0(nam, "-first.png")), first_,
         height = height, width = width)
  ggsave(here("images", "static", "svg", paste0(nam, "-first.svg")), first_,
         height = height, width = width)
  ggsave(here("images", "static", "png", paste0(nam, "-last.png")), last_,
         height = height, width = width)
  ggsave(here("images", "static", "svg", paste0(nam, "-last.svg")), last_,
         height = height, width = width)
})


# instr_extra <- instr %>% slice(c(1, 1:n()))
# animate_left_join(singer, instr_extra, by = c("name", "band")) # <- NOT WORKING


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


sets <- c(union = animate_union,
          union_all = animate_union_all,
          intersect = animate_intersect,
          setdiff = animate_setdiff)

a <- sapply(1:length(sets), function(i) {
  nam <- names(sets)[i]
  nam <- str_replace(nam, "_", "-")

  width <- 7
  height <- 7

  gif_ <- sets[[i]](x, y, by = "id")
  first_ <- sets[[i]](x, y, by = "id", export = "first")
  last_ <- sets[[i]](x, y, by = "id", export = "last")

  save_animation(gif_, here("images", paste0(nam, ".gif")))
  ggsave(here("images", "static", "png", paste0(nam, "-first.png")), first_,
         height = height, width = width)
  ggsave(here("images", "static", "svg", paste0(nam, "-first.svg")), first_,
         height = height, width = width)
  ggsave(here("images", "static", "png", paste0(nam, "-last.png")), last_,
         height = height, width = width)
  ggsave(here("images", "static", "svg", paste0(nam, "-last.svg")), last_,
         height = height, width = width)
})
