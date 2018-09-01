context("test-set_text_color")

test_that("correct color selection", {
  colors <- c("#FFFFFF", "#9E788C", "#B679E5", "#4BB757",
              "#000000", "#0027D8", "#E6071B", "#495B3F")
  expect_equal(set_text_color(colors), c(rep("#000000", 4), rep("#FFFFFF", 4)))
})
