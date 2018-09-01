context("test-set_text_color")

test_that("correct color selection", {
  colors <- c("#FFFFFF", scales::brewer_pal("seq", "Set1")(4), "#000000")
  expect_equal(choose_text_color(colors), c("#000000", rep("#FFFFFF", 5)))
})
