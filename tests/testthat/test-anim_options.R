context("test-anim_options")

test_that("merging of animation options works", {
  ao_new <- anim_options(5, 3, text_size = 9, title_size = 13)
  ao_old <- anim_options(ease_default = "cubic-in", text_family = "Times New Roman")
  ao_merged <- anim_options(5, 3, "cubic-in", text_size = 9, title_size = 13, text_family = "Times New Roman")
  expect_equal(merge(ao_new, ao_old), ao_merged)
})
