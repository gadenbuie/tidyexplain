context("test-anim_options")

test_that("merging of animation options works", {
  ao_new <- anim_options(5, 3, text_size = 9, title_size = 13)
  ao_old <- anim_options(ease_default = "cubic-in", text_family = "Times New Roman")
  ao_merged <- anim_options(5, 3, "cubic-in", text_size = 9, title_size = 13, text_family = "Times New Roman")
  expect_equal(merge(ao_new, ao_old), ao_merged)
})

test_that("setting and getting animation options works", {
  set_font_size(5, 10)
  expect_equal(get_anim_opt(), anim_options(text_size = 5, title_size = 10))
  expect_error(get_anim_opt("text_size"))
  expect_equal(get_text_size(), get_anim_opt()$text_size)
  expect_equal(get_title_size(), get_anim_opt()$title_size)

  anim_options_set(anim_options(2, 1))
  expect_equal(get_anim_opt("transition_length"), 2)
  expect_equal(get_anim_opt("state_length"), 1)
  expect_equal(get_anim_opt(), anim_options(2, 1, text_size = 5, title_size = 10))

  anim_options_set(anim_options())
  expect_equal(get_anim_opt("transition_length"), plot_settings$default$transition_length)

  anim_options_set(anim_options(enter = enter_appear(early = TRUE)))
  expect_equal(names(get_anim_opt("enter")), "enter_appear(early = TRUE)")
  expect_s3_class(get_anim_opt("enter")[[1]], "ggproto")
})
