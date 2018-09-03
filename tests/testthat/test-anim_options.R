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

  anim_options_set()
  expect_equal(get_anim_opt("transition_length"), plot_settings$default$transition_length)

  anim_options_set(anim_options(enter = enter_appear(early = TRUE)))
  expect_equal(names(get_anim_opt("enter")), "enter_appear(early = TRUE)")
  expect_s3_class(get_anim_opt("enter")[[1]], "ggproto")

  anim_options_set()
})

test_that("precedence: function > user-set global > default (> global default)", {
  ao_function <- anim_options(ease_default = "linear")
  ao_global <- anim_options(ease_default = "cubic", text_family = "Arial")
  expect_equal(default_anim_opts("gather", ao_function)$ease_default, "linear")

  anim_options_set(ao_global)
  expect_equal(default_anim_opts("gather")$ease_default, "cubic")
  expect_equal(default_anim_opts("gather", ao_function)$ease_default, "linear")

  ao_default <- default_anim_opts("gather", ao_function) # inside animate_ function
  ao_final <- validate_anim_opts(ao_default)             # just before animate_plot() or static_plot()
  expect_equal(ao_final$ease_default, "linear")
  expect_equal(ao_final$text_family, "Arial")
  expect_equivalent(names(ao_final$ease_other), c("y", "x"))
  expect_equal(ao_final$title_family, plot_settings$default$title_family)

  anim_options_set()
})
