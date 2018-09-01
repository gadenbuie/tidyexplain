context("test-tidyr_helpers")

test_that("get_quos_names works", {
  expect_equivalent(get_quos_names(-x), "-x")
  expect_equivalent(get_quos_names(x:y), "x:y")
  expect_equivalent(get_quos_names(-x, -y, -z), c("-x", "-y", "-z"))
})

test_that("dput_parsers works", {
  expect_equal(dput_parser("x"), '"x"')
  expect_equal(dput_parser(c("x", "y")), 'c("x", "y")')
})
