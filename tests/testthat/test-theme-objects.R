context("Testing theme objects")

test_that("gg_themes objects were properly created", {
  # Proper object class
  expect_true(is.theme(theme_readable()))
  expect_true(is.theme(theme_bw2()))
})

test_that("xp_themes objects were properly created", {
  # Proper object type
  expect_true(is.list(xp_theme_default()))
  expect_true(is.list(xp_theme_xpose4()))
  
  # Proper object attributes
  expect_true(inherits(xp_theme_default(), 'xpose_theme'))
  expect_true(inherits(xp_theme_xpose4(), 'xpose_theme'))
})
