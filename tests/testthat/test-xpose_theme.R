context('Check xpose_theme')


# Define custom theme -----------------------------------------------------
theme_xp_custom <- theme_xp_default()
theme_xp_custom$point_color <- 'green'


# Tests start here --------------------------------------------------------

test_that("returns error on missing xpdb", {
  expect_error(xpose_theme())
  })

test_that("returns identical xpdb if missing themes", {
  expect_identical(xpose_theme(xpdb = xpdb_ex_pk), xpdb_ex_pk)
})

test_that("full gg_themes are properly added", {
  expect_identical(xpose_theme(xpdb = xpdb_ex_pk, 
                               gg_theme = theme_bw2())$gg_theme, 
                   theme_bw2())
  })

test_that("message returned on bad gg_theme input", {
  expect_message(xpose_theme(xpdb = xpdb_ex_pk, 
                             gg_theme = theme_xp_default()))
})

test_that("full xp_themes are properly added", {
  expect_identical(xpose_theme(xpdb = xpdb_ex_pk,
                           xp_theme = theme_xp_xpose4())$xp_theme,
                   theme_xp_xpose4())
})

test_that("parial xp_themes are properly added", {
  expect_identical(xpose_theme(xpdb = xpdb_ex_pk, 
                           xp_theme = c(point_color = 'green'))$xp_theme,
               theme_xp_custom)
})

test_that("message on unnamed xp_theme input", {
  expect_message(xpose_theme(xpdb = xpdb_ex_pk, 
                             xp_theme = c('blue', 'dashed')))
})
