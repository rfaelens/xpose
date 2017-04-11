context('Check default plot functions')

p1 <- xplot_scatter(xpdb = xpdb_ex_pk, aes_string(x = 'PRED', y = 'DV'),
                    title = '@run-title', subtitle = '@run-subtitle', caption = '@run-caption',
                    point_color = 'red', line_color = 'blue', smooth_color = 'green')

test_that("errors are returned for missing xpdb", {
  expect_error(xplot_scatter())
})

test_that("ggxpose plot objects are returned with appropriate xpdb", {
  # Check class
  expect_equal(class(p1), c('xpose_plot', 'gg', 'ggplot'))
  
  # Check metadata
  expect_equal(p1$xpose$fun, 'xplot_scatter')
  expect_equal(p1$xpose$summary, xpdb_ex_pk$summary)
})

test_that('template titles work properly', {
  expect_equal(p1$labels$title, 'run001-title')
  expect_equal(p1$labels$subtitle, 'run001-subtitle')
  expect_equal(p1$labels$caption, 'run001-caption')
})

test_that('geom_abline layer is present', {
  expect_equal(class(p1$layers[[1]]$geom)[1], 'GeomAbline')
})

test_that('geom_line layer is present', {
  expect_equal(class(p1$layers[[2]]$geom)[1], 'GeomLine')
})

test_that('geom_point layer is present', {
  expect_equal(class(p1$layers[[3]]$geom)[1], 'GeomPoint')
})

test_that('geom_smooth layer is present', {
  expect_equal(class(p1$layers[[4]]$geom)[1], 'GeomSmooth')
})

test_that('xpose_geom mapping works properly', {
  # Check that the color of lines is blue
  expect_equal(p1$layers[[2]]$aes_params$colour, 'blue')
  
  # Check that the color of point is red
  expect_equal(p1$layers[[3]]$aes_params$colour, 'red')
  
  # Check that the color of smooth is green
  expect_equal(p1$layers[[4]]$aes_params$colour, 'green')
})
