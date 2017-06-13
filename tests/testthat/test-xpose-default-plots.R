context('Check default plot functions')


# Define plots to be tested -----------------------------------------------

# General tests
p1 <- xplot_scatter(xpdb = xpdb_ex_pk, aes_string(x = 'PRED', y = 'DV'), guides = TRUE, type = 'plst',
                    title = '@run-title', subtitle = '@run-subtitle', caption = '@run-caption',
                    point_color = 'red', line_color = 'blue', 
                    smooth_color = 'green', text_color = 'yellow', quiet = TRUE)

# Faceting test
p2 <- xplot_scatter(xpdb = xpdb_ex_pk, aes_string(x = 'PRED', y = 'DV'), 
                    panel_facets = 'SEX', quiet = TRUE)

p3 <- xplot_scatter(xpdb = xpdb_ex_pk, aes_string(x = 'PRED', y = 'DV'), 
                    panel_facets = OCC~SEX, quiet = TRUE)


# Tests start here --------------------------------------------------------

test_that("errors are returned for missing or bad xpdb", {
  expect_error(xplot_scatter())
  expect_null(xplot_scatter(xpdb = 1))
})

test_that("xpose plot objects are returned with appropriate xpdb", {
  # Check class
  expect_true(is.xpose.plot(p1))
  
  # Check metadata
  expect_equal(p1$xpose$fun, 'scatter_plot')
  expect_equal(p1$xpose$summary, xpdb_ex_pk$summary)
  expect_equal(p1$xpose$problem, 1)
  expect_true(p1$xpose$quiet)
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

test_that('geom_text layer is present', {
  expect_equal(class(p1$layers[[4]]$geom)[1], 'GeomText')
})

test_that('geom_smooth layer is present', {
  expect_equal(class(p1$layers[[5]]$geom)[1], 'GeomSmooth')
})


test_that('faceting works properly', {
  expect_true(is.null(p1$facet$params$facets))
  expect_true(is.list(p2$facet$params$facets))
  expect_equal(as.character(p3$facet$params$cols[[1]]), 'SEX')
  expect_equal(as.character(p3$facet$params$rows[[1]]), 'OCC')
})

test_that('xpose_geom mapping works properly', {
  # Check that the color of lines is blue
  expect_equal(p1$layers[[2]]$aes_params$colour, 'blue')
  
  # Check that the color of point is red
  expect_equal(p1$layers[[3]]$aes_params$colour, 'red')

  # Check that the color of smooth is yellow
  expect_equal(p1$layers[[4]]$aes_params$colour, 'yellow')
    
  # Check that the color of smooth is green
  expect_equal(p1$layers[[5]]$aes_params$colour, 'green')
})
