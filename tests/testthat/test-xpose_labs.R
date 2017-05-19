context('Check xpose_labs')

# Define plots to be tested -----------------------------------------------
p1 <- xplot_scatter(xpdb = xpdb_ex_pk, aes_string(x = 'PRED', y = 'DV'),
                    title = '@run-title', subtitle = '@run-subtitle', caption = '@run-caption',
                    point_color = 'red', line_color = 'blue', smooth_color = 'green')

# Tests start here --------------------------------------------------------
test_that('Check label modification', {
  expect_is(xpose_labs(title    = 'Hello world this is a plot for @run',
                       subtitle = NULL,
                       caption  = 'This run was taken from: @dir', 
                       x        = 'Individual Predictions (IPRED)'), 'xpose_labels')
  
  expect_is(xpose_labs(title    = NULL,
                       subtitle = NULL,
                       caption  = NULL, 
                       x        = NULL), 'xpose_labels')
})

test_that('xpose_labs can be added to xpose_plots', {
  p2 <- p1 + xpose_labs(title = 'This is @run', subtitle = 'Run featuring @nind ind', caption = 'saved in @dir')
  expect_equal(p2$labels$title, 'This is run001')
  expect_equal(p2$labels$subtitle, 'Run featuring 74 ind')
  expect_equal(p2$labels$caption, 'saved in analysis/models/pk/')
})
