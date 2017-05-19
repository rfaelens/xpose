# Inspired from ggplot2 ggsave tests
context('Check xpose_labs')

# Define plots to be tested -----------------------------------------------

p1 <- xplot_scatter(xpdb = xpdb_ex_pk, aes_string(x = 'PRED', y = 'DV'),
                    title = '@run-title', subtitle = '@run-subtitle', caption = '@run-caption',
                    point_color = 'red', line_color = 'blue', smooth_color = 'green')

# Faceting test
p2 <- xplot_scatter(xpdb = xpdb_ex_pk, aes_string(x = 'PRED', y = 'DV'), 
                    panel_facets = 'SEX')

# Tests start here --------------------------------------------------------


test_that("Check label modification", {
  expect_is(xpose_labs(title    = 'Hello world this is a plot for @run',
                       subtitle = NULL,
                       caption  = 'This run was taken from: @dir', 
                       x        = 'Individual Predictions (IPRED)'), "xpose_labels")
  
  expect_is(xpose_labs(title    = NULL,
                       subtitle = NULL,
                       caption  = NULL, 
                       x        = NULL), "xpose_labels")
})



