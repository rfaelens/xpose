# Inspired from ggplot2 ggsave tests
context('Check xpose_save')

# Define plots to be tested -----------------------------------------------

plot <- dv_vs_ipred(xpdb = xpdb_ex_pk)


# Tests start here --------------------------------------------------------

test_that("errors are returned for bad plot input", {
  expect_error(xpose_save(plot = NULL))
})

test_that("errors are returned for bad filename input", {
  paths_1 <- file.path(tempdir(), 
                       paste0('test_plot', c('.abcd', '.bcde', '')))
  on.exit(unlink(paths_1))
  
  # Unrecognized extension
  expect_error(xpose_save(plot = plot, filename = paths_1[1]))
  
  # Missing extension
  expect_error(xpose_save(plot = plot, filename = paths_1[3]))
  
  # Missing filename
  expect_error(xpose_save(plot = plot, filename = NULL))
  
  # Length filename > 1
  expect_error(xpose_save(plot = plot, filename = paths_1))
})


test_that("common graphical device work properly", {
  paths_2 <- file.path(tempdir(), 
                       paste0('test_plot.', c('pdf', 'jpeg', 'png')))
  on.exit(unlink(paths_2))
  
  expect_false(any(file.exists(paths_2)))
  
  # Test pdf
  xpose_save(plot = plot, filename = paths_2[1])
  expect_true(file.exists(paths_2[1]))
  
  # Test jpeg
  if (capabilities('jpeg')) {
    xpose_save(plot = plot, filename = paths_2[2], dpi = 20) # jpeg
    expect_true(file.exists(paths_2[2]))
  }
  
  # Test png
  if (capabilities('png')) {
    xpose_save(plot = plot, filename = paths_2[3], dpi = 20) # png
    expect_true(file.exists(paths_2[3]))
  }
})

test_that("template filenames and auto file extension work properly", {
  paths_3 <- file.path(tempdir(), 'run001_dv_vs_ipred.pdf') 
  on.exit(unlink(paths_3))
  
  expect_false(file.exists(paths_3))
  
  xpose_save(plot = plot, 
             filename = file.path(tempdir(), '@run_@plotfun'), 
             device = 'pdf')
  expect_true(file.exists(paths_3))
})
