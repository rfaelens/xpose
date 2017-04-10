context('xpose save')
# Inspired from ggplot2 ggsave tests

plot <- dv_vs_ipred(xpdb = xpdb_ex_pk)

test_that("errors are returned for bad plot input", {
  expect_error(xpose_save(plot = NULL))
})


test_that("errors are returned for bad filename input", {
  paths_1 <- file.path(tempdir(), 
                       paste0('test_plot.', c('abcd', 'bcde')))
  on.exit(unlink(paths_1))
  
  # Unrecognized extension
  expect_error(xpose_save(plot = plot, filename = paths_1[1]))
  
  # Missing filename
  expect_error(xpose_save(plot = plot, filename = NULL))
  
  # Length filename > 1
  expect_error(xpose_save(plot = plot, filename = paths_1))
})


test_that("all graphical devices work properly", {
  paths_2 <- file.path(tempdir(), 
                       paste0('test_plot.', c('pdf', 'jpeg', 'png', 'bmp', 'tiff')))
  on.exit(unlink(paths_2))
  
  expect_false(any(file.exists(paths_2)))
  xpose_save(plot = plot, filename = paths_2[1])           # pdf
  xpose_save(plot = plot, filename = paths_2[2], res = 20) # jpeg
  xpose_save(plot = plot, filename = paths_2[3], res = 20) # png
  xpose_save(plot = plot, filename = paths_2[4], res = 20) # bmp
  xpose_save(plot = plot, filename = paths_2[5], res = 20) # tiff
  expect_true(all(file.exists(paths_2)))
})
