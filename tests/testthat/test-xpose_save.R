context('xpose save')
# Inspired from ggplot2 ggsave tests

plot <- dv_vs_ipred(xpdb = xpdb_ex_pk)

test_that("errors are returned for bad plot input", {
  expect_error(xpose_save(plot = NULL))
})


test_that("errors are returned for bad filename input", {
  paths_1 <- paste0(tempdir(), '/test_plot.', c('abcd', 'bcde'))
  on.exit(unlink(paths_1))
  
  # Unrecognized extension
  expect_error(xpose_save(plot = plot, filename = paths_1[1]))
  
  # Missing filename
  expect_error(xpose_save(plot = plot, filename = NULL))
  
  # Length filename > 1
  expect_error(xpose_save(plot = plot, filename = paths_1))
})


test_that("all graphical devices work properly", {
  paths_2 <- paste0(tempdir(), '/test_plot.', 
                    c('pdf', 'jpeg', 'png', 'bmp', 'tiff'))
  on.exit(unlink(paths_2))
  
  expect_false(any(file.exists(paths_2)))
  xpose_save(plot = plot, filename = paths_2[1]) # pdf
  xpose_save(plot = plot, filename = paths_2[2]) # jpeg
  xpose_save(plot = plot, filename = paths_2[3]) # png
  xpose_save(plot = plot, filename = paths_2[4]) # bmp
  xpose_save(plot = plot, filename = paths_2[5]) # tiff
  expect_true(all(file.exists(paths_2)))
})


test_that("template filenames and auto extension work properly", {
  paths_3 <- paste0(tempdir(), '/run001_dv_vs_ipred.pdf')
  on.exit(unlink(paths_3))
  
  expect_false(file.exists(paths_3))
  
  # Pdf extension should automatically be added and template keywords
  # replaced by their value
  xpose_save(plot = plot, 
             filename = paste0(tempdir(), '/@run_@plotfun'))
  expect_true(file.exists(paths_3))
})
