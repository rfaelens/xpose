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


test_that("pdf graphical device works properly", {
  paths_2 <- file.path(tempdir(), paste0('test_plot.pdf'))
  on.exit(unlink(paths_2))

  xpose_save(plot = plot, filename = paths_2)               # pdf
  expect_true(file.exists(paths_2))
})

test_that("jpeg graphical device works properly", {
  paths_3 <- file.path(tempdir(), paste0('test_plot.jpeg'))
  on.exit(unlink(paths_3))
  
  xpose_save(plot = plot, filename = paths_3, res = 20)    # jpeg
  
  expect_true(file.exists(paths_3))
})

test_that("png graphical device works properly", {
  paths_4 <- file.path(tempdir(), paste0('test_plot.png'))
  on.exit(unlink(paths_4))
  
  xpose_save(plot = plot, filename = paths_4, res = 20)    # png
  
  expect_true(file.exists(paths_4))
})

test_that("bmp graphical device works properly", {
  paths_5 <- file.path(tempdir(), paste0('test_plot.bmp'))
  on.exit(unlink(paths_5))
  
  xpose_save(plot = plot, filename = paths_5, res = 20)    # bmp
  
  expect_true(file.exists(paths_5))
})

test_that("tiff graphical device works properly", {
  paths_6 <- file.path(tempdir(), paste0('test_plot.tiff'))
  on.exit(unlink(paths_6))
  
  xpose_save(plot = plot, filename = paths_6, res = 20)    # tiff
  
  expect_true(file.exists(paths_6))
})

  #expect_false(any(file.exists(paths_2)))
  #xpose_save(plot = plot, filename = paths_2[2], res = 20) # jpeg
  #xpose_save(plot = plot, filename = paths_2[3], res = 20) # png
  #xpose_save(plot = plot, filename = paths_2[4], res = 20) # bmp
  #xpose_save(plot = plot, filename = paths_2[5], res = 20) # tiff
  #expect_true(all(file.exists(paths_2)))
