context('Check xpose_data')

# Tests start here --------------------------------------------------------

test_that("error is returned when missing file and runno arguments", {
  expect_error(xpose_data())
})

test_that("error is returned when file does not exist", {
  expect_error(xpose_data(file = 'fake_mod.lst'))
})

test_that("error is returned for bad ext input", {
  expect_error(xpose_data(runno = '001', ext = 'pdf'))
})

test_that("properly creates the xpdb when using the file argument", {
  xpdb_1 <- xpose_data(file = 'run001.lst', quiet = TRUE)
  expect_true(inherits(xpdb_1, 'xpose_data'))
  
  xpdb_1$summary$dir <- "analysis/models/pk/" # Path has to be corrected for comparison
  xpdb_1$options$dir <- "analysis/models/pk/" # Path has to be corrected for comparison
  expect_identical(xpdb_1, xpdb_ex_pk)
})

test_that("properly creates the xpdb when using the runno argument", {
  xpdb_2 <- xpose_data(runno = '001', ext = '.lst', quiet = TRUE)
  expect_true(inherits(xpdb_2, 'xpose_data'))
  
  xpdb_2$summary$dir <- "analysis/models/pk/" # Path has to be corrected for comparison
  xpdb_2$options$dir <- "analysis/models/pk/" # Path has to be corrected for comparison
  expect_identical(xpdb_2, xpdb_ex_pk)
})
