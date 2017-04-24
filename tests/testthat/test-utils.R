context('Test utils functions')

# Tests start here --------------------------------------------------------

test_that("is.xp.theme works as expected", {
  expect_false(is.xp.theme(theme_bw2()))
  expect_true(is.xp.theme(theme_xp_xpose4()))
})

test_that("is.xpdb works as expected", {
  expect_false(is.xpdb(xpdb_ex_pk$data))
  expect_true(is.xpdb(xpdb_ex_pk))
})

test_that("is.model.file works as expected", {
  expect_false(is.model.file(xpdb_ex_pk$data))
  expect_true(is.model.file(xpdb_ex_pk$code))
})

test_that("is.formula works as expected", {
  expect_false(is.formula('x~y'))
  expect_true(is.formula(x~y))
})

test_that("msg works as expected", {
  expect_null(msg('Hello', verbose = FALSE))
  expect_message(msg('Hello', verbose = TRUE))
})

test_that("file_path works as expected", {
  expect_equal(file_path(dir = NULL, file = 'file.csv'), 'file.csv')
  expect_equal(file_path(dir = 'dir', file = 'file.csv'), paste('dir', 'file.csv', sep = .Platform$file.sep))
  
  # Test clean trailing forward slashes
  expect_equal(file_path(dir = 'inst/extdata/catab001//', file = 'test.csv'), "inst/extdata/catab001/test.csv")
  expect_equal(file_path(dir = 'inst/extdata/catab001', file = 'test.csv'), "inst/extdata/catab001/test.csv")
})
