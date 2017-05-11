context('Check read_nm_model')

# Define files to be tested -----------------------------------------------
ctrl_mod   <- xpdb_ex_pk$code


# Tests start here --------------------------------------------------------

test_that("error is returned when missing file and runno arguments", {
  expect_error(read_nm_model())
})

test_that("error is returned when file does not exist", {
  expect_error(read_nm_model(file = 'fake_mod.lst'))
})

test_that("properly parses a model given via the file argument", {
  expect_equal(read_nm_model(file = 'run001.lst'), ctrl_mod)
})

test_that("properly parses a model given via the runno and dir arguments", {
  expect_equal(read_nm_model(runno = '001', ext = '.lst'), ctrl_mod)
})
