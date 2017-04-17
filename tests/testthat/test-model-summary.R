context('Check model summary functions')

# Note: Add tests where the information is not available

# Tests start here --------------------------------------------------------

test_that("descr can be found in model code", {
  expect_equal(descr(xpdb_ex_pk$code), "ggxpose test run")
})

test_that("shrinkage can be found in model code", {
  # Etas
  expect_equal(shrinkage(xpdb_ex_pk$code, type = 'ETA', rounding = 2), 
               "ETA shrink: 10.16 % [1], 48.17 % [2], 14.7 % [3]")
  # Epsilons
  expect_equal(shrinkage(xpdb_ex_pk$code, type = 'EPS', rounding = 2), 
               "EPS shrink: 6.75 % [1]")
})

test_that("ofv can be obtained in model code", {
  expect_equal(ofv(xpdb_ex_pk$code), "-656.869")
})

test_that("raw_dat can be obtained in model code", {
  expect_equal(raw_dat(xpdb_ex_pk$code), "mx19_1.csv")
})

test_that("nobs can be obtained in model code", {
  expect_equal(nobs(xpdb_ex_pk$code), "1022")
})

test_that("nind can be obtained in model code", {
  expect_equal(nind(xpdb_ex_pk$code), "74")
})

test_that("method can be obtained in model code", {
  expect_equal(method(xpdb_ex_pk$code), "FOCE-I")
})
