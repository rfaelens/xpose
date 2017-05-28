context('Check model summary functions')

# Define files to be tested -----------------------------------------------
software <- 'nonmem'
model    <- xpdb_ex_pk$code
model2   <- model[0, ]
file     <- 'pk/model/run001.lst'
rounding <- 2

sum_out <- function(sum_fun) {
  as.character(sum_fun[1, c('label', 'value')])
}

# Tests start here --------------------------------------------------------

test_that("summary is properly created with the appropriate information", {
  expect_equal(sum_out(sum_software(software)), c('software', 'nonmem'))
  expect_equal(sum_out(sum_version(model, software)), c('version', '7.3.0'))
  expect_equal(sum_out(sum_file(file)), c('file', 'run001.lst'))
  expect_equal(sum_out(sum_run(file)), c('run', 'run001'))
  expect_equal(sum_out(sum_directory(file)), c('dir', 'pk/model'))
#  expect_equal(sum_out(sum_reference(model, software)), c('ref', 'na'))
  expect_equal(sum_out(sum_probn(model, software)), c('probn', '1'))
  expect_equal(sum_out(sum_description(model, software)), c('descr', 'xpose test run'))
  expect_equal(sum_out(sum_input_data(model, software)), c('data', 'mx19_1.csv'))
  expect_equal(sum_out(sum_nobs(model, software)), c('nobs', '1022'))
  expect_equal(sum_out(sum_nind(model, software)), c('nind', '74'))
  #expect_equal(sum_out(sum_nsim(model, software)), c('nsim', 'na'))
  #expect_equal(sum_out(sum_simseed(model, software)), c('simseed', 'na'))
  expect_equal(sum_out(sum_subroutine(model, software)), c('subroutine', '2'))
  expect_equal(sum_out(sum_runtime(model, software)), c('runtime', '00:00:04'))
  expect_equal(sum_out(sum_covtime(model, software)), c('covtime', '00:00:04'))
  #expect_equal(sum_out(sum_warnings(model, software)), c('warnings', 'na'))
  #expect_equal(sum_out(sum_errors(model, software)), c('errors', 'na'))
  expect_equal(sum_out(sum_nsig(model, software)), c('nsig', '3.6'))
  expect_equal(sum_out(sum_condn(model, software, rounding)), c('condn', '31.33'))
  #expect_equal(sum_out(sum_nesample(model, software)), c('nesample', 'na'))
  #expect_equal(sum_out(sum_esampleseed(model, software)), c('esampleseed', 'na'))
  expect_equal(sum_out(sum_ofv(model, software)), c('ofv', '-656.869'))
  expect_equal(sum_out(sum_method(model, software)), c('method', 'foce-i'))
  expect_equal(sum_out(sum_shk(model, software, 'eps', rounding)), c('epsshk', '6.75 [1]'))
  expect_equal(sum_out(sum_shk(model, software, 'eta', rounding)), c('etashk', '10.16 [1], 48.17 [2], 14.7 [3]'))
})

test_that("summary default summary is returned for missing information", {
  expect_equal(sum_out(sum_version(model2, software)), c('version', 'na'))
  expect_equal(sum_out(sum_reference(model2, software)), c('ref', 'na'))
  expect_equal(sum_out(sum_probn(model2, software)), c('probn', 'na'))
  expect_equal(sum_out(sum_description(model2, software)), c('descr', 'na'))
  expect_equal(sum_out(sum_input_data(model2, software)), c('data', 'na'))
  expect_equal(sum_out(sum_nobs(model2, software)), c('nobs', 'na'))
  expect_equal(sum_out(sum_nind(model2, software)), c('nind', 'na'))
  expect_equal(sum_out(sum_nsim(model2, software)), c('nsim', 'na'))
  expect_equal(sum_out(sum_simseed(model2, software)), c('simseed', 'na'))
  expect_equal(sum_out(sum_subroutine(model2, software)), c('subroutine', 'na'))
  expect_equal(sum_out(sum_runtime(model2, software)), c('runtime', 'na'))
  expect_equal(sum_out(sum_covtime(model2, software)), c('covtime', 'na'))
  expect_equal(sum_out(sum_warnings(model2, software)), c('warnings', 'na'))
  expect_equal(sum_out(sum_errors(model2, software)), c('errors', 'na'))
  expect_equal(sum_out(sum_nsig(model2, software)), c('nsig', 'na'))
  expect_equal(sum_out(sum_condn(model2, software, rounding)), c('condn', 'na'))
  expect_equal(sum_out(sum_nesample(model2, software)), c('nesample', 'na'))
  expect_equal(sum_out(sum_esampleseed(model2, software)), c('esampleseed', 'na'))
  expect_equal(sum_out(sum_ofv(model2, software)), c('ofv', 'na'))
  expect_equal(sum_out(sum_method(model2, software)), c('method', 'na'))
  expect_equal(sum_out(sum_shk(model2, software, 'eps', rounding)), c('epsshk', 'na'))
  expect_equal(sum_out(sum_shk(model2, software, 'eta', rounding)), c('etashk', 'na'))
})
