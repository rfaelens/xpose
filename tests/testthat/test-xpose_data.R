context('Check xpose_data')

# Tests start here --------------------------------------------------------

test_that('error is returned when missing file and runno arguments', {
  expect_error(xpose_data())
})

test_that('error is returned when file does not exist', {
  expect_error(xpose_data(file = 'fake_mod.lst', dir = 'data'))
})

test_that('error is returned for bad ext input', {
  expect_error(xpose_data(runno = '001', ext = 'pdf', dir = 'data'))
})

test_that('properly creates the xpdb when using the file argument', {
  xpdb_1 <- xpose_data(file = 'run001.lst', dir = 'data', quiet = TRUE) %>% 
    set_var_types(idv = 'TAD')
  expect_true(inherits(xpdb_1, 'xpose_data'))
  
  # Correct path and quiet option for identical comparison with xpdb_ex_pk
  xpdb_1$summary$value[xpdb_1$summary$label == 'dir'] <- 'analysis/models/pk/' 
  xpdb_1$options$dir <- 'analysis/models/pk/'
  attr(xpdb_1$code, 'dir') <- 'analysis/models/pk/'
  xpdb_1$options$quiet <- FALSE
  expect_identical(xpdb_1, xpdb_ex_pk)
})

test_that('properly creates the xpdb when using the runno argument', {
  xpdb_2 <- xpose_data(runno = '001', ext = '.lst', dir = 'data', quiet = TRUE) %>% 
    set_var_types(idv = 'TAD')
  expect_true(inherits(xpdb_2, 'xpose_data'))
  
  # Correct path and quiet option for identical comparison with xpdb_ex_pk
  xpdb_2$summary$value[xpdb_2$summary$label == 'dir'] <- 'analysis/models/pk/'
  xpdb_2$options$dir <- 'analysis/models/pk/'
  attr(xpdb_2$code, 'dir') <- 'analysis/models/pk/'
  xpdb_2$options$quiet <- FALSE
  expect_identical(xpdb_2, xpdb_ex_pk)
})

test_that('properly handles error in summary', {
  expect_message(xpdb_3 <- xpose_data(runno = '001', ext = '.lst', dir = 'data', 
                                      ignore = c('data', 'files'),
                                      quiet = TRUE, xp_theme = c(rounding = 'No')),
                 regexp = 'Error returned by run summary')
  expect_error(summary(xpdb_3), regex = 'No `summary` slot could be found in this xpdb')
})  
