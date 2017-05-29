context('Check xplot_helpers')

# Tests start here --------------------------------------------------------
test_that('Check check_vars', {
  expect_null(check_vars(NULL, xpdb_ex_pk))
  expect_null(check_vars(c('problem', 'simtab', 'index', 'data'), xpdb_ex_pk))
})

test_that('Check check_scales', {
  expect_equal(check_scales('x', NULL), 'continuous')
  expect_equal(check_scales(c('log10', 'continuous'), NULL), 'continuous')
  expect_equal(check_scales(c('log10', 'continuous'), 'log10'), c('log10','continuous'))
  
})

test_that('Check parse_title', {
  expect_equal(parse_title('OFV: @ofv', xpdb_ex_pk, problem = 1, quiet = TRUE), 'OFV: -656.869')
  expect_message(parse_title('OFV: @fake', xpdb_ex_pk, problem = 1, quiet = FALSE), regexp = 'not part of')
  expect_equal(parse_title('OFV: @fake', xpdb_ex_pk, problem = 1, quiet = TRUE), 'OFV: @fake')
  expect_equal(parse_title('OFV: @fake', xpdb_ex_pk, problem = 1, quiet = TRUE, extra_key = 'fake', extra_value = '1987'), 'OFV: 1987')
})
