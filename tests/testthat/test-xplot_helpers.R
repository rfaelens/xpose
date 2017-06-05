context('Check xplot_helpers')

# Tests start here --------------------------------------------------------
# test_that('Check check_vars', {
#   expect_null(check_vars(NULL, xpdb_ex_pk))
#   expect_null(check_vars(c('problem', 'simtab', 'index', 'data'), xpdb_ex_pk))
# })

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

test_that('Check filter_xp_theme', {
  # Keep matches
  expect_equal(filter_xp_theme(xpdb_ex_pk$xp_theme, 'point_', action = 'keep'),
               xpdb_ex_pk$xp_theme[grepl('point_', names(xpdb_ex_pk$xp_theme))])
  # Drop matches
  expect_equal(filter_xp_theme(xpdb_ex_pk$xp_theme, 'point_', action = 'drop'),
               xpdb_ex_pk$xp_theme[!grepl('point_', names(xpdb_ex_pk$xp_theme))])
})

test_that('Check last_data_problem', {
  expect_equal(last_data_problem(xpdb_ex_pk, simtab = FALSE), 1)
  expect_equal(last_data_problem(xpdb_ex_pk, simtab = TRUE), NA_integer_)
})

test_that('Check last_file_problem', {
  expect_equal(last_file_problem(xpdb_ex_pk, file = 'run001.ext'), 1)
  expect_equal(last_file_problem(xpdb_ex_pk, file = 'fake.file'), NA_integer_)
})

test_that('Check last_file_subprob', {
  expect_equal(last_file_subprob(xpdb_ex_pk, file = 'run001.ext', problem = 1), 0)
  expect_equal(last_file_subprob(xpdb_ex_pk, file = 'fake.file', problem = 1), NA_integer_)
})

test_that('Check xp_var', {
  expect_equal(xp_var(xpdb_ex_pk, 1, col = 'TIME')$type, 'idv')
  expect_equal(xp_var(xpdb_ex_pk, 1, type = 'idv')$col, 'TIME')
  expect_null(xp_var(xpdb_ex_pk, 1, col = 'FAKE_COL'))
})

test_that('Check append_aes', {
  expect_equal(aes_c(aes_string(x = 'IPRED', y = 'DV'), NULL), 
               aes_string(x = 'IPRED', y = 'DV'))
  expect_equal(aes_c(aes_string(x = 'IPRED', y = 'DV'), 
                     aes_string(y = 'PRED')), 
               aes_string(x = 'IPRED', y = 'PRED'))
})
