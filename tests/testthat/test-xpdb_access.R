context('Check xpdb_access functions')

# Tests for get_code ------------------------------------------------------

test_that("get_code checks input properly", {
  # Error with missing xpdb
  expect_error(get_code(), regexp = '\"xpdb\" is missing')
  
  # Error with bad problem input
  expect_error(get_code(xpdb_ex_pk, problem = 99), regexp = 'Problem no.99 not found in model code')
})

test_that("get_code works properly", {
  # Return full code
  expect_equal(get_code(xpdb_ex_pk), xpdb_ex_pk$code)
  
  # Return single problem
  expect_equal(get_code(xpdb_ex_pk, problem = 1), xpdb_ex_pk$code[xpdb_ex_pk$code$problem == 1, ])
  
  # Return multiple problems
  expect_equal(get_code(xpdb_ex_pk, problem = 0:1), xpdb_ex_pk$code[xpdb_ex_pk$code$problem %in% 0:1, ])
})


# Tests for get_data ------------------------------------------------------
test_that("get_data checks input properly", {
  # Error with missing xpdb
  expect_error(get_data(), regexp = '\"xpdb\" is missing')
  
  # Error with missing table and problems
  expect_error(get_data(xpdb_ex_pk), regexp = '`table` or `problem` required')
  
  # Error with simulataneous table and problems
  expect_error(get_data(xpdb_ex_pk, table = 'sdtab001', problem = 1), regexp = 'together')
  
  # Error with bad problem input
  expect_error(get_data(xpdb_ex_pk, problem = 99), regexp = 'Problem no.99 not found')
  
  # Error with bad table input
  expect_error(get_data(xpdb_ex_pk, table = 'faketab'), regexp = 'faketab not found')
})

test_that("get_data works properly", {
  # Return single problem
  expect_equal(get_data(xpdb_ex_pk, problem = 1), xpdb_ex_pk$data$data[[1]])
  
  # Return single table
  expect_equal(get_data(xpdb_ex_pk, table = 'sdtab001'), 
               xpdb_ex_pk$data$data[[1]][, xpdb_ex_pk$data$index[[1]]$col[xpdb_ex_pk$data$index[[1]]$table == 'sdtab001']])
  # Return multiple tables
  expect_equal(get_data(xpdb_ex_pk, table = c('sdtab001', 'patab001')), 
               list(sdtab001 = xpdb_ex_pk$data$data[[1]][xpdb_ex_pk$data$index[[1]]$col[xpdb_ex_pk$data$index[[1]]$table == 'sdtab001']],
                    patab001 = xpdb_ex_pk$data$data[[1]][xpdb_ex_pk$data$index[[1]]$col[xpdb_ex_pk$data$index[[1]]$table == 'patab001']]))
})


# Tests for get_file ------------------------------------------------------
test_that("get_file checks input properly", {
  # Error with missing xpdb
  expect_error(get_file(), regexp = '\"xpdb\" is missing')
  
  # Error with missing file
  expect_error(get_file(xpdb_ex_pk), regexp = '`file` or `ext` required')
  
  # Error with bad file input
  expect_error(get_file(xpdb_ex_pk, file = 'fakefile'), regexp = 'fakefile not found')
  
  # Error with bad problem input
  expect_error(get_file(xpdb_ex_pk, file = 'run001.ext', problem = 99), regexp = 'Problem no.99 not found')
  
  # Error with bad sub-problem input
  expect_error(get_file(xpdb_ex_pk, file = 'run001.ext', subprob = 99), regexp = 'Sub-problem no.99 not found')
})

test_that("get_file works properly", {
  # Return single file
  expect_equal(get_file(xpdb_ex_pk, file = 'run001.ext', problem = 1, subprob = 0), xpdb_ex_pk$files[xpdb_ex_pk$files$name == 'run001.ext', ]$data[[1]])
  
  # Return multiple files
  expect_equal(get_file(xpdb_ex_pk, file = c('run001.ext', 'run001.phi')), 
                        list(`run001.ext` = xpdb_ex_pk$files[xpdb_ex_pk$files$name == 'run001.ext', ]$data[[1]],
                             `run001.phi` = xpdb_ex_pk$files[xpdb_ex_pk$files$name == 'run001.phi', ]$data[[1]]))
})


# Tests for get_summary ---------------------------------------------------
test_that("get_summary checks input properly", {
  # Error with missing xpdb
  expect_error(get_summary(), regexp = '\"xpdb\" is missing')
  
  # Error with bad problem input
  expect_error(get_summary(xpdb_ex_pk, problem = 99), regexp = 'Problem no.99 not found')
  
  # Error with bad sub-problem input
  expect_error(get_summary(xpdb_ex_pk, subprob = 99), regexp = 'Sub-problem no.99 not found')
})

test_that("get_summary works properly", {
  # Return single problem
  expect_equal(get_summary(xpdb_ex_pk, problem = 1), xpdb_ex_pk$summary[xpdb_ex_pk$summary$problem == 1, ])
  
  # Return multiple problems
  expect_equal(get_summary(xpdb_ex_pk, problem = 0:1), xpdb_ex_pk$summary[xpdb_ex_pk$summary$problem %in% 0:1, ])
})
