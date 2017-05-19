context('Check manual_import')

ctrl_str <- list(tab_suffix = '',
                 sim_suffix = 'sim',
                 tab_names = c('sdtab', 'mutab', 'patab', 'catab', 'cotab', 'mytab', 'extra', 'xptab', 'cwtab'))

ctrl_data <- xpose_data(file = 'run001.lst', quiet = TRUE)$data$index[[1]]
ctrl_data <- dplyr::arrange_(.data = ctrl_data, .dots = 'tables')

# Tests start here --------------------------------------------------------
test_that('manual_nm_import function works properly', {
  expect_equal(manual_nm_import(), ctrl_str)
})

test_that('list_nm_tables_manual function works properly', {
  test <- xpose_data(file = 'run001.lst', manual_import = manual_nm_import(), quiet = FALSE)$data$index[[1]]
  test <- dplyr::arrange_(.data = test, .dots = 'tables')
  expect_identical(test, ctrl_data)
})
