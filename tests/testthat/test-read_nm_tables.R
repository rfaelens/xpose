context('Check read_nm_tables')

# Define files to be tested -----------------------------------------------

test_tab <- read_nm_tables(files = 'sdtab001', verbose = FALSE)
ctrl_tab <- xpdb_ex_pk$data[, xpdb_ex_pk$tab_index[xpdb_ex_pk$tab_index$tables == 'sdtab001',]$colnames[[1]]]

test_file <- c("TABLE NO.  4",
               " ID          KA          CL          V           ALAG1       ETA1        ETA2        ETA3        DV          PRED        RES         WRES",
               "  1.1000E+02  4.1052E-01  2.5483E+01  1.4917E+02  2.3223E-01 -4.5845E-02 -3.5313E-03 -2.1460E+00  0.0000E+00 -3.6889E+00  0.0000E+00  0.0000E+00",
               "  1.1000E+02  4.1052E-01  2.5483E+01  1.4917E+02  2.3223E-01 -4.5845E-02 -3.5313E-03 -2.1460E+00 -2.4841E+00 -5.6877E-01 -1.9153E+00 -3.8853E+00")
ctrl_file <- xpdb_ex_pk$data[1:2, xpdb_ex_pk$tab_index[xpdb_ex_pk$tab_index$tables == 'patab001',]$colnames[[1]]]

firstonly_test <- as.nm.table.list(dplyr::tibble(problem   = 1, 
                                                 file      = c('sdtab001', 'patab001'),
                                                 firstonly = c(TRUE, FALSE),
                                                 simtab    = FALSE))

# Tests start here --------------------------------------------------------

test_that("message is returned when missing file argument", {
  expect_null(read_nm_tables(verbose = FALSE))
  expect_message(read_nm_tables(verbose = TRUE))
})

test_that("message is returned when all provided files are missing", {
  expect_null(read_nm_tables(files = 'fake_table.tab', verbose = FALSE))
  expect_message(read_nm_tables(files = 'fake_table.tab', verbose = TRUE), regexp = 'could be found')
})

test_that("message is returned when tables exist but are duplicated", {
  expect_null(read_nm_tables(files = c('sdtab001', 'patab001', 'sdtab001'), verbose = FALSE))
  expect_message(read_nm_tables(files = c('sdtab001', 'patab001', 'sdtab001'), verbose = TRUE), regexp = 'duplicated')
})

test_that("message is returned when missing table header", {
  files <- tempfile('tmp_header')
  on.exit(unlink(files))
  
  # Test with skip = 1 and header = FALSE
  writeLines(text = test_file[c(1, 3:4)], con = files[1])
  expect_message(read_nm_tables(files = files[1], verbose = TRUE), regexp = 'Dropped.+absence of headers')
})

test_that("returns a tibble when user mode is used", {
  expect_equal(test_tab, ctrl_tab)
})

test_that("tables with firstonly are properly handled", {
  expect_message(read_nm_tables(firstonly_test, verbose = TRUE), regexp = 'Consolidating|Joining')
  expect_equal(read_nm_tables(firstonly_test, verbose = FALSE)$data[[1]], 
               xpdb_ex_pk$data[, unique(unlist(xpdb_ex_pk$tab_index[xpdb_ex_pk$tab_index$tables %in% c('sdtab001', 'patab001'), ]$colnames))])
})

test_that("properly assign skip and header arguments", {
  files <- tempfile(c('tmp_a', 'tmp_b'))
  on.exit(unlink(files))
  
  # Test with skip = 1 and header = TRUE
  writeLines(text = test_file[1:4], con = files[1])
  expect_equal(read_nm_tables(files = files[1]),  ctrl_file)
  
  # Test with skip = 0 and header = TRUE
  writeLines(text = test_file[2:4], con = files[2])
  expect_equal(read_nm_tables(files = files[2]),  ctrl_file)
})
