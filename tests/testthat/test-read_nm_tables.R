context('Check read_nm_tables')

# Define files to be tested -----------------------------------------------

test_tab <- read_nm_tables(files = 'sdtab001', verbose = FALSE)
ctrl_tab <- xpdb_ex_pk$data[, xpdb_ex_pk$tab_index$sdtab001]

test_file <- c("TABLE NO.  4",
               " ID          KA          CL          V           ALAG1       ETA1        ETA2        ETA3        DV          PRED        RES         WRES",
               "  1.1000E+02  4.1052E-01  2.5483E+01  1.4917E+02  2.3223E-01 -4.5845E-02 -3.5313E-03 -2.1460E+00  0.0000E+00 -3.6889E+00  0.0000E+00  0.0000E+00",
               "  1.1000E+02  4.1052E-01  2.5483E+01  1.4917E+02  2.3223E-01 -4.5845E-02 -3.5313E-03 -2.1460E+00 -2.4841E+00 -5.6877E-01 -1.9153E+00 -3.8853E+00")
ctrl_file <- xpdb_ex_pk$data[1:2, xpdb_ex_pk$tab_index$patab001]


# Tests start here --------------------------------------------------------

test_that("message is returned when missing file argument", {
  expect_null(read_nm_tables(verbose = FALSE))
  expect_message(read_nm_tables(verbose = TRUE))
})

test_that("message is returned when all provided files are missing", {
  expect_null(read_nm_tables(files = 'fake_table.tab', verbose = FALSE))
  expect_message(read_nm_tables(files = 'fake_table.tab', verbose = TRUE))
})

test_that("returns a proper table when valid arguments are provided", {
  expect_equal(test_tab, ctrl_tab)
})

test_that("auto mode properly assign skip and header arguments", {
  files <- tempfile(c('tmp_a', 'tmp_b'))
  on.exit(unlink(files))
  
  # Test with skip = 1 and header = TRUE
  writeLines(text = test_file[1:4], con = files[1])
  expect_equal(read_nm_tables(files = files[1]),  ctrl_file)
  
  # Test with skip = 0 and header = TRUE
  writeLines(text = test_file[2:4], con = files[2])
  expect_equal(read_nm_tables(files = files[2]),  ctrl_file)
})
