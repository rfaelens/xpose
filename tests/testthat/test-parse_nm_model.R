context('Check parse_nm_model')

# Define files to be tested -----------------------------------------------

ctrl_mod <- data.frame(LEVEL = c(19, 20, 20, 21),
                       SUB   = c('$COVARIANCE', rep('$TABLE', 3)),
                       ABREV = c('COV', rep('TAB', 3)),
                       CODE  = c('PRINT=E', 
                                 'ID OCC DOSE AMT SS II TIME TAD IPRED CWRES CPRED IWRES',
                                 'EVID ONEHEADER NOPRINT FILE=fake_sdtab.tab',
                                 'ID KA CL V ALAG1 ETAS(1:LAST) ONEHEADER NOPRINT FILE=fake_patab.tab'),
                       COMMENT = NA)

ctrl_mod   <- xpdb_ex_pk$code


# Tests start here --------------------------------------------------------

test_that("error is returned when missing file and runno arguments", {
  expect_error(parse_nm_model())
})

test_that("error is returned when file does not exist", {
  expect_error(parse_nm_model(file = 'fake_mod.lst'))
})

test_that("properly parses a model given via the file argument", {
  expect_equal(parse_nm_model(file = 'run001.lst'), ctrl_mod)
})

test_that("properly parses a model given via the runno and dir arguments", {
  expect_equal(parse_nm_model(runno = '001', ext = '.lst'), ctrl_mod)
})
