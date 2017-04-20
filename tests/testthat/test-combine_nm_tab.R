context('Check combine_nm_tab')

# Define files to be tested -----------------------------------------------

test_mod_false <- data.frame(LEVEL = c(19, 20, 20, 21),
                             SUB   = c('$COVARIANCE', rep('$TABLE', 3)),
                             ABREV = c('COV', rep('TAB', 3)),
                             CODE  = c('PRINT=E', 
                                       'ID OCC DOSE AMT SS II TIME TAD IPRED CWRES CPRED IWRES',
                                       'EVID ONEHEADER NOPRINT FILE=fake_sdtab.tab',
                                       'ID KA CL V ALAG1 ETAS(1:LAST) ONEHEADER NOPRINT FILE=fake_patab.tab'),
                             COMMENT = NA)

test_mod_true <- data.frame(LEVEL = c(19, 20, 20, 21),
                            SUB   = c('$COVARIANCE', rep('$TABLE', 3)),
                            ABREV = c('COV', rep('TAB', 3)),
                            CODE  = c('PRINT=E', 
                                      'ID OCC DOSE AMT SS II TIME TAD IPRED CWRES CPRED IWRES',
                                      'EVID ONEHEADER NOPRINT FILE =sdtab001 ',
                                      'ID KA CL V ALAG1 ETAS(1:LAST) ONEHEADER NOPRINT FILE= patab001'),
                            COMMENT = NA)

ctrl_tab   <- xpdb_ex_pk$data[, union(xpdb_ex_pk$tab_index$sdtab001, xpdb_ex_pk$tab_index$patab001)]


# Tests start here --------------------------------------------------------

test_that("error is returned when missing mod_file argument", {
  expect_error(combine_nm_tab())
})

test_that("error is returned when all table listed in mod_file are missing", {
  expect_null(combine_nm_tab(mod_file = test_mod_false, verbose = FALSE))
})

test_that("returns a proper table with index when valid arguments are provided", {
  test_tab <- combine_nm_tab(mod_file = test_mod_true)
  expect_equal(names(test_tab), c("data",  "index"))
  expect_equal(test_tab$data, ctrl_tab)
})
