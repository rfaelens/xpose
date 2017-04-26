context('Check list_nm_tables')

# Define files to be tested -----------------------------------------------

test_mod <- structure(
  data.frame(LEVEL = c(19, 20, 20, 21),
             SUB   = c('$COVARIANCE', rep('$TABLE', 3)),
             ABREV = c('COV', rep('TAB', 3)),
             CODE  = c('PRINT=E', 
                       'ID OCC DOSE AMT SS II TIME TAD IPRED CWRES CPRED IWRES',
                       'EVID ONEHEADER NOPRINT FILE =my_sdtab-001a.tab',
                       'ID KA CL V ALAG1 ETAS(1:LAST) ONEHEADER NOPRINT FILE= some-tab_001.csv'),
             COMMENT = NA),
  class = c('mod_file', 'data.frame'))

# Tests start here --------------------------------------------------------

test_that("error is returned when missing mod_file argument", {
  expect_error(list_nm_tables())
})

test_that("error is returned when input is not a mod_file", {
  expect_error(list_nm_tables(mod_file = data.frame(LEVEL = 1, SUB = '$TABLE', 
                                                    ABREV = 'TAB', CODE = 'FILE=sdtab001',
                                                    COMMENT = NA)))
})

test_that("null is returned no $TABLE is listed in the code", {
  expect_null(list_nm_tables(mod_file = test_mod[1,]))
})

test_that("null is returned no valid file names are found under $TABLE", {
  expect_null(list_nm_tables(mod_file = test_mod[1:2,]))
})

test_that("returns table names when proper input is provided", {
  expect_equal(list_nm_tables(mod_file = test_mod), 
               c("my_sdtab-001a.tab", "some-tab_001.csv"))
})
