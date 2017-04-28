context('Check list_nm_tables')

# Define files to be tested -----------------------------------------------

test_mod <- structure(
  dplyr::tibble(problem    = 1,
                level      = c(19, 20, 20, 21),
                subroutine = c('cov', rep('tab', 3)),
                code       = c('PRINT=E', 
                               'ID OCC DOSE AMT SS II TIME TAD IPRED CWRES CPRED IWRES',
                               'EVID ONEHEADER NOPRINT FILE =my_sdtab-001a.tab',
                               'ID KA CL V ALAG1 ETAS(1:LAST) ONEHEADER NOPRINT FILE= some-tab_001.csv'),
                comment = ''),
  file  = 'run001.mod', dir   = NULL,
  class = c('nm_model', 'tbl_df', 'tbl', 'data.frame'))

test2_mod <- structure(test_mod[1, ],
                       file  = 'run001.mod', dir   = NULL,
                       class = c('nm_model', 'tbl_df', 'tbl', 'data.frame'))

test3_mod <- structure(test_mod[1:2, ],
                       file  = 'run001.mod', dir   = NULL,
                       class = c('nm_model', 'tbl_df', 'tbl', 'data.frame'))

# Tests start here --------------------------------------------------------

test_that("error is returned when missing mod_file argument", {
  expect_error(list_nm_tables())
})

test_that("error is returned when input is not a mod_file", {
  expect_error(list_nm_tables(mod_file = dplyr::tibble(problem = 1, level = 10, 
                                                       subroutine = 'tab', code = 'FILE=sdtab001',
                                                       comment = '')))
})

test_that("null is returned no $TABLE are listed in the code", {
  expect_null(list_nm_tables(mod_file = test2_mod))
})

test_that("null is returned no valid file names are found under $TABLE", {
  expect_null(list_nm_tables(mod_file = test3_mod))
})

test_that("returns table names when proper input is provided", {
  expect_equal(list_nm_tables(mod_file = test_mod), 
               c("my_sdtab-001a.tab", "some-tab_001.csv"))
})
