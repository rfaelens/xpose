context('Check xpose_print')

ctrl_text <- 'run001.lst overview: \n - Software: nonmem 7.3.0 \n - Attached files: \n   + tables: $prob no.1: catab001, cotab001, patab001, sdtab001 \n   + sim tabs: <none> \n   + output files: run001.cor, run001.cov, run001.ext, run001.grd, run001.phi \n - gg_theme: theme_readable \n - xp_theme: theme_xp_default \n - Options: dir = analysis/models/pk/, quiet = TRUE, manual_import = NULL'

# Tests start here --------------------------------------------------------
test_that('Check proper message is returned', {
  expect_equal(capture_output(print(xpdb_ex_pk)), ctrl_text)
})
