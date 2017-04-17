context('Check basic gof functions')

# Tests start here --------------------------------------------------------

test_that("errors are returned for missing xpdb", {
  expect_error(dv_vs_pred())
  expect_error(dv_vs_ipred())
  expect_error(cwres_vs_pred())
  expect_error(cwres_vs_idv())
  expect_error(absval_iwres_vs_pred())
})

test_that("ggxpose plot objects are returned with appropriate xpdb", {
  expect_equal(class(dv_vs_pred(xpdb_ex_pk)), c('xpose_plot', 'gg', 'ggplot'))
  expect_equal(class(dv_vs_ipred(xpdb_ex_pk)), c('xpose_plot', 'gg', 'ggplot'))
  expect_equal(class(cwres_vs_pred(xpdb_ex_pk)), c('xpose_plot', 'gg', 'ggplot'))
  expect_equal(class(cwres_vs_idv(xpdb_ex_pk)), c('xpose_plot', 'gg', 'ggplot'))
  expect_equal(class(absval_iwres_vs_pred(xpdb_ex_pk)), c('xpose_plot', 'gg', 'ggplot'))
})
