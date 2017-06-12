context('Check plot functions')

# Tests start here --------------------------------------------------------

test_that("errors are returned for missing xpdb_ex_pk", {
  expect_error(dv_vs_pred())
  expect_error(dv_vs_ipred())
  expect_error(dv_vs_cpred())
  expect_error(dv_vs_idv())
  expect_error(ipred_vs_idv())
  expect_error(pred_vs_idv())
  expect_error(dv_preds_vs_idv())
  expect_error(res_vs_idv())
  expect_error(res_vs_pred())
  expect_error(absval_res_vs_idv())
  expect_error(absval_res_vs_pred())
  expect_error(prm_vs_iteration())
  expect_error(grd_vs_iteration())
  expect_error(ind_plots())
})

test_that("xpose plot objects are returned with appropriate xpdb_ex_pk", {
  expect_true(is.xpose.plot(dv_vs_pred(xpdb_ex_pk)))
  expect_true(is.xpose.plot(dv_vs_ipred(xpdb_ex_pk)))
  expect_true(is.xpose.plot(dv_vs_cpred(xpdb_ex_pk)))
  expect_true(is.xpose.plot(dv_vs_idv(xpdb_ex_pk)))
  expect_true(is.xpose.plot(ipred_vs_idv(xpdb_ex_pk)))
  expect_true(is.xpose.plot(pred_vs_idv(xpdb_ex_pk)))
  expect_true(is.xpose.plot(dv_preds_vs_idv(xpdb_ex_pk)))
  expect_true(is.xpose.plot(res_vs_idv(xpdb_ex_pk)))
  expect_true(is.xpose.plot(res_vs_pred(xpdb_ex_pk)))
  expect_true(is.xpose.plot(absval_res_vs_idv(xpdb_ex_pk)))
  expect_true(is.xpose.plot(absval_res_vs_pred(xpdb_ex_pk)))
})

