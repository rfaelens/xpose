context('Check plot functions')

functions_to_test <- list(dv_vs_pred,
                           dv_vs_ipred,
                           dv_vs_idv,
                           ipred_vs_idv,
                           pred_vs_idv,
                           dv_preds_vs_idv,
                           res_vs_idv,
                           res_vs_pred,
                           absval_res_vs_idv,
                           absval_res_vs_pred,
                           prm_vs_iteration,
                           grd_vs_iteration,
                           ind_plots,
                           prm_distrib,
                           eta_distrib,
                           res_distrib,
                           cov_distrib,
                           prm_qq,
                           eta_qq,
                           res_qq,
                           cov_qq)

# Tests start here --------------------------------------------------------

test_that("errors are returned for missing xpdb_ex_pk", {
  purrr::map(functions_to_test, ~expect_error(.x()))
})

test_that("xpose plot objects are returned with appropriate xpdb_ex_pk", {
  purrr::map(functions_to_test, ~expect_true(is.xpose.plot(.x(xpdb_ex_pk))))
  
  expect_true(is.xpose.plot(res_vs_idv(xpdb_ex_pk, res = c('CWRES', 'IWRES'))))
  expect_true(is.xpose.plot(res_vs_pred(xpdb_ex_pk, res = c('CWRES', 'IWRES'))))
  expect_true(is.xpose.plot(absval_res_vs_idv(xpdb_ex_pk, res = c('CWRES', 'IWRES'))))
  expect_true(is.xpose.plot(absval_res_vs_pred(xpdb_ex_pk, res = c('CWRES', 'IWRES'))))
  expect_true(is.xpose.plot(res_distrib(xpdb_ex_pk, res = c('CWRES', 'IWRES'))))
  expect_true(is.xpose.plot(res_qq(xpdb_ex_pk, res = c('CWRES', 'IWRES'))))
})


