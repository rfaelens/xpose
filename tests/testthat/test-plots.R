context('Check plot functions')

prediction_plot_functions <- c('dv_vs_pred', 'dv_vs_ipred', 'dv_vs_idv', 
                               'ipred_vs_idv', 'pred_vs_idv', 'dv_preds_vs_idv')
residual_plot_functions   <- c('res_vs_idv', 'res_vs_pred', 'absval_res_vs_idv', 
                               'absval_res_vs_pred', 'res_distrib', 'res_qq') 
iteration_plot_functions  <- c('prm_vs_iteration', 'grd_vs_iteration')
parameter_plot_functions  <- c('prm_distrib', 'prm_qq')
eta_plot_functions        <- c('eta_distrib', 'eta_qq')
cov_plot_functions        <- c('cov_distrib', 'cov_qq')
individual_plot_functions <- c('ind_plots')

plot_functions <- c(prediction_plot_functions, residual_plot_functions, 
                    iteration_plot_functions, parameter_plot_functions, 
                    eta_plot_functions, cov_plot_functions, 
                    individual_plot_functions)

not_iteration_functions <- plot_functions[!grepl('iteration', plot_functions)]

xpdb_sim_only <- xpose_data(file = 'sim.lst', dir = 'data')


# Tests start here --------------------------------------------------------

test_that_for_all(plot_functions, 'errors are returned when xpdb_ex_pk is missing', {
   expect_error(.plot_function())
})

test_that_for_all(plot_functions, 'xpose plot objects are returned with appropriate xpdb_ex_pk', {
  expect_true(is.xpose.plot(.plot_function(xpdb_ex_pk)))
})

test_that_for_all(residual_plot_functions, 'xpose plot objects are returned with appropriate xpdb_ex_pk and muliple residuals', {
  expect_true(is.xpose.plot(.residual_plot_function(xpdb_ex_pk, res = c('CWRES', 'IWRES'))))
})

# Need to fix errors with distrib plots
# test_that_for_all(not_iteration_functions, 'no error occurs when xpdb is from a simulation only', {
#   expect_error(.not_iteration_function(xpdb_sim_only), NA)
# })

test_that_for_all(iteration_plot_functions, 'error occurs when xpdb is from a simulation only', {
  expect_error(.iteration_plot_functions(xpdb_sim_only))
})


