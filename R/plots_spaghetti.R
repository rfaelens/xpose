#' Observations, individual model predictions and model prediction 
#' plotted against the independent variable
#'
#' @description Plot of observations (DV), individual model predictions (IPRED) 
#' and population predictions (PRED) plotted against the independent variable (IDV).
#'
#' @inheritParams dv_vs_pred
#' @inheritSection xplot_scatter Layers mapping
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}}
#' @examples
#' dv_preds_vs_idv(xpdb_ex_pk)
#' 
#' @export
dv_preds_vs_idv <- function(xpdb,
                            mapping  = NULL,
                            group    = 'ID',
                            type     = 'pls',
                            facets   = NULL,
                            title    = 'Observations, Individual predictions and Population predictions vs. @x | @run',
                            subtitle = 'Ofv: @ofv, Eps shrink: @epsshk',
                            caption  = '@dir',
                            log      = NULL,
                            problem,
                            ...) {
  if (missing(problem)) problem <- last_data_problem(xpdb, simtab = FALSE)
  if (missing(facets)) facets <- 'variable'
  
  xplot_scatter(xpdb = xpdb, group = group,
                data_opt = data_opt_set(problem = problem, tidy = TRUE, 
                                        filter = only_obs(xpdb, problem),
                                        value_col = xp_var(xpdb, problem, 
                                                           type = c('dv', 'pred', 'ipred'))$col),
                mapping = aes_c(aes_string(x = xp_var(xpdb, problem, type = 'idv')$col, 
                                           y = 'value'), mapping), 
                type = type, guides = FALSE, panel_facets = facets,
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]), ...)
}

