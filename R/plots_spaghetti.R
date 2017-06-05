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
                            aes      = NULL,
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
  
  xplot_scatter(xpdb = xpdb, aes = aes, group = group,
                data_opt = data_opt(problem = problem, 
                                    filter = function(x) x[x[, 'EVID'] == 0, c('ID', 'TAD', 'DV', 'IPRED', 'PRED')],
                                    tidy = TRUE, index_col = c('ID', 'TAD')),
                vars = aes_string(x = 'TAD',#xp_var(xpdb, problem, type = 'idv')$col, 
                                  y = 'value'), 
                type = type, guides = FALSE, panel_facets = 'variable', 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]), ...)
}

