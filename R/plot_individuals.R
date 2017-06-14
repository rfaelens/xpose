#' Observations, individual predictions and population predictions plotted against 
#' the independent variable for every individual
#'
#' @description Observations (DV), individual predictions (IPRED) and population predictions (PRED) plotted against 
#' the independent variable for every individual
#' 
#' @inheritParams dv_vs_pred
#' @inheritSection xplot_scatter Layers mapping
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}}
#' @examples
#' # Basic example
#' ind_plots(xpdb_ex_pk)
#' 
#' # Example with mapping and facetting
#' ind_plots(xpdb_ex_pk, aes(x = TAD), facets = OCC~ID)
#' 
#' @export
ind_plots <- function(xpdb,
                      mapping  = NULL,
                      group    = 'variable',
                      type     = 'l',
                      facets   = NULL,
                      title    = 'Individual plots | @run',
                      subtitle = 'Ofv: @ofv, Eps shrink: @epsshk',
                      caption  = '@dir',
                      log      = NULL,
                      problem,
                      ...) {
  if (missing(problem)) problem <- last_data_problem(xpdb, simtab = FALSE)
  if (is.null(facets)) facets <- xp_var(xpdb, problem, type = 'id')$col
  
  xplot_scatter(xpdb = xpdb, group = group,
                data_opt = data_opt_set(problem = problem, 
                                        source = 'data',
                                        filter = only_obs(xpdb, problem),
                                        tidy   = TRUE,
                                        value_col = xp_var(xpdb, problem, 
                                                           type = c('dv', 'pred', 'ipred'))$col),
                mapping = aes_c(aes_string(x = xp_var(xpdb, problem, type = 'idv')$col, 
                                           y = 'value', line_color = 'variable',
                                           point_color = 'variable'), mapping),
                type = type, panel_facets = facets,
                xscale = check_scales('x', log),
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]),
                panel_scales = 'free_y', ...)
}
