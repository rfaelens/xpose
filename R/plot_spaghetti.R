#' Observations and model predictions plotted against the independent variable
#'
#' @description Plot of observations (DV), individual model predictions (IPRED) 
#' and/or population predictions (PRED) plotted against the independent variable (IDV).
#'
#' @inheritParams dv_vs_pred
#' @inheritSection xplot_scatter Layers mapping
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}}
#' @examples
#' dv_vs_idv(xpdb_ex_pk, aes(x = TAD))
#' 
#' ipred_vs_idv(xpdb_ex_pk, aes(x = TAD))
#' 
#' pred_vs_idv(xpdb_ex_pk, aes(x = TAD))
#' 
#' dv_preds_vs_idv(xpdb_ex_pk, aes(x = TAD))
#' 
#' @name pred_vs_idv
#' @export
dv_vs_idv <- function(xpdb,
                      mapping  = NULL,
                      group    = 'ID',
                      type     = 'pls',
                      facets   = NULL,
                      title    = '@y vs. @x | @run',
                      subtitle = 'Ofv: @ofv',
                      caption  = '@dir',
                      log      = NULL,
                      problem,
                      quiet,
                      ...) {
  if (missing(problem)) problem <- last_data_problem(xpdb, simtab = FALSE)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  xplot_scatter(xpdb = xpdb, group = group, quiet = quiet,
                data_opt = data_opt_set(problem = problem, 
                                        filter = only_obs(xpdb, problem, quiet)),
                mapping = aes_c(aes_string(x = xp_var(xpdb, problem, type = 'idv')$col, 
                                           y = xp_var(xpdb, problem, type = 'dv')$col), mapping),
                type = type, panel_facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]), ...)
}


#' @name pred_vs_idv
#' @export
ipred_vs_idv <- function(xpdb,
                         mapping  = NULL,
                         group    = 'ID',
                         type     = 'pls',
                         facets   = NULL,
                         title    = '@y vs. @x | @run',
                         subtitle = 'Ofv: @ofv, Eps shrink: @epsshk',
                         caption  = '@dir',
                         log      = NULL,
                         problem,
                         quiet,
                         ...) {
  if (missing(problem)) problem <- last_data_problem(xpdb, simtab = FALSE)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  xplot_scatter(xpdb = xpdb, group = group, quiet = quiet,
                data_opt = data_opt_set(problem = problem, 
                                        filter = only_obs(xpdb, problem, quiet)),
                mapping = aes_c(aes_string(x = xp_var(xpdb, problem, type = 'idv')$col, 
                                           y = xp_var(xpdb, problem, type = 'ipred')$col), mapping),
                type = type, panel_facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]), ...)
}


#' @name pred_vs_idv
#' @export
pred_vs_idv <- function(xpdb,
                        mapping  = NULL,
                        group    = 'ID',
                        type     = 'pls',
                        facets   = NULL,
                        title    = '@y vs. @x | @run',
                        subtitle = 'Ofv: @ofv',
                        caption  = '@dir',
                        log      = NULL,
                        problem,
                        quiet,
                        ...) {
  if (missing(problem)) problem <- last_data_problem(xpdb, simtab = FALSE)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  xplot_scatter(xpdb = xpdb, group = group, quiet = quiet,
                data_opt = data_opt_set(problem = problem, 
                                        filter = only_obs(xpdb, problem, quiet)),
                mapping = aes_c(aes_string(x = xp_var(xpdb, problem, type = 'idv')$col, 
                                           y = xp_var(xpdb, problem, type = 'pred')$col), mapping),
                type = type, panel_facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]), ...)
}


#' Observations, individual model predictions and model prediction 
#' plotted against the independent variable
#'
#' @rdname pred_vs_idv
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
                            quiet,
                            ...) {
  if (missing(problem)) problem <- last_data_problem(xpdb, simtab = FALSE)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  if (is.null(facets)) facets <- 'variable'
  
  xplot_scatter(xpdb = xpdb, group = group, quiet = quiet,
                data_opt = data_opt_set(problem = problem, tidy = TRUE, 
                                        filter = only_obs(xpdb, problem, quiet),
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

