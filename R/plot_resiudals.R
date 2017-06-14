#' Residuals plotted against population predictions
#'
#' @description Model residuals plotted against population predictions (PRED). 
#' 
#' The residuals can be one of:
#' \itemize{
#'   \item RES: model residuals
#'   \item WRES: weighted model residuals
#'   \item CWRES: conditional weighted model residuals
#'   \item EWRES/ECWRES: Monte Carlo based model residuals
#'   \item NPDE: Normalized prediction distribution error 
#' }
#' 
#' @inheritParams dv_vs_pred
#' @param res Type of residual to be used. Default is "CWRES".
#' @inheritSection xplot_scatter Layers mapping
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}}
#' @examples
#' # Standard residual
#' res_vs_pred(xpdb_ex_pk, res = 'CWRES')
#' 
#' # Absolute value of the residuals
#' absval_res_vs_pred(xpdb_ex_pk, res = 'CWRES')
#' 
#' @export
res_vs_pred <- function(xpdb,
                        mapping  = NULL,
                        res      = 'CWRES',
                        group    = 'ID',
                        type     = 'pls',
                        facets   = NULL,
                        title    = '@y vs. @x | @run',
                        subtitle = 'Ofv: @ofv',
                        caption  = '@dir',
                        log      = NULL,
                        guides   = TRUE,
                        problem,
                        quiet,
                        ...) {
  if (missing(problem)) problem <- last_data_problem(xpdb, simtab = FALSE)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  xplot_scatter(xpdb = xpdb, group = group, quiet = quiet,
                data_opt = data_opt_set(problem = problem, 
                                    filter = only_obs(xpdb, problem, quiet)),
                mapping = aes_c(aes_string(x = xp_var(xpdb, problem, type = 'pred')$col, 
                                           y = toupper(res)), mapping), 
                type = type, guides = guides, panel_facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]),
                guides_slope = 0, ...)
}


#' @rdname res_vs_pred
#' @export
absval_res_vs_pred <- function(xpdb,
                               mapping  = NULL,
                               res      = 'CWRES',
                               group    = 'ID',
                               type     = 'pls',
                               facets   = NULL,
                               title    = '@y vs. @x | @run',
                               subtitle = 'Ofv: @ofv',
                               caption  = '@dir',
                               log      = NULL,
                               guides   = FALSE,
                               problem,
                               quiet,
                               ...) {
  if (missing(problem)) problem <- last_data_problem(xpdb, simtab = FALSE)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  xplot_scatter(xpdb = xpdb, group = group, quiet = quiet,
                data_opt = data_opt_set(problem = problem, 
                                        filter = only_obs(xpdb, problem, quiet)),
                mapping = aes_c(aes_string(x = xp_var(xpdb, problem, type = 'pred')$col, 
                                           y = stringr::str_c('abs(', toupper(res), ')')), mapping),
                type = type, guides = guides, panel_facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]),
                guides_slope = 0, ...)
}


#' Residuals plotted against the independent variable
#'
#' @description Model residuals plotted against the independent variable (IDV).
#' 
#' The residuals can be one of:
#' \itemize{
#'   \item RES: model residuals
#'   \item WRES: weighted model residuals
#'   \item CWRES: conditional weighted model residuals
#'   \item EWRES/ECWRES: Monte Carlo based model residuals
#'   \item NPDE: Normalized prediction distribution error 
#' }
#'
#' @inheritParams dv_vs_pred
#' @param res Type of residual to be used. Default is "CWRES".
#' @inheritSection xplot_scatter Layers mapping
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}}
#' @examples
#' # Standard residual
#' res_vs_idv(xpdb_ex_pk, res = 'CWRES', aes(x = TAD))
#' 
#' # Absolute value of the residuals
#' absval_res_vs_idv(xpdb_ex_pk, res = 'CWRES', aes(x = TAD))
#' 
#' @export
res_vs_idv <- function(xpdb,
                       mapping  = NULL,
                       res      = 'CWRES',
                       group    = 'ID',
                       type     = 'pls',
                       facets   = NULL,
                       title    = '@y vs. @x | @run',
                       subtitle = 'Ofv: @ofv',
                       caption  = '@dir',
                       log      = NULL,
                       guides   = TRUE,
                       problem,
                       quiet,
                       ...) {
  if (missing(problem)) problem <- last_data_problem(xpdb, simtab = FALSE)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  xplot_scatter(xpdb = xpdb, group = group, quiet = quiet,
                data_opt = data_opt_set(problem = problem, 
                                    filter = only_obs(xpdb, problem, quiet)),
                mapping = aes_c(aes_string(x = xp_var(xpdb, problem, type = 'idv')$col, 
                                           y = toupper(res)), mapping),
                type = type, guides = guides, panel_facets = facets, 
                xscale = check_scales('x', log),
                yscale = check_scales('y', log),
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]),
                guides_slope = 0, ...)
}


#' @rdname res_vs_idv
#' @export
absval_res_vs_idv <- function(xpdb,
                              mapping  = NULL,
                              res      = 'CWRES',
                              group    = 'ID',
                              type     = 'pls',
                              facets   = NULL,
                              title    = '@y vs. @x  | @run',
                              subtitle = 'Ofv: @ofv',
                              caption  = '@dir',
                              log      = NULL,
                              guides   = FALSE,
                              problem,
                              quiet,
                              ...) {
  if (missing(problem)) problem <- last_data_problem(xpdb, simtab = FALSE)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  xplot_scatter(xpdb = xpdb, group = group, quiet = quiet,
                data_opt = data_opt_set(problem = problem,
                                        filter = only_obs(xpdb, problem, quiet)),
                mapping = aes_c(aes_string(x = xp_var(xpdb, problem, type = 'idv')$col, 
                                           y = stringr::str_c('abs(', toupper(res), ')')), mapping), 
                type = type, guides = guides, panel_facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]),
                guides_slope = 0, ...)
}
