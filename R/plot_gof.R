#' Observations plotted against model predictions
#'
#' @description Plot of observations (DV) vs population predictions (PRED), 
#' individual predictions (IPRED) or conditional population predictions (CPRED).
#'
#' @param xpdb An xpose database object.
#' @param mapping List of aesthetics mappings to be used for the xpose plot 
#' (e.g. \code{point_color}).
#' @param group Grouping variable to be used for lines.
#' @param type String setting the type of plot to be used points 'p',
#' line 'l' and smooth 's' or any combination of the three.
#' @param facets Either a character string to use \link[ggplot2]{facet_wrap} 
#' or a formula to use \link[ggplot2]{facet_grid}.
#' @param title Plot title. Use \code{NULL} to remove.
#' @param subtitle Plot subtitle. Use \code{NULL} to remove.
#' @param caption Page caption. Use \code{NULL} to remove.
#' @param log String assigning logarithmic scale to axes, can be either '', 
#' 'x', y' or 'xy'.
#' @param guides Enable guides display (e.g. unity line).
#' @param problem The $problem number to use for ploting. By default the data 
#' is taken from the estimation problem.
#' @param ... Any additional aesthetics to be passed on \code{xplot_scatter}.
#' 
#' @inheritSection xplot_scatter Layers mapping
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}}
#' @examples
#' dv_vs_pred(xpdb_ex_pk)
#' 
#' dv_vs_ipred(xpdb_ex_pk)
#' 
#' dv_vs_cpred(xpdb_ex_pk)
#' 
#' @name dv_vs_pred
#' @export
dv_vs_ipred <- function(xpdb,
                        mapping  = NULL,
                        group    = 'ID',
                        type     = 'pls',
                        facets   = NULL,
                        title    = '@y vs. @x | @run',
                        subtitle = 'Ofv: @ofv, Eps shrink: @epsshk',
                        caption  = '@dir',
                        log      = NULL,
                        guides   = TRUE,
                        problem,
                        ...) {
  if (missing(problem)) problem <- last_data_problem(xpdb, simtab = FALSE)
  
  xplot_scatter(xpdb = xpdb, group = group,
                data_opt = data_opt_set(problem = problem, 
                                        filter = only_obs(xpdb, problem)),
                mapping = aes_c(aes_string(x = xp_var(xpdb, problem, type = 'ipred')$col, 
                                           y = xp_var(xpdb, problem, type = 'dv')$col), mapping),
                type = type, guides = guides, panel_facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]),
                guides_slope = 1, ...)
}

#' @rdname dv_vs_pred
#' @export
dv_vs_pred <- function(xpdb,
                       mapping  = NULL,
                       group    = 'ID',
                       type     = 'pls',
                       facets   = NULL,
                       title    = '@y vs. @x | @run',
                       subtitle = 'Ofv: @ofv',
                       caption  = '@dir',
                       log      = NULL,
                       guides   = TRUE,
                       problem,
                       ...) {
  if (missing(problem)) problem <- last_data_problem(xpdb, simtab = FALSE)
  xplot_scatter(xpdb = xpdb, group = group,
                data_opt = data_opt_set(problem = problem, 
                                        filter = only_obs(xpdb, problem)),
                mapping = aes_c(aes_string(x = xp_var(xpdb, problem, type = 'pred')$col, 
                                           y = xp_var(xpdb, problem, type = 'dv')$col), mapping),
                type = type, guides = guides, panel_facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]),
                guides_slope = 1, ...)
}


#' @rdname dv_vs_pred
#' @export
dv_vs_cpred <- function(xpdb,
                        mapping  = NULL,
                        group    = 'ID',
                        type     = 'pls',
                        facets   = NULL,
                        title    = '@y vs. @x | @run',
                        subtitle = 'Ofv: @ofv',
                        caption  = '@dir',
                        log      = NULL,
                        guides   = TRUE,
                        problem,
                        ...) {
  if (missing(problem)) problem <- last_data_problem(xpdb, simtab = FALSE)
  xplot_scatter(xpdb = xpdb, group = group,
                data_opt = data_opt_set(problem = problem, 
                                        filter = only_obs(xpdb, problem)),
                mapping = aes_c(aes_string(x = xp_var(xpdb, problem, type = 'cpred')$col, 
                                           y = xp_var(xpdb, problem, type = 'dv')$col), mapping),
                type = type, guides = guides, panel_facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]),
                guides_slope = 1, ...)
}
