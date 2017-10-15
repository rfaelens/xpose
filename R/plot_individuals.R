#' Observations, individual predictions and population predictions plotted against 
#' the independent variable for every individual
#'
#' @description Observations (DV), individual predictions (IPRED) and population predictions (PRED) plotted against 
#' the independent variable for every individual
#' 
#' @inheritParams dv_vs_pred
#' @param point_alpha Points alpha, should be a vector of 3 values (i.e. DV, IPRED, PRED).
#' @param point_color Points (and lines) color, should be a vector of 3 values (i.e. DV, IPRED, PRED).
#' @param line_linetype Lines linetype, should be a vector of 3 values (i.e. DV, IPRED, PRED).
#' @inheritSection xplot_scatter Layers mapping
#' @inheritSection xplot_scatter Faceting
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}}
#' @examples
#' # Basic example
#' ind_plots(xpdb_ex_pk, page = 1,
#'           ncol = 3, nrow = 3)
#' 
#' # Example with custom mapping and faceting
#' ind_plots(xpdb_ex_pk, aes(x = TAD), 
#'           facets = SEX~ID)
#' 
#' @export
ind_plots <- function(xpdb,
                      mapping  = NULL,
                      group    = 'variable',
                      type     = 'lp',
                      title    = 'Individual plots | @run',
                      subtitle = 'Ofv: @ofv, Eps shrink: @epsshk',
                      caption  = '@dir | Page @page of @lastpage',
                      log      = NULL,
                      facets,
                      problem,
                      quiet,
                      point_alpha   = c(0.8, 0, 0),
                      point_color   = c('grey60', 'deepskyblue4', 'deepskyblue3'),
                      line_linetype = c('blank', 'solid', '55'),
                      ...) {
  # Check input
  check_xpdb(xpdb, check = 'data')
  if (missing(problem)) problem <- default_plot_problem(xpdb)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  if (missing(facets)) facets <- add_facet_var(facets = xpdb$xp_theme$facets,
                                               variable = xp_var(xpdb, problem, type = 'id')$col)

  extra_args <- list(...)
  if (!any(names(extra_args) == 'nrow')) extra_args$nrow <- 3
  if (!any(names(extra_args) == 'ncol')) extra_args$ncol <- 3

  do.call('xplot_scatter', 
          c(extra_args, 
            list(xpdb = xpdb, group = group, quiet = quiet,
                 opt = data_opt(problem = problem, source = 'data',
                                filter = only_obs(xpdb, problem, quiet),
                                tidy = TRUE,
                                value_col = xp_var(xpdb, problem, 
                                                   type = c('dv', 'pred', 'ipred'))$col),
                 mapping = aes_c(aes_string(x = xp_var(xpdb, problem, type = 'idv')$col, 
                                            y = 'value', line_color = 'variable', 
                                            line_linetype = 'variable', point_color = 'variable', 
                                            point_alpha = 'variable'), mapping),
                 type = type, facets = facets,
                 xscale = check_scales('x', log),
                 yscale = check_scales('y', log), 
                 title = title, subtitle = subtitle, caption = caption,
                 plot_name = as.character(match.call()[[1]])))) +
    scale_alpha_manual(values = point_alpha) +
    scale_color_manual(values = point_color) +
    scale_linetype_manual(values = line_linetype)
}
