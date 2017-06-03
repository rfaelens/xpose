#' Observations plotted against model predictions
#'
#' @description Plot of observations (DV) vs population predictions (PRED), individual predictions (IPRED) 
#' or conditional population predictions (CPRED).
#'
#' @param xpdb An xpose database object.
#' @param aes The xpose aesthetics (eg. \code{point_color}).
#' @param group Grouping variable to be used for lines.
#' @param type String setting the type of plot to be used points 'p',
#' line 'l' and smooth 's' or any combination of the three.
#' @param facets Either a character string to use \link[ggplot2]{facet_wrap} or a formula 
#' to use \link[ggplot2]{facet_grid}.
#' @param title Plot title. Use \code{NULL} to remove.
#' @param subtitle Plot subtitle. Use \code{NULL} to remove.
#' @param caption Page caption. Use \code{NULL} to remove.
#' @param log String assigning logarithmic scale to axes, can be either '', 'x', y' or 'xy'.
#' @param guides Enable guides display (e.g. unity line).
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
                        aes      = NULL,
                        group    = 'ID',
                        type     = 'pls',
                        facets   = NULL,
                        title    = 'DV vs. IPRED | @run',
                        subtitle = 'Ofv: @ofv, Eps shrink: @epsshk',
                        caption  = '@dir',
                        log      = NULL,
                        guides   = TRUE,
                        ...) {
  
  xplot_scatter(xpdb = xpdb, aes = aes, group = group,
                vars = aes_string(x = 'IPRED', y = 'DV'), 
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
                       aes      = NULL,
                       group    = 'ID',
                       type     = 'pls',
                       facets   = NULL,
                       title    = 'DV vs. PRED | @run',
                       subtitle = 'Ofv: @ofv',
                       caption  = '@dir',
                       log      = NULL,
                       guides   = TRUE,
                       ...) {
  
  xplot_scatter(xpdb = xpdb, aes = aes, group = group,
                vars = aes_string(x = 'PRED', y = 'DV'), 
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
                        aes      = NULL,
                        group    = 'ID',
                        type     = 'pls',
                        facets   = NULL,
                        title    = 'DV vs. CPRED | @run',
                        subtitle = 'Ofv: @ofv',
                        caption  = '@dir',
                        log      = NULL,
                        guides   = TRUE,
                        ...) {
  
  xplot_scatter(xpdb = xpdb, aes = aes, group = group,
                vars = aes_string(x = 'CPRED', y = 'DV'), 
                type = type, guides = guides, panel_facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]),
                guides_slope = 1, ...)
}
