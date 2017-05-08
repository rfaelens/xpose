#' Conditional residuals (CWRES) plotted against population predictions (PRED)
#'
#' @description Plot of conditional residuals (CWRES) plotted against population predictions (PRED).
#'
#' @inheritParams dv_vs_ipred
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}}
#' @examples
#' cwres_vs_pred(xpdb_ex_pk)
#' 
#' @export
cwres_vs_pred <- function(xpdb,
                         aes      = NULL,
                         group    = 'ID',
                         type     = 'pls',
                         facets   = NULL,
                         title    = NULL,
                         subtitle = NULL,
                         caption  = NULL,
                         log      = NULL,
                         guides   = TRUE,
                         ...) {
  
  xplot_scatter(xpdb = xpdb, aes = aes, group = group,
                vars = aes_(x = quote(PRED), y = quote(CWRES)), 
                type = type, guides = guides, panel_facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = check_title(title, 'CWRES vs. PRED | @run'), 
                subtitle = check_title(subtitle, 'Ofv: @ofv'),
                caption = check_title(caption, '@dir'),
                plot_name = as.character(match.call()[[1]]),
                guides_slope = 0, ...)
}

#' Conditional residuals (CWRES) plotted against independent variable (IDV)
#'
#' @description Plot of conditional residuals (CWRES) plotted against independent variable (IDV).
#'
#' @inheritParams dv_vs_ipred
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}}
#' @examples
#' cwres_vs_idv(xpdb_ex_pk)
#' 
#' @export
cwres_vs_idv <- function(xpdb,
                         aes      = NULL,
                         group    = 'ID',
                         type     = 'pls',
                         facets   = NULL,
                         title    = NULL,
                         subtitle = NULL,
                         caption  = NULL,
                         log      = NULL,
                         guides   = TRUE,
                         ...) {
  
  xplot_scatter(xpdb = xpdb, aes = aes, group = group,
                vars = aes_(x = quote(TIME), y = quote(CWRES)),
                type = type, guides = guides, panel_facets = facets, 
                xscale = check_scales('x', log),
                yscale = check_scales('y', log),
                title = check_title(title, 'CWRES vs. TIME | @run'),
                subtitle = check_title(subtitle, 'Ofv: @ofv'),
                caption = check_title(caption, '@dir'),
                plot_name = as.character(match.call()[[1]]),
                guides_slope = 0, ...)
}

#' Absolute value of individual weighted residuals (IWRES) plotted against population predictions (PRED)
#'
#' @description Plot of absolute value of individual weighted residuals (IWRES) plotted against population predictions (PRED).
#'
#' @inheritParams dv_vs_ipred
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}}
#' @examples
#' absval_iwres_vs_pred(xpdb_ex_pk)
#' 
#' @export
absval_iwres_vs_pred <- function(xpdb,
                                 aes      = NULL,
                                 group    = 'ID',
                                 type     = 'pls',
                                 facets   = NULL,
                                 title    = NULL,
                                 subtitle = NULL,
                                 caption  = NULL,
                                 log      = NULL,
                                 guides   = TRUE,
                                 ...) {
  
  xplot_scatter(xpdb = xpdb, aes = aes, group = group,
                vars = aes_(x = quote(PRED), y = quote(abs(IWRES))), 
                type = type, guides = guides, panel_facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = check_title(title, '|IWRES| vs. PRED | @run'), 
                subtitle = check_title(subtitle, 'Ofv: @ofv'),
                caption = check_title(caption, '@dir'),
                plot_name = as.character(match.call()[[1]]),
                guides_slope = 0, ...)
}
