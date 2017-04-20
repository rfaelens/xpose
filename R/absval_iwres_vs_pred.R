#' Absolute value of individual weighted residuals (IWRES) plotted against population predictions (PRED)
#'
#' @description Plot of absolute value of individual weighted residuals (IWRES) plotted against population predictions (PRED).
#'
#' @inheritParams dv_vs_ipred
#' @inheritSection xplot_scatter Template titles
#' @return An \code{xpose_plot}
#' @seealso \code{\link{xplot_scatter}}
#' @examples
#' \dontrun{
#' absval_iwres_vs_pred(xpdb_ex_pk)
#' }
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
