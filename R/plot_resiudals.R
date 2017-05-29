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
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}}
#' @examples
#' res_vs_pred(xpdb_ex_pk, res = 'CWRES')
#' 
#' @export
res_vs_pred <- function(xpdb,
                        aes      = NULL,
                        res      = 'CWRES',
                        group    = 'ID',
                        type     = 'pls',
                        facets   = NULL,
                        title    = 'CWRES vs. PRED | @run',
                        subtitle = 'Ofv: @ofv',
                        caption  = '@dir',
                        log      = NULL,
                        guides   = TRUE,
                        ...) {
  
  xplot_scatter(xpdb = xpdb, aes = aes, group = group,
                vars = aes_(x = quote(PRED), y = quote(CWRES)), 
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
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}}
#' @examples
#' res_vs_idv(xpdb_ex_pk, res = 'CWRES')
#' 
#' @export
res_vs_idv <- function(xpdb,
                       aes      = NULL,
                       res      = 'CWRES',
                       group    = 'ID',
                       type     = 'pls',
                       facets   = NULL,
                       title    = 'CWRES vs. TIME | @run',
                       subtitle = 'Ofv: @ofv',
                       caption  = '@dir',
                       log      = NULL,
                       guides   = TRUE,
                       ...) {
  
  xplot_scatter(xpdb = xpdb, aes = aes, group = group,
                vars = aes_(x = quote(TIME), y = quote(CWRES)),
                type = type, guides = guides, panel_facets = facets, 
                xscale = check_scales('x', log),
                yscale = check_scales('y', log),
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]),
                guides_slope = 0, ...)
}

#' Absolute value of residuals plotted against population predictions
#'
#' @description Absolute value of the model residuals plotted against the population predictions (PRED).
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
#' 
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}}
#' @examples
#' absval_res_vs_pred(xpdb_ex_pk, res = 'CWRES')
#' 
#' @export
absval_res_vs_pred <- function(xpdb,
                               aes      = NULL,
                               res      = "CWRES",
                               group    = 'ID',
                               type     = 'pls',
                               facets   = NULL,
                               title    = '|CWRES| vs. PRED | @run',
                               subtitle = 'Ofv: @ofv',
                               caption  = '@dir',
                               log      = NULL,
                               guides   = FALSE,
                               ...) {
  
  xplot_scatter(xpdb = xpdb, aes = aes, group = group,
                vars = aes_(x = quote(PRED), y = quote(abs(CWRES))), 
                type = type, guides = guides, panel_facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]),
                guides_slope = 0, ...)
}

#' Absolute value of residuals plotted against independent variable
#'
#' @description Absolute value of the model residuals plotted against the independent variable (IDV).
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
#' 
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}}
#' @examples
#' absval_res_vs_idv(xpdb_ex_pk, res = 'CWRES')
#' 
#' @export
absval_res_vs_idv <- function(xpdb,
                              aes      = NULL,
                              res      = "CWRES",
                              group    = 'ID',
                              type     = 'pls',
                              facets   = NULL,
                              title    = '|CWRES| vs. TIME | @run',
                              subtitle = 'Ofv: @ofv',
                              caption  = '@dir',
                              log      = NULL,
                              guides   = FALSE,
                              ...) {
  
  xplot_scatter(xpdb = xpdb, aes = aes, group = group,
                vars = aes_(x = quote(TIME), y = quote(abs(CWRES))), 
                type = type, guides = guides, panel_facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]),
                guides_slope = 0, ...)
}
