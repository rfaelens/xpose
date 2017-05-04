#' Observations (DV) plotted against individual predictions (IPRED)
#'
#' @description Plot of observations (DV) vs individual predictions (IPRED).
#'
#' @param xpdb An xpose database object.
#' @param aes The xpose aesthetics (eg. \code{point_color}).
#' @param group Grouping variable to be used for lines.
#' @param type String setting the type of plot to be used points 'p',
#' line 'l' and smooth 's' or any combination of the three.
#' @param facets Either a character string to use \link[ggplot2]{facet_wrap} or a formula 
#' to use \link[ggplot2]{facet_grid}.
#' @param title Main title of the plot. If NULL automated title will be generated.
#' Use \code{FALSE} to remove title and subtitle.
#' @param subtitle the plot subtitle. If NULL automated subtitle will be generated.
#' Use \code{FALSE} to remove subtitle.
#' @param caption page caption. If NULL automated caption will be generated.
#' Use \code{FALSE} to remove caption.
#' @param log String assigning logarithmic scale to axes, can be either '', 'x', y' or 'xy'.
#' @param guides Enable guides display (e.g. unity line).
#' @param ... Any additional aesthetics to be passed on \code{xplot_scatter}.
#' 
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}}
#' @examples
#' \dontrun{
#' dv_vs_ipred(xpdb_ex_pk)
#' }
#' @export
dv_vs_ipred <- function(xpdb,
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
                vars = aes_(x = quote(IPRED), y = quote(DV)), 
                type = type, guides = guides, panel_facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = check_title(title, 'DV vs. IPRED | @run'), 
                subtitle = check_title(subtitle, 'Ofv: @ofv, @epsshr'),
                caption = check_title(caption, '@dir'),
                plot_name = as.character(match.call()[[1]]),
                guides_slope = 1, ...)
}

#' Observations (DV) plotted against population predictions (PRED)
#'
#' @description Plot of observations (DV) vs population predictions (PRED).
#'
#' @inheritParams dv_vs_ipred
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}}
#' @examples
#' \dontrun{
#' dv_vs_pred(xpdb_ex_pk)
#' }
#' @export
dv_vs_pred <- function(xpdb,
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
                vars = aes_(x = quote(PRED), y = quote(DV)), 
                type = type, guides = guides, panel_facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = check_title(title, 'DV vs. PRED | @run'), 
                subtitle = check_title(subtitle, 'Ofv: @ofv'),
                caption = check_title(caption, '@dir'),
                plot_name = as.character(match.call()[[1]]),
                guides_slope = 1, ...)
}
