#' Conditional residuals (CWRES) plotted against independent variable (IDV)
#'
#' @description Plot of conditional residuals (CWRES) plotted against independent variable (IDV).
#'
#' @param xpdb an xpose database object.
#' @param aes ggxpose aesthetics (eg. \code{point_color}).
#' @param group grouping variable to be used.
#' @param type string setting the type of plot to be used points 'p',
#' line 'l' and smooth 's' or any combination of the 3.
#' @param by variable to be used for faceting.
#' @param layers a list of additional ggplot layers to be used.
#' @param title the main title of the plot. If NULL automated title will be generated.
#' Use FALSE to remove title and subtitle.
#' @param subtitle the plot subtitle. If NULL automated subtitle will be generated.
#' Use FALSE to remove subtitle.
#' @param log logical if TRUE axes will be logged.
#' @param guides should the guides (eg. unity line) be displayed.
#' @param gg_theme a ggplot2 theme to be used on this specific plot.
#' @param ... any additional aesthetics.
#'
#' @export
cwres_vs_idv <- function(xpdb,
                         aes      = NULL,
                         group    = 'ID',
                         type     = 'pls',
                         by       = NULL,
                         layers   = NULL,
                         title    = NULL,
                         subtitle = NULL,
                         log      = FALSE,
                         guides   = TRUE,
                         gg_theme = NULL,
                         ...) {

  check_xpdb(xpdb)
  check_vars(c('TIME', 'CWRES', by), xpdb)

  vars   <- aes_(x = quote(TIME), y = quote(CWRES))
  xscale <- ifelse(log, 'log10', 'continuous')
  yscale <- 'continuous'
  titles <- titlr('CWRES vs. TIME', subfun = 'ofv',
                  title, subtitle, xpdb)

  xpose_plot_default(xpdb = xpdb, vars = vars, aes = aes, group = group,
                     layers = layers, type = type, guides = guides,
                     gg_theme = gg_theme, panel_facets = by, xscale = xscale,
                     yscale = yscale, title = titles[1], subtitle = titles[2],
                     plot_name = as.character(match.call()[[1]]),
                     guide_slope = 0, guide_intercept = 0, ...)
}
