#' Absolute value of individual weighted residuals (IWRES) plotted against population predictions (PRED)
#'
#' @description Plot of absolute value of individual weighted residuals (IWRES) plotted against population predictions (PRED).
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
#' @param gg_theme a ggplot2 theme to be used on this specific plot.
#' @param ... any additional aesthetics.
#'
#' @export
absval_iwres_vs_pred <- function(xpdb,
                                 aes      = NULL,
                                 group    = 'ID',
                                 type     = 'pls',
                                 by       = NULL,
                                 layers   = NULL,
                                 title    = NULL,
                                 subtitle = NULL,
                                 log      = FALSE,
                                 gg_theme = NULL,
                                 ...) {

  check_xpdb(xpdb)
  check_vars(c('PRED', 'IWRES', by), xpdb)

  vars   <- aes_(x = quote(PRED), y = quote(abs(IWRES)))
  xscale <- ifelse(log, 'log10', 'continuous')
  yscale <- 'continuous'
  titles <- titlr('|IWRES| vs. PRED', subfun = 'eps_shrink',
                  title, subtitle, xpdb)

  xpose_plot_default(xpdb = xpdb, vars = vars, aes = aes, group = group,
                     layers = layers, type = type, guides = FALSE,
                     gg_theme = gg_theme, panel_facets = by, xscale = xscale,
                     yscale = yscale, title = titles[1], subtitle = titles[2],
                     plot_name = as.character(match.call()[[1]]), ...)
}

