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
#' @param caption page caption. If NULL automated caption will be generated.
#' Use FALSE to remove caption.
#' @param log logical if TRUE axes will be logged.
#' @param guides should the guides (eg. unity line) be displayed.
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
                                 caption  = NULL,
                                 log      = FALSE,
                                 guides   = TRUE,
                                 gg_theme = NULL,
                                 ...) {
  
  ##### Change this to true false
  check_xpdb(xpdb)
  check_vars(c('IWRES', 'PRED', by), xpdb)
  #######
  
  xpose_plot_default(xpdb = xpdb, aes = aes, group = group,
                     vars = aes_(x = quote(PRED), y = quote(abs(IWRES))), 
                     layers = layers, type = type, guides = guides,
                     gg_theme = gg_theme, panel_facets = by, 
                     xscale = ifelse(log, 'log10', 'continuous'),
                     yscale = 'continuous', 
                     title = check_title(title, '|IWRES| vs. PRED | @run'), 
                     subtitle = check_title(subtitle, 'Ofv: @ofv'),
                     caption = check_title(caption, '@dir'),
                     plot_name = as.character(match.call()[[1]]),
                     guides_slope = 0, ...)
}


