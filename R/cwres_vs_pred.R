#' Conditional residuals (CWRES) plotted against population predictions (PRED)
#'
#' @description Plot of conditional residuals (CWRES) plotted against population predictions (PRED).
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
#'
#' @export
cwres_vs_pred <- function(xpdb,
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
  plot_name <- as.character(match.call()[[1]])

  check_vars(c('PRED', 'CWRES', by), xpdb)
  vars   <- aes_(x = quote(PRED), y = quote(CWRES))
  xscale <- ifelse(log, 'log10', 'continuous')
  yscale <- 'continuous'
  guide_slope     <- 0
  guide_intercept <- 0
  title_label     <- 'CWRES vs. PRED'

  if (is.null(title)) { title <- paste0(title_label, ' (', xpdb$modfile, ')') }
  if (is.null(subtitle)) { subtitle <- xpdb$mod_info$ofv }

  xpose_plot_default(xpdb = xpdb, vars = vars, aes = aes, group = group,
                     layers = layers, type = type, title = title,
                     subtitle = subtitle, guides = guides, panel_facets = by,
                     xscale = xscale, yscale = yscale, gg_theme = gg_theme,
                     plot_name = plot_name, guide_slope = guide_slope,
                     guide_intercept = guide_intercept, ...)
}
