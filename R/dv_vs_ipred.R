#' Observations (DV) plotted against individual predictions (IPRED)
#'
#' @description Plot of observations (DV) vs individual predictions (IPRED).
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
dv_vs_ipred <- function(xpdb,
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

  check_vars(c('IPRED','DV'))
  vars   <- aes_string(x = 'IPRED', y = 'DV')
  xscale <- ifelse(log, 'log10', 'continuous')
  yscale <- xscale
  gslope <- 1
  fun_name  <- 'DV vs. IPRED'

  if (is.null(title)) { title <- paste0(fun_name, ' (', xpdb$modfile, ')') }
  if (is.null(subtitle)) { subtitle <- xpdb$mod_info$eps_shrink }

  xpose_plot_default(xpdb = xpdb, vars = vars, aes = aes, group = group,
                     layers = layers, type = type, by = by, title = title,
                     subtitle = subtitle, guides = guides, xscale = xscale,
                     yscale = yscale, gg_theme = gg_theme,
                     plot_name = plot_name, guide_slope = gslope, ...)
}
