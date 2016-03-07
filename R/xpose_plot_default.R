#' Default xpose plot
#'
#' @description manually generate plotting functions.
#'
#' @param xpdb an xpose database object.
#' @param aes ggxpose aesthetics (eg. \code{point_color}).
#' @param group grouping variable to be used.
#' @param type string setting the type of plot to be used points 'p',
#' line 'l' and smooth 's' or any combination of the 3.
#' @param layers a list of additional ggplot layers to be used.
#' @param title the main title of the plot. If NULL automated title will be generated.
#' Use FALSE to remove title and subtitle.
#' @param subtitle the plot subtitle. If NULL automated subtitle will be generated.
#' Use FALSE to remove subtitle.
#' @param guides should the guides (eg. unity line) be displayed.
#' @param xscale scale type for x axis (eg. 'continuous', 'discrete', 'log10').
#' @param yscale scale type for y axis (eg. 'continuous', 'discrete', 'log10').
#' @param gg_theme a ggplot2 theme to be used on this specific plot.
#' @param plot_name name that will be used by \code{xpose_save()} to save the plot.
#'
#' @return An \code{xpose_plot}
#' @examples
#' \dontrun{
#'
#' }
#' @export
xpose_plot_default <- function(xpdb,
                               vars     = NULL,
                               aes      = NULL,
                               group    = 'ID',
                               type     = 'pls',
                               layers   = NULL,
                               title    = NULL,
                               subtitle = NULL,
                               guides   = TRUE,
                               xscale   = 'continuous',
                               yscale   = 'continuous',
                               gg_theme = NULL,
                               plot_name = 'xpose_plot_default',
                               ...) {

  check_xpdb(xpdb) # Check inputs

  # Format data
  if ('MDV' %in% colnames(xpdb$data)) {
    data <- dplyr::filter_(.data = xpdb$data, 'MDV == 0')
  } else if ('EVID' %in% colnames(xpdb$data)) {
    data <- dplyr::filter_(.data = xpdb$data, 'EVID == 0')
  }

  # Create ggplot base
  xp   <- ggplot(data = data, ...) + vars

  # Add unity line
  if (guides) {
    xp <- xp + xp_geoms(xp_theme = xpdb$xp_theme,
                        name     = 'guide',
                        ggfun    = 'geom_abline',
                        ...)
  }

  # Add lines
  if (grepl('l', tolower(type))) {
    xp <- xp + xp_geoms(mapping  = aes,
                        xp_theme = xpdb$xp_theme,
                        group    = group,
                        name     = 'line',
                        ggfun    = 'geom_line',
                        ...)
  }

  # Add points
  if (grepl('p', tolower(type))) {
    xp <- xp + xp_geoms(mapping  = aes,
                        xp_theme = xpdb$xp_theme,
                        name     = 'point',
                        ggfun    = 'geom_point',
                        ...)
  }

  # Add smooth
  if (grepl('s', tolower(type))) {
    xp <- xp + xp_geoms(mapping  = aes,
                        xp_theme = xpdb$xp_theme,
                        name     = 'smooth',
                        ggfun    = 'geom_smooth',
                        ...)
  }


  # Add title and subtitle

  if (!(is.logical(title) && title == FALSE)) {
    if (!(is.logical(subtitle) && subtitle == FALSE)) {
      xp <- xp + labs(title = bquote(atop(bold(.(title)), scriptstyle(.(subtitle)))))
    } else {
      xp <- xp + labs(title = title)
    }
  }

  # Define scales
  xp <- xp + xp_geoms(mapping  = aes,
                      xp_theme = xpdb$xp_theme,
                      name     = 'xscale',
                      ggfun    = paste0('scale_x_', xscale),
                      ...)

  xp <- xp + xp_geoms(mapping  = aes,
                      xp_theme = xpdb$xp_theme,
                      name     = 'yscale',
                      ggfun    = paste0('scale_y_', yscale),
                      ...)

  # Define panels
  if (!is.null(list(...)[['panel_facets']])) {
    xp <- xp + xp_geoms(mapping  = aes,
                        xp_theme = xpdb$xp_theme,
                        name     = 'panel',
                        ggfun    = 'facet_wrap',
                        ...)
  }

  # Add users defined layers
  if (!is.null(layers)) { xp <- xp + layers }

  # Add themes
  if (is.null(gg_theme)) {
    xp <- xp + xpdb$gg_theme
  } else {
    xp <- xp + gg_theme
  }

  # Add metadata
  xp$xpose <- list(modfile = xpdb$modfile,
                   descr   = xpdb$descr,
                   fun     = plot_name)

  xp <- structure(xp, class = c('xpose_plot', 'gg', 'ggplot'))

  return(xp)
}
