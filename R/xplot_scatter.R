#' Default xpose scatter plot function
#'
#' @description Manually generate scatter plots from an xpdb object.
#'
#' @param xpdb an xpose database object.
#' @param vars the variable mapping from an aes().
#' @param aes ggxpose aesthetics (eg. \code{point_color}).
#' @param group grouping variable to be used.
#' @param type string setting the type of plot to be used points 'p',
#' line 'l', smooth 's' and text 't' or any combination of the 4.
#' @param layers a list of additional ggplot layers to be used.
#' @param title the main title of the plot. If NULL automated title will be generated.
#' Use FALSE to remove title and subtitle.
#' @param subtitle the plot subtitle. If NULL automated subtitle will be generated.
#' Use FALSE to remove subtitle.
#' @param caption page caption. If NULL automated caption will be generated.
#' Use FALSE to remove caption.
#' @param guides should the guides (eg. unity line) be displayed.
#' @param xscale scale type for x axis (eg. 'continuous', 'discrete', 'log10').
#' @param yscale scale type for y axis (eg. 'continuous', 'discrete', 'log10').
#' @param plot_name name that will be used by \code{xpose_save()} to save the plot.
#' @param ... any additional aesthetics.
#'
#' @section Template titles:
#' Template titles can be used to create highly informative diagnostics plots. 
#' They can be applied to any plot title, subtitle and caption. Template titles 
#' are defined via a single string containing key variables staring with a @ (e.g. @ofv)
#' which will be replaced by their actual value when rendering the plot.
#' For example '@run, @nobs observations in @nind subjects' would become 'run001, 
#' 1022 observations in 74 subjects'. The available key variables are listed under 
#' \code{\link{template_titles}}.
#' 
#' @return An \code{xpose_plot}
#' @examples
#' \dontrun{
#' xplot_scatter(xpdb, vars = aes(x = IPRED, y = DV))
#' }
#' @export
xplot_scatter <- function(xpdb,
                          vars     = NULL,
                          aes      = NULL,
                          group    = 'ID',
                          type     = 'pls',
                          layers   = NULL,
                          title    = '@run',
                          subtitle = NULL,
                          caption  = NULL,
                          guides   = TRUE,
                          xscale   = 'continuous',
                          yscale   = 'continuous',
                          plot_name = 'xplot_scatter',
                          ...) {
  
  if (!is.xpdb(xpdb)) { 
    msg('Bad input to the argument`xpdb`', verbose = TRUE)
    return(NULL)
  }
  
  # Format data
  if ('MDV' %in% colnames(xpdb$data)) {
    data <- dplyr::filter_(.data = xpdb$data, 'MDV == 0')
  } else if ('EVID' %in% colnames(xpdb$data)) {
    data <- dplyr::filter_(.data = xpdb$data, 'EVID == 0')
  }
  
  # Create ggplot base
  xp   <- ggplot(data = data, ...) + 
    xpdb$gg_theme + 
    vars
  
  # Add unity line
  if (guides) {
    xp <- xp + xp_geoms(xp_theme = xpdb$xp_theme,
                        name     = 'guides',
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
  
  # Add text (need a way to link labels = )
  # if (grepl('t', tolower(type))) {
  #   xp <- xp + xp_geoms(mapping  = aes,
  #                       xp_theme = xpdb$xp_theme,
  #                       name     = 'text',
  #                       ggfun    = 'geom_text',
  #                       ...)
  # }
  
  # Add smooth
  if (grepl('s', tolower(type))) {
    xp <- xp + xp_geoms(mapping  = aes,
                        xp_theme = xpdb$xp_theme,
                        name     = 'smooth',
                        ggfun    = 'geom_smooth',
                        ...)
  }
  
  
  # Add title and subtitle
  xp <- xp + labs(title    = write_title(title, xpdb),
                  subtitle = write_title(subtitle, xpdb),
                  caption  = write_title(caption, xpdb))
  
  
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
                        ggfun    = ifelse(is.formula(list(...)[['panel_facets']]), 
                                          'facet_grid', 'facet_wrap'),
                        ...)
  }
  
  # Add users defined layers
  if (!is.null(layers)) { xp <- xp + layers }
  
  # Add metadata to plots
  xp$xpose <- list(fun     = plot_name,
                   summary = xpdb$summary)
  
  xp <- structure(xp, class = c('xpose_plot', 'gg', 'ggplot'))
  
  return(xp)
}
