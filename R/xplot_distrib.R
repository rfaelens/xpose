#' Default xpose distribution plot function
#'
#' @description Manually generate distribution plots from an xpdb object.
#'
#' @inheritParams xplot_scatter
#' @param type String setting the type of plot to be used histogram 'h',
#' density 'd', rug 'r' the three.
#' @param guides Should the guides (e.g. reference distribution) be displayed.

#' @section Layers mapping:
#' Plots can be customized by mapping arguments to specific layers. The naming convention is 
#' layer_option where layer is one of the names defined in the list below and option is 
#' any option supported by this layer e.g. histogram_fill = 'blue', rug_sides = 'b', etc.
#' \itemize{
#'   \item histogram: options to \code{geom_histogram}
#'   \item density: options to \code{geom_density}
#'   \item panel: options to \code{facet_wrap} (facets is character) or \code{facet_grid} 
#'   (facets is a formula)
#'   \item rug: options to \code{geom_rug}
#'   \item xscale: options to \code{scale_x_continuous} or \code{scale_x_log10}
#'   \item yscale: options to \code{scale_y_continuous} or \code{scale_y_log10}
#' }
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}}
#' 
#' @examples
#' # A simple histogram
#' xplot_distrib(xpdb_ex_pk, aes(x = WT))
#' 
#' # A simple density plot
#' xplot_distrib(xpdb_ex_pk, aes(x = CWRES))
#' 
#' @export
xplot_distrib <- function(xpdb,
                          mapping   = NULL,
                          type      = 'hr',
                          guides    = FALSE,
                          xscale    = 'continuous',
                          yscale    = 'continuous',
                          title     = NULL,
                          subtitle  = NULL,
                          caption   = NULL,
                          plot_name = 'density_plot',
                          gg_theme,
                          xp_theme,
                          data_opt,
                          quiet,
                          ...) {
  
  # Check input
  if (!is.xpdb(xpdb)) { 
    msg('Bad input to the argument`xpdb`', ifelse(missing(quiet), TRUE, quiet))
    return()
  }
  
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  # Fetch data
  if (missing(data_opt)) data_opt <- data_opt_set()
  data <- fetch_data(xpdb, quiet = quiet, problem = data_opt$problem, subprob = data_opt$subprob, 
                     source = data_opt$source, simtab = data_opt$simtab, filter = data_opt$filter, 
                     tidy = data_opt$tidy, index_col = data_opt$index_col, value_col = data_opt$value_col)
  if (is.null(data)) {
    msg('No data available for plotting. Please check the variable mapping and filering options.', quiet)
    return()
  }
  
  # Assing xp_theme and gg_theme
  if (!missing(xp_theme)) xpdb <- update_themes(xpdb = xpdb, xp_theme = xp_theme)
  if (missing(gg_theme)) gg_theme <- xpdb$gg_theme
  
  # Create ggplot base
  xp <- ggplot(data = data, mapping, ...) + gg_theme 
  
  # Add histogram
  if (stringr::str_detect(type, stringr::fixed('h', ignore_case = TRUE))) {
    xp <- xp + xp_geoms(mapping  = mapping,
                        xp_theme = xpdb$xp_theme,
                        name     = 'histogram',
                        ggfun    = 'geom_histogram',
                        ...)
  }
  
  # Add density
  if (stringr::str_detect(type, stringr::fixed('d', ignore_case = TRUE))) {
    xp <- xp + xp_geoms(mapping  = mapping,
                        xp_theme = xpdb$xp_theme,
                        name     = 'density',
                        ggfun    = 'geom_density',
                        ...)
  }
  
  # Add rug
  if (stringr::str_detect(type, stringr::fixed('r', ignore_case = TRUE))) {
    xp <- xp + xp_geoms(mapping  = mapping,
                        xp_theme = xpdb$xp_theme,
                        name     = 'rug',
                        ggfun    = 'geom_rug',
                        ...)
  }
  
  # Add reference distibution
  if (guides) {
    msg('Reference distribution not available yet.', quiet)
    # xp <- xp + xp_geoms(xp_theme = xpdb$xp_theme,
    #                     name     = 'guides',
    #                     ggfun    = 'geom_line',
    #                     ...)
  }
  
  # Define scales
  xp <- xp + 
    xp_geoms(mapping  = mapping,
             xp_theme = xpdb$xp_theme,
             name     = 'xscale',
             ggfun    = paste0('scale_x_', xscale),
             ...) +
    xp_geoms(mapping  = mapping,
             xp_theme = xpdb$xp_theme,
             name     = 'yscale',
             ggfun    = paste0('scale_y_', yscale),
             ...)
  
  # Define panels
  if (!is.null(list(...)[['panel_facets']])) {
    if (!is.formula(list(...)[['panel_facets']])) {
      xp <- xp + xp_geoms(mapping  = mapping,
                          xp_theme = xpdb$xp_theme,
                          name     = 'panel',
                          ggfun    = 'facet_wrap_paginate',
                          ...)
    } else {
      xp <- xp + xp_geoms(mapping  = mapping,
                          xp_theme = filter_xp_theme(xpdb$xp_theme, 'panel_dir', 'drop'),
                          name     = 'panel',
                          ggfun    = 'facet_grid_paginate',
                          ...)
    }
  }
  
  # Add labels
  xp <- xp + labs(title = title, subtitle = subtitle, caption = caption)
  
  # Add metadata to plots
  xp$xpose <- list(fun      = plot_name,
                   summary  = xpdb$summary,
                   problem  = attr(data, 'problem'),
                   quiet    = quiet,
                   xp_theme = xpdb$xp_theme[stringr::str_c(c('title', 'subtitle', 'caption'), 
                                                           '_suffix')])
  
  # Ouptut the plot
  structure(xp, class = c('xpose_plot', class(xp)))
}
