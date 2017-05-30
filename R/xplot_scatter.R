#' Default xpose scatter plot function
#'
#' @description Manually generate scatter plots from an xpdb object.
#'
#' @param xpdb An \code{xpose_data} object generated with \code{\link{xpose_data}}.
#' @param vars Variable mapping using the \code{\link[ggplot2]{aes}} function.
#' @param aes xpose aesthetics (eg. \code{point_color}).
#' @param group Grouping variable to be used for lines.
#' @param type String setting the type of plot to be used points 'p',
#' line 'l', smooth 's' and text 't' or any combination of the 4.
#' @param layers A list of additional ggplot layers to be added to the plot.
#' @param guides should the guides (eg. unity line) be displayed.
#' @param xscale scale type for x axis (eg. 'continuous', 'discrete', 'log10').
#' @param yscale scale type for y axis (eg. 'continuous', 'discrete', 'log10').
#' @param title Plot title. Use \code{NULL} to remove.
#' @param subtitle Plot subtitle. Use \code{NULL} to remove.
#' @param caption Page caption. Use \code{NULL} to remove.
#' @param plot_name name that will be used by \code{xpose_save()} to save the plot.
#' @param xp_theme An xpose theme or vector of modifications to the xpose theme
#' (eg. \code{c(point_color = 'red', line_linetype = 'dashed')}).
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' @param problem Numeric, the $problem number to use for ploting. By default the data 
#' is taken from the estimation problem.
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
#' @examples
#' xplot_scatter(xpdb_ex_pk, aes(x = IPRED, y = DV))
#' 
#' @export
xplot_scatter <- function(xpdb,
                          vars      = NULL,
                          aes       = NULL,
                          group     = 'ID',
                          type      = 'pls',
                          layers    = NULL,
                          guides    = TRUE,
                          xscale    = 'continuous',
                          yscale    = 'continuous',
                          title     = NULL,
                          subtitle  = NULL,
                          caption   = NULL,
                          plot_name = 'scatter_plot',
                          xp_theme,
                          quiet,
                          problem,
                          ...) {
  
  # Check input
  if (!is.xpdb(xpdb)) { 
    msg('Bad input to the argument`xpdb`', 
        dplyr::if_else(missing(quiet), TRUE, quiet))
    return()
  }
  
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  # Get data
  if (missing(problem)) problem <- max(xpdb$data$problem[!xpdb$data$simtab])
  data   <- get_data(xpdb, problem = problem)
  msg(c('Using data from $problem no.', problem), quiet)
  
  # Filter observations
  if ('MDV' %in% colnames(data)) {
    data <- dplyr::filter(data, data$MDV == 0)
  } else if ('EVID' %in% colnames(data)) {
    data <- dplyr::filter(data, data$EVID == 0)
  }
  
  # Update xp_theme if needed
  if (!missing(xp_theme)) xpdb <- update_themes(xpdb = xpdb, xp_theme = xp_theme)
  
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
    if (!is.formula(list(...)[['panel_facets']])) {
      xp <- xp + xp_geoms(mapping  = aes,
                          xp_theme = xpdb$xp_theme,
                          name     = 'panel',
                          ggfun    = 'facet_wrap',
                          ...)
    } else {
      tmp_xtheme <- xpdb$xp_theme[which(!names(xpdb$xp_theme) %in%
                                          stringr::str_c('panel_', 
                                                         c('ncol', 'nrow', 'dir'), 
                                                         sep = ''))]
      xp <- xp + xp_geoms(mapping  = aes,
                          xp_theme = tmp_xtheme,
                          name     = 'panel',
                          ggfun    = 'facet_grid',
                          ...)
    }
  }
  
  # Add labels
  xp <- xp + labs(title = title, subtitle = subtitle, caption = caption)
  
  # Add users defined layers
  if (!is.null(layers)) { xp <- xp + layers }
  
  # Add metadata to plots
  xp$xpose <- list(fun     = plot_name,
                   summary = xpdb$summary,
                   problem = problem,
                   quiet   = quiet)
  
  structure(xp, class = c('xpose_plot', class(xp)))
}
