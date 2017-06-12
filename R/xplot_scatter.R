#' Default xpose scatter plot function
#'
#' @description Manually generate scatter plots from an xpdb object.
#'
#' @param xpdb An \code{xpose_data} object generated with \code{\link{xpose_data}}.
#' @param mapping List of aesthetics mappings to be used for the xpose plot 
#' (e.g. \code{point_color}).
#' @param group Grouping variable to be used for lines.
#' @param type String setting the type of plot to be used points 'p',
#' line 'l', smooth 's' and text 't' or any combination of the four.
#' @param guides should the guides (eg. unity line) be displayed.
#' @param xscale scale type for x axis (eg. 'continuous', 'discrete', 'log10').
#' @param yscale scale type for y axis (eg. 'continuous', 'discrete', 'log10').
#' @param title Plot title. Use \code{NULL} to remove.
#' @param subtitle Plot subtitle. Use \code{NULL} to remove.
#' @param caption Page caption. Use \code{NULL} to remove.
#' @param plot_name name that will be used by \code{xpose_save()} to save the plot.
#' @param gg_theme A ggplot2 theme object (eg. \code{\link[ggplot2]{theme_classic}}).
#' @param xp_theme An xpose theme or vector of modifications to the xpose theme
#' (eg. \code{c(point_color = 'red', line_linetype = 'dashed')}).
#' @param data_opt A list of options in order to create appropriate data input for 
#' ggplot2. For more information see \code{\link{data_opt_set}}.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' @param ... any additional aesthetics.
#' 
#' @section Layers mapping:
#' Plots can be customized by mapping arguments to specific layers. The naming convention is 
#' layer_option where layer is one of the names defined in the list below and option is 
#' any option supported by this layer e.g. point_color = 'blue', smooth_method = 'lm', etc.
#' \itemize{
#'   \item point: options to \code{geom_point}
#'   \item line: options to \code{geom_line}
#'   \item guide: options to \code{geom_abline}
#'   \item panel: options to \code{facet_wrap} (facets is character) or \code{facet_grid} 
#'   (facets is a formula)
#'   \item smooth: options to \code{geom_smooth}
#'   \item text: options to \code{geom_text}
#'   \item xscale: options to \code{scale_x_continuous} or \code{scale_x_log10}
#'   \item yscale: options to \code{scale_y_continuous} or \code{scale_y_log10}
#' }
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
                          mapping   = NULL,
                          group     = 'ID',
                          type      = 'pls',
                          guides    = FALSE,
                          xscale    = 'continuous',
                          yscale    = 'continuous',
                          title     = NULL,
                          subtitle  = NULL,
                          caption   = NULL,
                          plot_name = 'scatter_plot',
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
  if (is.null(data)) return()
  
  # Assing xp_theme and gg_theme
  if (!missing(xp_theme)) xpdb <- update_themes(xpdb = xpdb, xp_theme = xp_theme)
  if (missing(gg_theme)) gg_theme <- xpdb$gg_theme
  
  # Create ggplot base
  xp <- ggplot(data = data, mapping, ...) + gg_theme 
  
  # Add unity line
  if (guides) {
    xp <- xp + xp_geoms(xp_theme = xpdb$xp_theme,
                        name     = 'guides',
                        ggfun    = 'geom_abline',
                        ...)
  }
  
  # Add lines
  if (stringr::str_detect(type, stringr::fixed('l', ignore_case = TRUE))) {
    xp <- xp + xp_geoms(mapping  = c(mapping, aes_string(line_group = group)),
                        xp_theme = xpdb$xp_theme,
                        group    = group,
                        name     = 'line',
                        ggfun    = 'geom_line',
                        ...)
  }
  
  # Add points
  if (stringr::str_detect(type, stringr::fixed('p', ignore_case = TRUE))) {
    xp <- xp + xp_geoms(mapping  = mapping,
                        xp_theme = xpdb$xp_theme,
                        name     = 'point',
                        ggfun    = 'geom_point',
                        ...)
  }
  
  # Add text
  if (stringr::str_detect(type, stringr::fixed('t', ignore_case = TRUE))) {
    xp <- xp + xp_geoms(mapping  = c(mapping, aes_string(text_label = xp_var(xpdb, attr(data, 'problem'), 
                                                                         type = 'id')$col)),
                        xp_theme = xpdb$xp_theme,
                        name     = 'text',
                        ggfun    = 'geom_text',
                        ...)
  }
  
  # Add smooth
  if (stringr::str_detect(type, stringr::fixed('s', ignore_case = TRUE))) {
    xp <- xp + xp_geoms(mapping  = mapping,
                        xp_theme = xpdb$xp_theme,
                        name     = 'smooth',
                        ggfun    = 'geom_smooth',
                        ...)
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
                          xp_theme = filter_xp_theme(xpdb$xp_theme, stringr::str_c('panel_', c('ncol', 'nrow', 'dir'), 
                                                                                   collapse = '|'), 'drop'),
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
