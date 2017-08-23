#' Save xpose plot
#'
#' @description Automatically save xpose plots to files.
#'
#' This function was inspired by the \link[ggplot2]{ggsave}. If no arguments are provided, 
#' the function will automatically name and save a plot after its run number and the plotting 
#' function name by using the metadata attached to the plot.
#'
#' @param plot A xpose plot object.
#' @param file An optional name to be given to the file. Template variables such as @run 
#' can be used to generate template names.
#' @param dir Directory under which the xpose plot will be saved. Template variables such as @dir 
#' can be used to generate template names.
#' @param device Graphical device to use. Can be either be a device function
#'   (e.g. \code{\link{png}}), or one of 'eps', 'ps', 'tex' (pictex),
#'   'pdf' (default), 'jpeg', 'tiff', 'png', 'bmp', 'svg' or 'wmf' (windows only).
#' @param width,height Plot size in \code{units}.
#' @param units Units of the plot size ('in', 'cm', or 'mm').
#' @param dpi Plot resolution. Applies only to raster output types.
#' @param ... Additional arguments passed on to \link[ggplot2]{ggsave} or graphics \code{device}.
#'
#' @examples
#' \dontrun{
#' xpdb_ex_pk %>% 
#'  dv_vs_ipred() %>% 
#'  xpose_save()
#' }
#' @export
xpose_save <- function(plot     = last_plot(),
                       file     = '@run_@plotfun.pdf',
                       dir      = NULL,
                       device   = NULL,
                       width    = 7,
                       height   = 6,
                       units    = c('in', 'cm', 'mm'),
                       dpi      = 200,
                       ...) {
  
  if (is.null(plot)) {
    stop('The `plot` argument is NULL.')
  } else if (is.null(file)) {
    stop('The `file` argument is NULL.')
  }
  
  # Parse the dir and file arguments for keywords
  if (!is.null(dir)) {
    dir <- parse_title(string = dir, xpdb = plot$xpose,
                       problem = plot$xpose$problem, quiet = plot$xpose$quiet,
                       extra_key = 'plotfun', extra_value = plot$xpose$fun)
  }
  
  file <- parse_title(string = file, xpdb = plot$xpose,
                      problem = plot$xpose$problem, quiet = plot$xpose$quiet,
                      extra_key = 'plotfun', extra_value = plot$xpose$fun)
  
  
  # Add device to file
  if (!is.null(device) && !is.function(device) && !grepl('\\.[[:alnum:]]+$', file)) {
    file <- paste0(file, '.', device)
  }
  
  # Call ggsave
  ggsave(plot = plot, filename = file, path = dir, device = device, 
         width = width, height = height, units = units, dpi = dpi, ...)
}
