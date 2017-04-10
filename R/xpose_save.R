#' Save ggxpose plot
#'
#' @description Automatically save ggxpose plots to pdf, jpg or png.
#'
#' If no arguments are provided, the function will automatically name
#' and save a plot after its run number and the plotting function name
#' by using the metadata attached to the plot.
#'
#' @param plot a ggxpose plot object.
#' @param filename an optional name to be given to the file. Template variables such as @run 
#' can be used to generate template names. By default, the file extension will be set to .pdf 
#' by default but can be changed to .jpeg, .png, .bmp, .tiff
#' @param dir an optional path to a specific directory.
#' @param width the page width in inches
#' @param height the page height in inches
#' @param res the nominal resolution in ppi. Not used with .pdf
#' @param ... additional options to be passed to ploting functions.
#'
#' @examples
#' \dontrun{
#' xpdb %>% 
#'  dv_vs_ipred() %>% 
#'  xpose_save()
#' }
#' @export
xpose_save <- function(plot     = last_plot(),
                       filename = '@run_@plotfun.pdf',
                       dir      = NULL,
                       width    = 8,
                       height   = 7,
                       res      = 200,
                       ...) {
  
  if (is.null(plot)) {
    stop('Argument \"plot\" required.', call. = FALSE)
  }
  
  if (!is.null(dir) && !substr(dir, nchar(dir), nchar(dir)) == '/') {
    dir <- paste0(dir, '/')
  }
  
  filename <- parse_title(string = filename, xpdb = plot$xpose,
                          extra_key = '@plotfun', extra_value = plot$xpose$fun)
  
  format <- unlist(regmatches(filename, gregexpr('\\.\\w+$', filename)))
  
  if (length(format) == 0) {
    format <- '.pdf'
    filename <- paste0(filename, format)
  }
  
  switch(format,
         '.pdf' = grDevices::pdf(file = paste0(dir, filename),
                                 width = width, height = height, ...),
         '.jpeg' = grDevices::jpeg(filename = paste0(dir, filename),
                                   width = width, height = height, units = 'in',
                                   res = res, ...),
         '.png' = grDevices::png(filename = paste0(dir, filename),
                                 width = width, height = height, units = 'in', 
                                 res = res, ...),
         '.bmp' = grDevices::bmp(filename = paste0(dir, filename),
                                 width = width, height = height, units = 'in', 
                                 res = res, ...),
         '.tiff' = grDevices::tiff(filename = paste0(dir, filename),
                                   width = width, height = height, units = 'in', 
                                   res = res, ...),
         stop('unknown format provided in \"filename\"', call. = FALSE)
  )
  
  print(plot)
  grDevices::dev.off()
}
