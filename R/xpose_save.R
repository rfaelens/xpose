#' Save ggxpose plot
#'
#' @description Automatically save ggxpose plots to pdf, jpg or png.
#'
#' If no arguments are provided, the function will automatically name
#' and save a plot after its run number and the plotting function name
#' by using the metadata attached to the plot.
#'
#' @param plot a ggxpose plot object.
#' @param dir an optional path to a specific directory.
#' @param name an optional name to be given to the file. If left NULL the run
#' number and plotting function will be used instead.
#' @param format the file format to use: 'pdg', 'jpg' or 'png',
#' @param width the page width in inches
#' @param height the page height in inches
#' @param ... additional options to be passed to \code{jpg()} or \code{png()} functions.
#'
#' @examples
#' \dontrun{
#' dv_vs_ipred(xpdb) %>% xpose_save()
#' }
#' @export
xpose_save <- function(plot   = NULL,
                       dir    = NULL,
                       name   = NULL,
                       format = 'pdf',
                       width  = 12,
                       height = 9,
                       ...) {

  if (is.null(plot)) {
    stop('Argument \"plot\" required.')
  }

  if (!is.null(dir) && !substr(dir, nchar(dir), nchar(dir)) == '/') {
    dir <- paste0(dir, '/')
  }

  if (is.null(name)) {
    name <- paste(plot$xpose$modfile, plot$xpose$fun, sep = '_')
  }

  switch(format,
         'pdf' = pdf(file = paste0(dir, name,'.pdf'),
                     width = width, height = height),
         'jpg' = jpeg(filename = paste0(dir, name,'.jpg'),
                      width = width, height = height, units = 'in',...),
         'png' = png(filename = paste0(dir, name,'.png'),
                     width = width, height = height, units = 'in',...)
         )

  print(plot)
  dev.off()
}
