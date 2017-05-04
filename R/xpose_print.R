#' Print an xpdb object
#' 
#' @description This function will return a summary of the xpdb content to the console.
#' 
#' @param xpdb An \code{xpose_data} object generated with \code{\link{xpose_data}}.
#' 
#' @method print xpose_data
#' @export
print.xpose_data <- function(xpdb) {
  cat('xpose_data summary for', xpdb$summary$file , 'ran with', xpdb$summary$software, ':',
      '\n - Code:', xpdb$summary$file, ', ' , length(unique(xpdb$code$problem)), '$problems',
      '\n - data: ', paste0(purrr::map(xpdb$data$index, ~.$tables), collpase = ', '))
  
  # code = model_code, 
  #summary = summary, cor = NULL,
  # cov = NULL, ext = NULL, prm = NULL, phi = NULL,
  # grd = NULL, data = data, sim = sim, gg_theme = gg_theme,
  # xp_theme = xp_theme, options = list(quiet = quiet)
}
