#' Draw an xpose_plot object
#' 
#' @description This function explicitly draw an xpose_plot and interprets keywords 
#' contained in labels.
#' 
#' @param x An \code{xpose_plot} object.
#' @param ... Options to be passed on to the ggplot2 print method.
#' 
#' @method print xpose_plot
#' @examples
#' my_plot <- dv_vs_ipred(xpdb_ex_pk) +
#'             labs(title = 'A label with keywords @nind, @nobs')

#' # Using the print function
#' print(my_plot)
#' 
#' # Or simply by writting the plot object name
#' my_plot
#' 
#' @export
print.xpose_plot <- function(x, ...) {
  if (is.xpose.plot(x)) {
    x$labels$title <- append_suffix(x$xpose, x$labels$title, 'title')
    x$labels$subtitle <- append_suffix(x$xpose, x$labels$subtitle, 'subtitle')
    x$labels$caption  <- append_suffix(x$xpose, x$labels$caption, 'caption')
    x$labels <- x$labels %>% 
      purrr::map_if(stringr::str_detect(., '@'),
                    ~parse_title(string = ., xpdb = x$xpose,
                                 problem = x$xpose$problem, quiet = x$xpose$quiet))
  }
  print.ggplot(x, ...)
}

print.ggplot <- get('print.ggplot', envir = asNamespace('ggplot2'))
