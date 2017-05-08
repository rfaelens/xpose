#' Print an xpose_data object
#' 
#' @description This function returns a summary of an xpose_data to the console.
#' 
#' @param x An \code{xpose_data} object generated with \code{\link{xpose_data}}.
#' @param ... Ignored in this function
#' 
#' @method print xpose_data
#' @examples 
#' # Using the print function
#' print(xpdb_ex_pk)
#' 
#' # Or simply by writting the xpdb name
#' xpdb_ex_pk
#' 
#' @export
print.xpose_data <- function(x, ...) {
  if (!is.null(x$data)) {
    tab_names <- x$data %>% 
      purrr::by_row(summarize_table_names, .to = 'string') %>%
      {purrr::flatten_chr(.$string)} %>% 
      stringr::str_c(collapse = '\n             ')
  } else {
    tab_names <- '<none>'
  }
  
  if (!is.null(x$sim)) {
    sim_names <- x$sim %>% 
      purrr::by_row(summarize_table_names, .to = 'string') %>%
      {purrr::flatten_chr(.$string)} %>% 
      stringr::str_c(collapse = '\n                 ')
  } else {
    sim_names <- '<none>'
  }
  
  if (!is.null(x$files)) {
    out_names <- unique(x$files$name) %>% 
      sort() %>% 
      stringr::str_c(collapse = ', ')
  } else {
    out_names <- '<none>'
  }
  
  cat(x$summary$file, 'overview:',
      '\n - Software:', x$summary$software, x$summary$version,
      '\n - Attached files:', 
      '\n   + tables:', tab_names,
      '\n   + sim tables:', sim_names,
      '\n   + output files:', out_names,
      '\n - gg_theme:', attr(x$gg_theme, 'theme'),
      '\n - xp_theme:', attr(x$xp_theme, 'theme'),
      '\n - Options:', paste(names(x$options), x$options, sep = ' = ', collapse = ', '))
}

summarize_table_names <- function(dat) {
  purrr::map(dat$index, ~.$tables) %>% 
    purrr::flatten_chr() %>% 
    stringr::str_c(collapse = ', ') %>% 
    stringr::str_c('$prob no.',dat$problem,': ', ., sep = '')
}
