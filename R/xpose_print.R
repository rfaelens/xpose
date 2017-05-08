#' Print an xpdb object
#' 
#' @description This function will return a summary of the xpdb content to the console.
#' 
#' @param xpdb An \code{xpose_data} object generated with \code{\link{xpose_data}}.
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
print.xpose_data <- function(xpdb) {
  if (!is.null(xpdb$data)) {
    tab_names <- xpdb$data %>% 
      purrr::by_row(summarize_table_names, .to = 'string') %>%
      {purrr::flatten_chr(.$string)} %>% 
      stringr::str_c(collapse = '\n             ')
  } else {
    tab_names <- '<none>'
  }
  
  if (!is.null(xpdb$sim)) {
    sim_names <- xpdb$sim %>% 
      purrr::by_row(summarize_table_names, .to = 'string') %>%
      {purrr::flatten_chr(.$string)} %>% 
      stringr::str_c(collapse = '\n                 ')
  } else {
    sim_names <- '<none>'
  }
  
  if (!is.null(xpdb$files)) {
    out_names <- unique(xpdb$files$name) %>% 
      sort() %>% 
      stringr::str_c(collapse = ', ')
  } else {
    out_names <- '<none>'
  }
  
  cat(xpdb$summary$file, 'overview:',
      '\n - Software:', xpdb$summary$software, xpdb$summary$version,
      '\n - Attached files:', 
      '\n   + tables:', tab_names,
      '\n   + sim tables:', sim_names,
      '\n   + output files:', out_names,
      '\n - gg_theme:', attr(xpdb$gg_theme, 'theme'),
      '\n - xp_theme:', attr(xpdb$xp_theme, 'theme'),
      '\n - Options:', paste(names(xpdb$options), xpdb$options, sep = ' = ', collapse = ', '))
}

summarize_table_names <- function(dat) {
  purrr::map(dat$index, ~.$tables) %>% 
    purrr::flatten_chr() %>% 
    stringr::str_c(collapse = ', ') %>% 
    stringr::str_c('$prob no.',dat$problem,': ', ., sep = '')
}
