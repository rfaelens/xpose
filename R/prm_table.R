#' Display a parameter estimates to the console
#' 
#' @description Display parameter estimates from an xpdb object to the console.
#' 
#' @inheritParams get_prm
#' @examples
#' # Display parameters to the console
#' prm_table(xpdb_ex_pk, .problem = 1)
#' 
#' @export
prm_table <- function(xpdb,
                      .problem = NULL, 
                      .subprob = NULL, 
                      .method  = NULL,
                      digits   = 4,
                      show_all = FALSE) {
  
  x <- get_prm(xpdb = xpdb, .problem = .problem, .subprob = .subprob, 
               .method = .method, digits = digits, show_all = show_all, quiet = TRUE)
  
  cat('\nThe relative standard errors for omega and sigma are reported on the approximate
standard deviation scale (SE/variance estimate)/2.\n')
  
  # Convert single prm_df to list
  if (dplyr::is.tbl(x)) x <- list(x)
  
  # Generate output to console
  purrr::map(.x = x, function(prm) {
    prm_attr <- attributes(prm)
    header <- dplyr::data_frame(name = 'Parameter', label = 'Label', 
                                value = 'Value', rse = 'RSE', fixed = '   ')
    
    cat('\nEstimates for $prob no.', prm_attr$problem, 
        ', subprob no.', prm_attr$subprob, ', method ', prm_attr$method, '\n', sep = '')
    
    prm %>% 
      dplyr::mutate_all(.funs = 'as.character') %>% 
      dplyr::mutate(fixed = ifelse(.$fixed, 'fix', '   ')) %>% 
      {dplyr::bind_rows(header, .)} %>% 
      dplyr::mutate(name  = stringr::str_pad(.$name, max(nchar(.$name)), 'right'),
                    label = stringr::str_pad(.$label, max(nchar(.$label)), 'right'),
                    value = stringr::str_pad(.$value, max(nchar(.$value)), 'right'),
                    rse   = ifelse(is.na(.$rse), ' - ', .$rse)) %>%
      dplyr::mutate(string = stringr::str_c('', .$name, .$label, .$value, .$fixed, .$rse, sep = ' ')) %>% 
      {purrr::flatten_chr(.[,'string'])} %>% 
      cat(sep = '\n')
  }) %>% 
    invisible()
}
