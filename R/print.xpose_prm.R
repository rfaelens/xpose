#' Print an xpose_prm object
#' 
#' @description This function returns to the console a list of the parameter estimates.
#' 
#' @param x An \code{xpose_prm} object generated with \code{\link{get_prm}}.
#' @param ... Ignored in this function
#' 
#' @method print xpose_prm
#' @examples 
#' # Create the xpose_prm object
#' prm <- get_prm(xpdb_ex_pk)
#' 
#' # Output using the print function
#' print(prm)
#' 
#' # Or simply by writting the xpdb name
#' prm
#' 
#' @export
print.xpose_prm <- function(x, ...) {
  
  ###### TEMP
  problem = 1
  ######
  
  header <- x[1, c('name','label','value','rse', 'fixed')]
  header[1,] <- c('Parameter', 'Label', 'Value', 'RSE', '   ')
  
  cat('\nThe relative standard errors for omega and sigma are reported on the approximate
standard deviation scale (SE/variance estimate)/2.\n')
  
  cat('\nParameter estimates for problem no.', problem, '\n')
  
  x %>% 
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
}
