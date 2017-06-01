#' List available variables
#'
#' @description Function listing all available variables in an xpdb object.
#' 
#' @param xpdb An \code{xpose_data} object from which the model code will be extracted.
#' @param problem The problem to be used, by lists all available problems.
#'
#' @examples
#' list_vars(xpdb_ex_pk)
#' @export
list_vars <- function(xpdb, problem = NULL) {
  x <- xpdb$data[xpdb$data$problem %in% problem, ] %>% 
    dplyr::mutate(grouping = as.integer(.$problem)) %>% 
    dplyr::group_by_(.dots = 'grouping') %>% 
    tidyr::nest() %>% 
    {purrr::map(.$data, function(df) {
      cat('\nList of available variables for problem no.', df$problem[1], '\n')
      df$index[[1]] %>% 
        dplyr::group_by_(.dots = 'type') %>% 
        tidyr::nest() %>% 
        dplyr::mutate(string = purrr::map_chr(.$data, ~stringr::str_c(unique(.$col), collapse = ', ')),
                      descr = dplyr::case_when(.$type == 'id' ~ 'Subject identifier (id)',
                                               .$type == 'occ' ~ 'Occasion flag (occ)',
                                               .$type == 'na' ~ 'Unknown (na)',
                                               .$type == 'amt' ~ 'Dose amount (amt)',
                                               .$type == 'idv' ~ 'Independent variable (idv)',
                                               .$type == 'pred' ~ 'Model predictions (pred)',
                                               .$type == 'res' ~ 'Residuals (res)',
                                               .$type == 'evid' ~ 'Event identifier (evid)',
                                               .$type == 'dv' ~ 'Dependent variable (dv)',
                                               .$type == 'catcov' ~ 'Categorical covariates (catcov)',
                                               .$type == 'contcov' ~ 'Continuous covariates (contcov)',
                                               .$type == 'param' ~ 'Model parameter (param)',
                                               .$type == 'eta' ~ 'Eta (eta)',
                                               .$type == 'eps' ~ 'Epsilon (eps)',
                                               .$type == 'dvid' ~ 'DV identifier (dvid)',
                                               .$type == 'mdv' ~ 'Missing dependent variable (mdv)')) %>% 
        dplyr::mutate(descr = stringr::str_pad(.$descr, 33, 'right')) %>% 
        {stringr::str_c(' -', .$descr, ':', .$string, sep = ' ')} %>% 
        cat(sep = '\n')})}
}
