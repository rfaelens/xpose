#' Filter an xpdb by matching condition
#' 
#' @description Use filter() find rows/cases where conditions are true. 
#' Unlike base subsetting, rows where the condition evaluates to NA are dropped.
#' 
#' @param .data An xpose database object.
#' @param .problem The problem from which the data will be modified
#' @param .source The source of the data in the xpdb. Can either be 'data' or an output 
#' file extension e.g. 'phi'.
#' @param ... Logical predicates defined in terms of the variables in .data. 
#' Multiple conditions are combined with &. Only rows where the condition evaluates 
#' to \code{TRUE} are kept. 
#' These arguments are automatically quoted and evaluated in the 
#' context of the data frame. They support unquoting and splicing. 
#' See the dplyr vignette("programming") for an introduction to these concepts.
#' @method filter xpose_data
#' @examples
#' xpdb_ex_pk %>% 
#'  filter(DV > -2, .problem = 1) %>% 
#'   dv_vs_ipred()
#' @export
filter.xpose_data <- function(.data, ..., .problem, .source) {
  # Check input
  xpdb <- .data # To avoid issues with dplyr arguments
  if (missing(.source)) .source <- 'data'
  check_xpdb(xpdb, check = .source)
  
  # Direct filter to specified source
  if (.source == 'data') {
    if (missing(.problem)) .problem <- all_data_problem(xpdb)
    if (!all(.problem %in% all_data_problem(xpdb))) {
      stop('Problem no.', stringr::str_c(.problem[!.problem %in% xpdb[['data']]$problem], collapse = ', '), 
           ' not found in model output data.', call. = FALSE)
    }
    
    xpdb[['data']] <- xpdb[['data']] %>%
      dplyr::mutate(data = purrr::map_if(.$data, xpdb[['data']]$problem %in% .problem,
                                         ~dplyr::filter(., rlang::UQS(rlang::quos(...)))))
  } else {
    if (missing(.problem)) .problem <- xpdb[['files']]$problem
    if (!all(.source %in% xpdb[['files']]$extension)) {
      stop('File extension ', stringr::str_c(.source[!.source %in% xpdb[['files']]$extension], collapse = ', '), 
           ' not found in model output files.', call. = FALSE)
    }
    
    if (!all(.problem %in% xpdb[['files']]$problem[xpdb[['files']]$extension %in% .source])) {
      stop('Problem no.', stringr::str_c(.problem[!.problem %in% xpdb[['files']]$problem], collapse = ', '), 
           ' not found in model output files.', call. = FALSE)
    }
    
    xpdb[['files']] <- xpdb[['files']] %>%
      dplyr::mutate(data = purrr::map_if(.$data, xpdb[['files']]$problem %in% .problem &
                                           xpdb[['files']]$extension %in% .source,
                                         ~dplyr::filter(., rlang::UQS(rlang::quos(...)))))
  }
  xpdb
}
