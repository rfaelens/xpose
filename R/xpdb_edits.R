#' Filter an xpdb by matching condition
#' 
#' @description Use filter() find rows/cases where conditions are true. 
#' Unlike base subsetting, rows where the condition evaluates to NA are dropped.
#' 
#' @param .data An xpose database object.
#' @param .problem The problem from which the data will be modified
#' @param .source The source of the data in the xpdb. Can be data or an output 
#' file extension.
#' @param ... Logical predicates defined in terms of the variables in .data. 
#' Multiple conditions are combined with &. Only rows where the conditon evalutes 
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
  xpdb <- .data # To avoid issues with dplyr arguments
  if (missing(.source)) .source <- 'data'
  if (.source == 'data') {
    if (missing(.problem)) .problem <- xpdb[['data']]$problem
    if (!all(.problem %in% xpdb[['data']]$problem)) {
      stop('Problem no.', stringr::str_c(.problem[!.problem %in% xpdb[['data']]$problem], collapse = ', '), 
           ' not found in model output data.', call. = FALSE)
    }
    xpdb[['data']] <- xpdb[['data']] %>%
      dplyr::mutate(data = purrr::map_if(.$data, xpdb[['data']]$problem %in% .problem,
                                         ~dplyr::filter(., !!dplyr::quo(...))))
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
                                         ~dplyr::filter(., !!dplyr::quo(...))))
  }
  xpdb
}
