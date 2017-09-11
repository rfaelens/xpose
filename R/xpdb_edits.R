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
  xpdb <- .data # Avoids issues with dplyr arguments
  if (missing(.source)) .source <- 'data'
  if (length(.source) > 1) stop('Argument `.source` should be of length 1.', call. = FALSE)
  check_xpdb(xpdb, check = .source)
  
  # Direct filter to specified source
  if (.source == 'data') {
    if (missing(.problem)) .problem <- all_data_problem(xpdb)
    if (!all(.problem %in% all_data_problem(xpdb))) {
      stop('Problem no.', stringr::str_c(.problem[!.problem %in% xpdb[['data']]$problem], collapse = ', '), 
           ' not found in model output data.', call. = FALSE)
    }
    
    check_quo_vars(xpdb, ..., .source = .source, .problem = .problem)
    
    xpdb[['data']] <- xpdb[['data']] %>%
      dplyr::mutate(data = purrr::map_if(.$data, xpdb[['data']]$problem %in% .problem,
                                         .f = dplyr::filter, rlang::UQS(rlang::quos(...))),
                    modified = dplyr::if_else(.$problem %in% .problem, TRUE, .$modified))
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
    
    check_quo_vars(xpdb, ..., .source = .source, .problem = .problem)
    
    xpdb[['files']] <- xpdb[['files']] %>%
      dplyr::mutate(data = purrr::map_if(.$data, xpdb[['files']]$problem %in% .problem &
                                           xpdb[['files']]$extension %in% .source,
                                         .f = dplyr::filter, rlang::UQS(rlang::quos(...))),
                    modified = dplyr::if_else(.$problem %in% .problem & .$extension %in% .source, TRUE, .$modified))
  }
  xpdb
}


#' Add new variables to an xpdb
#' 
#' @description \code{mutate()} adds new variables and preserves existing ones.
#' 
#' @param .data An xpose database object.
#' @param .problem The problem from which the data will be modified
#' @param .source The source of the data in the xpdb. Can either be 'data' or an output 
#' file extension e.g. 'phi'.
#' @param ... Name-value pairs of expressions. Use \code{NULL} to drop a variable.
#' 
#' These arguments are automatically quoted and evaluated in the 
#' context of the data frame. They support unquoting and splicing. 
#' See the dplyr vignette("programming") for an introduction to these concepts.
#' @method mutate xpose_data
#' @examples
#' xpdb_ex_pk %>% 
#'  mutate(TAD2 = TIME %% 40, .problem = 1) %>% 
#'  dv_vs_idv(aes(x = TAD2))
#' @export
mutate.xpose_data <- function(.data, ..., .problem, .source) {
  
  # Check input
  xpdb <- .data # Avoids issues with dplyr arguments
  if (missing(.source)) .source <- 'data'
  if (length(.source) > 1) stop('Argument `.source` should be of length 1.', call. = FALSE)
  check_xpdb(xpdb, check = .source)
  
  # Direct filter to specified source
  if (.source == 'data') {
    if (missing(.problem)) .problem <- all_data_problem(xpdb)
    if (!all(.problem %in% all_data_problem(xpdb))) {
      stop('Problem no.', stringr::str_c(.problem[!.problem %in% xpdb[['data']]$problem], collapse = ', '), 
           ' not found in model output data.', call. = FALSE)
    }
    
    check_quo_vars(xpdb, ..., .source = .source, .problem = .problem)
    
    xpdb[['data']] <- xpdb[['data']] %>%
      dplyr::mutate(data = purrr::map_if(.$data, xpdb[['data']]$problem %in% .problem,
                                         .f = dplyr::mutate, rlang::UQS(rlang::quos(...))),
                    modified = dplyr::if_else(.$problem %in% .problem, TRUE, .$modified))
    
    # Update index
    xpdb[['data']] <- xpdb[['data']] %>% 
      dplyr::group_by_(.dots = 'problem') %>% 
      tidyr::nest(.key = 'tmp') %>% 
      dplyr::mutate(tmp = purrr::map_if(.$tmp, 
                                        xpdb[['data']]$problem %in% .problem,
                                        function(x) {
                                          col_names <- colnames(x$data[[1]])
                                          # Drop columns not present in data anymore
                                          x$index[[1]] <- x$index[[1]] %>% 
                                            dplyr::filter(.$col %in% col_names)
                                          
                                          # Add new columns found in data
                                          add_cols <- col_names[!col_names %in% x$index[[1]]$col]
                                          if (length(add_cols) > 0) {
                                            x$index[[1]] <- x$index[[1]] %>%   
                                              dplyr::bind_rows(
                                                dplyr::tibble(table = 'na', col = add_cols, type = 'na', 
                                                              label = NA_character_, 
                                                              units = NA_character_))
                                          }
                                          x
                                        })) %>% 
      tidyr::unnest_(unnest_cols = 'tmp')
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
    
    check_quo_vars(xpdb, ..., .source = .source, .problem = .problem)
    
    xpdb[['files']] <- xpdb[['files']] %>%
      dplyr::mutate(data = purrr::map_if(.$data, xpdb[['files']]$problem %in% .problem &
                                           xpdb[['files']]$extension %in% .source,
                                         .f = dplyr::mutate, rlang::UQS(rlang::quos(...))),
                    modified = dplyr::if_else(.$problem %in% .problem & .$extension %in% .source, TRUE, .$modified))
  }
  xpdb
}


#' Check quoted variables
#' 
#' @description Ensure that variables used in quos are present in the 
#' data and return informative error messages otherwise.
#' 
#' @param xpdb An xpose database object.
#' @param ... Name-value pairs of expressions.
#' @param .problem The problem from which the data will be modified
#' @param .source The source of the data in the xpdb. Can either be 'data' or an output 
#' file extension e.g. 'phi'.
#' 
#' @return Silent if checks are successful, returns errors otherwise.
#' 
# @keywords internal
# @export
check_quo_vars <- function(xpdb, ..., .source, .problem) {
  quo_vars <- rlang::quos(...) %>% 
    purrr::map(all.vars) %>% 
    purrr::flatten_chr()
  
  if (length(quo_vars) > 0) {
    if (.source == 'data') {
      tmp <- xpdb$data[xpdb$data$problem %in% .problem, ] 
    } else {
      tmp <- xpdb$files[xpdb$files$extension %in% .source & xpdb$files$problem %in% .problem, ]
    }
    
    tmp <- dplyr::mutate(.data = tmp,
                         missing = purrr::map(tmp$data, ~stringr::str_c(quo_vars[!quo_vars %in% colnames(.)], collapse = ', ')),
                         error = purrr::map_lgl(tmp$data, ~any(!quo_vars %in% colnames(.))))
    
    if (any(tmp$error)) {
      tmp %>% 
        dplyr::filter(.$error) %>% 
        dplyr::mutate(string = stringr::str_c('missing: ', .$missing, ' variables in ', 
                                              ifelse(.source == 'data', '', stringr::str_c('`', .source, '` file ')), 
                                              '$prob no.', .$problem, '.')) %>% 
                                              {stop(stringr::str_c(.$string, collapse = '\n       '), call. = FALSE)}
    }
  }
}
