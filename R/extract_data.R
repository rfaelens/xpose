#' Extract model code
#'
#' @description Extract model code from an xpdb object.
#' 
#' @param xpdb An \code{xpose_data} object from which the model code will be extracted.
#' @param problem The problem to be used, in addition, problem 0 is attributed to 
#' general output (e.g. NM-TRAN warnings in NONMEM). By default returns the 
#' entire code.
#' @return A tibble of the parsed model.
#' @seealso \code{\link{xpose_data}}, \code{\link{read_nm_model}}
#' @examples
#' parsed_model <- get_code(xpdb_ex_pk)
#' parsed_model
#' 
#' @export
get_code <- function(xpdb, problem = NULL) {
  if (!is.xpdb(xpdb)) {
    stop('Valid `xpdb` input required.', call. = FALSE)
  }
  
  x <- xpdb$code
  
  if (!is.null(problem)) {
    if (!all(problem %in% x$problem)) {
      stop('Problem no.', stringr::str_c(problem[!problem %in% x$problem], collapse = ', '), 
           ' not found in model code.', call. = FALSE)
    }
    x <- x[x$problem %in% problem, ]
  }
  x
}


#' Extract model output table data
#'
#' @description Extract model output table data from an xpdb object.
#' 
#' @param xpdb An \code{xpose_data} object from which the model output file data will be extracted.
#' @param table Name of the output table to be extracted from the xpdb e.g. 'sdtab001'. Alternative to 
#' the "problem" argument.
#' @param problem Extracts all tables from the specified problem. Alternative to the "table" argument.
#' 
#' @return A tibble for single file or a named list for multiple files.
#' @seealso \code{\link{xpose_data}}, \code{\link{read_nm_tables}}
#' @examples
#' # By table name
#' sdtab <- get_data(xpdb_ex_pk, 'sdtab001')
#' sdtab
#' 
#' # By problem
#' tables <- get_data(xpdb_ex_pk, problem = 1)
#' tables
#' 
#' # Tip to list available tables in the xpdb
#' print(xpdb_ex_pk)
#' 
#' @export
get_data <- function(xpdb, table = NULL, problem = NULL) {
  if (!is.xpdb(xpdb)) {
    stop('Valid `xpdb` input required.', call. = FALSE)
  }
  
  if (is.null(table) && is.null(problem)) {
    stop('Argument `table` or `problem` required.', call. = FALSE) 
  }
  
  if (!is.null(table) && !is.null(problem)) {
    stop('Arguments `table` and `problem` cannot be used together.', call. = FALSE) 
  }
  
  x <- xpdb$data
  
  if (!is.null(problem)) {
    # When selecting tables based on problem level
    if (!all(problem %in% x$problem)) {
      stop('Problem no.', stringr::str_c(problem[!problem %in% x$problem], collapse = ', '), 
           ' not found in model output data.', call. = FALSE)
    }
    x <- x$data[x$problem %in% problem]
    
    if (length(problem) > 1) {
      purrr::set_names(x, stringr::str_c('problem_', sort(problem), sep = ''))
    } else {
      x[[1]]
    }
    
  } else {
    # When selecting tables based on their name
    full_index <- x %>% 
      dplyr::select(dplyr::one_of('problem', 'index')) %>% 
      tidyr::unnest_(unnest_cols = 'index')
    
    if (any(!table %in% full_index$table)) {
      stop(stringr::str_c(table[!table %in% full_index$table], collapse = ', '), 
           ' not found in model output data.', call. = FALSE) 
    }
    x <- full_index[full_index$table %in% table, ] %>% 
      dplyr::group_by_(.dots = c('problem', 'table')) %>% 
      tidyr::nest(.key = 'tmp') %>% 
      dplyr::mutate(cols = purrr::map(.$tmp, ~.$col)) %>% 
      dplyr::group_by_(.dots = 'table') %>% 
      tidyr::nest(.key = 'tmp') %>% 
      dplyr::mutate(out = purrr::map(.$tmp, function(y) {
        x[x$problem == y$problem, ]$data[[1]][, y$cols[[1]]]
      }))
    
    if (length(unique(x$table)) > 1) {
      purrr::set_names(x$out, x$table)
    } else {
      x$out[[nrow(x)]]
    }
  }
}


#' Extract model output file data
#'
#' @description Extract model output file data from an xpdb object.
#' 
#' @param xpdb An \code{xpose_data} object from which the model output file data will be extracted.
#' @param file Name of the file to be extracted from the xpdb e.g. 'run001.ext'.
#' @param problem The problem to be used, by default returns the last one for each file.
#' @param subprob The subproblem to be used, by default returns the last one for each file.
#' 
#' @return A tibble for single file or a named list for multiple files.
#' @seealso \code{\link{xpose_data}}, \code{\link{read_nm_files}}
#' @examples
#' # Single file (returns a tibble)
#' ext_file <- get_file(xpdb_ex_pk, 'run001.ext')
#' ext_file
#' 
#' # Multiple files (returns a list)
#' files <- get_file(xpdb_ex_pk, c('run001.ext', 'run001.phi'))
#' files
#' 
#' # Tip to list available files in the xpdb
#' print(xpdb_ex_pk)
#' 
#' @export
get_file <- function(xpdb, file = NULL, problem = NULL, subprob = NULL) {
  if (!is.xpdb(xpdb)) {
    stop('Valid `xpdb` input required.', call. = FALSE)
  }
  
  if (is.null(file)) {
    stop('Argument `file` required.', call. = FALSE) 
  }
  
  # Filter by file
  if (any(!file %in% xpdb$files$name)) {
    stop(stringr::str_c(file[!file %in% xpdb$files$name], collapse = ', '), 
         ' not found in model output files.', call. = FALSE) 
  }
  x <- xpdb$files[xpdb$files$name %in% file, ]
  
  # Filter by $problem
  if (!is.null(problem)) {
    if (!all(problem %in% x$problem)) {
      stop('Problem no.', stringr::str_c(problem[!problem %in% x$problem], collapse = ', '), 
           ' not found in model output files.', call. = FALSE)
    }
    x <- x[x$problem %in% problem, ]
  }
  
  # Filter by sub-problem
  if (!is.null(subprob)) {
    if (!all(subprob %in% x$subprob)) {
      stop('Sub-problem no.', stringr::str_c(subprob[!subprob %in% x$subprob], collapse = ', '), 
           ' not found in model output files.', call. = FALSE)
    }
    x <- x[x$subprob %in% subprob, ]
  }
  
  # Select final record
  if (length(unique(x$name)) > 1) {
    x <- x[!duplicated(x$name, fromLast = TRUE), ]
    purrr::set_names(x$data, x$name)
  }
  x$data[[nrow(x)]]
}


#' Extract model summary data
#'
#' @description Extract model summary data from an xpdb object.
#' 
#' @param xpdb An \code{xpose_data} object from which the summary data will be extracted.
#' @param problem The problem to be used, by default returns the last one for each label.
#' @param subprob The subproblem to be used, by default returns the last one for each label.
#' @param only_last Logical, if \code{TRUE} only the last record for each label is returned in case 
#' of multiple problem and/or subproblem. If \code{FALSE} all values are returned.
#' 
#' @return A tibble of model summary.
#' @seealso \code{\link{xpose_data}}, \code{\link{template_titles}}
#' @examples
#' run_summary <- get_summary(xpdb_ex_pk)
#' run_summary
#' 
#' @export
get_summary <- function(xpdb, problem = NULL, subprob = NULL, only_last = FALSE) {
  if (!is.xpdb(xpdb)) {
    stop('Valid `xpdb` input required.', call. = FALSE)
  }
  
  x <- xpdb$summary
  
  # Filter by $problem
  if (!is.null(problem)) {
    if (!all(problem %in% x$problem)) {
      stop('Problem no.', stringr::str_c(problem[!problem %in% x$problem], collapse = ', '), 
           ' not found in model summary.', call. = FALSE)
    }
    x <- x[x$problem %in% problem, ]
  }
  
  # Filter by sub-problem
  if (!is.null(subprob)) {
    if (!all(subprob %in% x$subprob)) {
      stop('Sub-problem no.', stringr::str_c(subprob[!subprob %in% x$subprob], collapse = ', '), 
           ' not found in model summary.', call. = FALSE)
    }
    x <- x[x$subprob %in% subprob, ]
  }
  
  # Remove duplicates
  if (only_last) x <- x[!duplicated(x$label, fromLast = TRUE), ]
  
  x
}
