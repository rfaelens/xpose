#' Extract output file data
#'
#' @description Extract output file data from an xpdb object.
#' 
#' @param xpdb An \code{xpose_data} object from which data will be extracted.
#' @param file Output file(s) to be extracted from the xpdb e.g. 'run001.ext'.
#' @param problem The problem to be used if multiple problem are available. By default 
#' returns the last problem for each file.
#' @param subprob The subproblem to be used if multiple subproblem are available. By default 
#' returns the last subproblem for each file.
#' 
#' @return A tibble for single file or a named list for multiple files.
#' @examples
#' # Single file (returns a tibble)
#' ext_file <- get_file(xpdb_ex_pk, 'run001.ext')
#' 
#' # Multiple files (returns a list)
#' files <- get_file(xpdb_ex_pk, c('run001.ext', 'run001.phi'))
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
        ' not found.', call. = FALSE) 
  }
  x <- xpdb$files[xpdb$files$name %in% file, ]
  
  # Filter by $problem
  if (!is.null(problem)) {
    if (!all(problem %in% x$prob)) {
      stop('Problem no.', problem, ' not found for ', 
           stringr::str_c(file, collapse = ', '), '.', call. = FALSE)
    }
    x <- x[x$prob %in% problem, ]
  }
  
  # Filter by sub-problem
  if (!is.null(subprob)) {
    if (!all(subprob %in% x$subprob)) {
      stop('Sub-problem no.', subprob, ' not found for ', 
           stringr::str_c(file, collapse = ', '), '.', call. = FALSE)
    }
    x <- x[x$subprob %in% subprob, ]
  }
  
  # Select final record
  if (length(file) > 1) {
    x <- x[!duplicated(x$name, fromLast = TRUE), ]
    purrr::set_names(x$data, x$name)
  } else {
    x$data[[1]]
  }
}

#get_data()
#get_code()
#get_summary()

