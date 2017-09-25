#' Access model code
#'
#' @description Access model code from an xpdb object.
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
  check_xpdb(xpdb, check = 'code')
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


#' Access model output table data
#'
#' @description Access model output table data from an xpdb object.
#' 
#' @param xpdb An \code{xpose_data} object from which the model output file data will be extracted.
#' @param table Name of the output table to be extracted from the xpdb e.g. 'sdtab001'. Alternative to 
#' the `problem` argument.
#' @param problem Accesses all tables from the specified problem. Alternative to the `table` argument.
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
  check_xpdb(xpdb, check = 'data')
  
  if (is.null(table) && is.null(problem)) {
    stop('Argument `table` or `problem` required.', call. = FALSE) 
  }
  
  if (!is.null(table) && !is.null(problem)) {
    stop('Arguments `table` and `problem` cannot be used together.', call. = FALSE) 
  }
  
  if (!is.null(problem) && is.na(problem)) return() # For internal use
  
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


#' Access model output file data
#'
#' @description Access model output file data from an xpdb object.
#' 
#' @param xpdb An \code{xpose_data} object from which the model output file data will be extracted.
#' @param file Full name of the file to be extracted from the xpdb e.g. 'run001.phi'. Alternative to the 'ext' argument.
#' @param ext Extension of the file to be extracted from the xpdb e.g. 'phi'. Alternative to the 'file' argument.
#' @param problem The problem to be used, by default returns the last one for each file.
#' @param subprob The subproblem to be used, by default returns the last one for each file.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' 
#' @return A tibble for single file or a named list for multiple files.
#' @seealso \code{\link{xpose_data}}, \code{\link{read_nm_files}}
#' @examples
#' # Single file (returns a tibble)
#' ext_file <- get_file(xpdb_ex_pk, file = 'run001.ext')
#' ext_file
#' 
#' # Multiple files (returns a list)
#' files <- get_file(xpdb_ex_pk, file = c('run001.ext', 'run001.phi'))
#' files
#' 
#' # Tip to list available files in the xpdb
#' print(xpdb_ex_pk)
#' 
#' @export
get_file <- function(xpdb, file = NULL, ext = NULL, problem = NULL, subprob = NULL, quiet) {
  check_xpdb(xpdb, check = 'files')
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  if (is.null(file) && is.null(ext)) {
    stop('Argument `file` or `ext` required.', call. = FALSE) 
  }
  
  if (!is.null(file) && !is.null(ext)) {
    stop('Argument `file` and `ext` should not be used simultaneously.', call. = FALSE) 
  }
  
  # Get file name from extension
  if (!is.null(ext)) {
    file <- unique(xpdb$files$name[xpdb$files$extension %in% ext])
    if (length(file) == 0) {
      stop('File extension ', stringr::str_c(ext[!ext %in% xpdb$files$extension], collapse = ', '), ' not found in model output files.', call. = FALSE) 
    }
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
  } else {
    last_row <- nrow(x)
    msg(c('Returning data from problem no.', x$problem[last_row] , ' sub-problem no.', x$subprob[last_row]), quiet)
    x$data[[last_row]]
  }
}


#' Access model summary data
#'
#' @description Access model summary data from an xpdb object.
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
  check_xpdb(xpdb, check = 'summary')
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


#' Access model parameters
#'
#' @description Access model parameter estimates from an xpdb object.
#' 
#' @param xpdb An \code{xpose_data} object from which the model output file data will be extracted.
#' @param problem The problem to be used, by default returns the last one for each file.
#' @param subprob The subproblem to be used, by default returns the last one for each file.
#' @param signif The number of significant digits to be displayed.
#' @param show_all Logical, whether the 0 fixed off-diagonal elements should be removed outputed or not.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' 
#' @return A tibble for single problem/subprob or a named list for multiple problem|subprob.
#' @examples
#' get_prm(xpdb_ex_pk, problem = 1)
#' 
#' @export
get_prm <- function(xpdb, 
                    problem  = NULL, 
                    subprob  = NULL, 
                    digits   = 4,
                    show_all = FALSE,
                    quiet) {
  
  check_xpdb(xpdb, check = 'files')
  if (missing(quiet)) quiet <- xpdb$options$quiet
  if (is.null(problem)) problem <- last_file_problem(xpdb, ext = 'ext')
  if (is.null(subprob)) subprob <- last_file_subprob(xpdb, ext = 'ext', problem = problem)
  
  # Initate raw ext file formating
  ext_file <- get_file(xpdb = xpdb, ext = 'ext', problem = problem, 
                       subprob = subprob, quiet = quiet) %>% 
    dplyr::filter(.$ITERATION %in% c(-1000000000, -1000000001, -1000000006)) %>% 
    dplyr::mutate(name = dplyr::case_when(.$ITERATION == -1000000000 ~ 'value', 
                                          .$ITERATION == -1000000001 ~ 'rse',
                                          TRUE ~ 'fixed')) %>% 
    dplyr::select(colnames(.)[!colnames(.) %in% c('ITERATION', 'OBJ')]) %>% 
    {as.data.frame(t(.), stringsAsFactors = FALSE)} %>% 
    {purrr::set_names(x = ., nm = purrr::flatten_chr(.[nrow(.),]))} %>% 
    dplyr::mutate(name  = row.names(.)) %>% 
    dplyr::slice(-nrow(.)) %>% 
    dplyr::mutate(value = as.numeric(.$value),
                  fixed = as.logical(as.numeric(.$fixed)))
  
  # Check RSE column
  if (any(colnames(ext_file) == 'rse')) {
    has_rse  <- TRUE
    ext_file <- dplyr::mutate(.data = ext_file, rse = as.numeric(ext_file$rse))
  } else {
    has_rse      <- FALSE
    ext_file$rse <- NA
  }
  
  # Add flag for diagonal elements identification
  ext_file <- ext_file %>% 
    dplyr::mutate(type = dplyr::case_when(stringr::str_detect(.$name, 'THETA') ~ 'the',
                                          stringr::str_detect(.$name, 'OMEGA') ~ 'ome',
                                          stringr::str_detect(.$name, 'SIGMA') ~ 'sig'),
                  number = stringr::str_replace_all(.$name, '[^\\d,]+', '')) %>% 
    tidyr::separate(col = 'number', into = c('m', 'n'), sep = ',', 
                    fill = 'right') %>% 
    dplyr::mutate(diagonal = dplyr::if_else(.$m == .$n, TRUE, FALSE),
                  m = NULL, n = NULL)
  
  # Convert RSE to CV%
  if (has_rse) {
    ext_file <- ext_file %>% 
      dplyr::mutate(rse = dplyr::case_when(.$fixed ~ NA_real_,
                                           .$type == 'the' ~ abs(.$rse / .$value),
                                           TRUE ~ abs(.$rse / .$value) / 2)) # Approximate standard deviation scale
  }
  
  # Change variances to CV%, round values and reorder row/cols
  ext_file$value[ext_file$type %in% c('ome', 'sig') & ext_file$diagonal] <- 
    sqrt(ext_file$value[ext_file$type %in% c('ome', 'sig') & ext_file$diagonal])
  
  ext_file <- ext_file %>%
    dplyr::mutate(label = '',
                  value = signif(.$value, digits = digits),
                  rse   = signif(.$rse, digits = digits),
                  order = dplyr::case_when(.$type == 'the' ~ 1,
                                           .$type == 'ome' ~ 2,
                                           TRUE ~ 3)) %>% 
    dplyr::arrange_(.dots = 'order') %>% 
    dplyr::select(dplyr::one_of('type', 'name', 'label', 'value', 'rse', 'fixed', 'diagonal'))
  
  # Assign THETA labels
  n_theta     <- sum(ext_file$type == 'the')
  theta_names <- xpdb$code$comment[xpdb$code$subroutine == 'the']
  if (n_theta != length(theta_names)) {
    warning('$THETA labels did not match the number of THETAs in the `.ext` file.', call. = FALSE)
  } else {
    ext_file$label[ext_file$type == 'the'] <- theta_names
  }
  
  # Assign OMEGA labels
  n_omega     <- sum(ext_file$type == 'ome' & ext_file$diagonal, na.rm = TRUE)
  omega_names <- xpdb$code %>% 
    dplyr::filter(.$subroutine == 'ome') %>% 
    dplyr::filter(!(stringr::str_detect(.$code, 'BLOCK\\(\\d+') & .$comment == '')) %>% 
    {purrr::flatten_chr(.[, 'comment'])}
  
  if (n_omega != length(omega_names)) {
    warning('$OMEGA labels did not match the number of OMEGAs in the `.ext` file.', call. = FALSE)
  } else {
    ext_file$label[ext_file$type == 'ome' & ext_file$diagonal] <- omega_names
  }
  
  # Assign SIGMA labels
  n_sigma     <- sum(ext_file$type == 'sig' & ext_file$diagonal, na.rm = TRUE)
  sigma_names <- xpdb$code$comment[xpdb$code$subroutine == 'sig']
  if (n_sigma != length(sigma_names)) {
    warning('$SIGMA labels did not match the number of SIGMAs in the `.ext` file.', call. = FALSE)
  } else {
    ext_file$label[ext_file$type == 'sig' & ext_file$diagonal] <- sigma_names
  }
  
  # Filter_all
  if (!show_all) {
    ext_file <- ext_file %>% 
      filter(!(.$type %in% c('ome', 'sig') & .$value == 0 & !.$diagonal))
  }
  
  structure(ext_file, class = c('xpose_prm', class(ext_file)))
}
