#' NONMEM output table import function
#'
#' @description Quickly import NONMEM output tables into R.
#' When both \code{skip} and \code{header} are \code{NULL},
#' \code{read_nm_tables} will automatically detect the optimal
#' settings to import the tables. When more than one files are
#' provided for a same NONMEM run, they will be combined into
#' a single \code{data.frame}.
#'
#' @param files A character vector of path to the files or a \code{nm_table_list} file created with \code{list_nm_tables}.
#' @param combined Logical value indicating whether multiple tables should be combined into a single one. If the number of rows 
#' does not match an error will be returned.
#' @param rm_duplicates Logical value indicating whether duplicated columns should be removed.
#' @param verbose Logical, if \code{TRUE} messages are printed to the console.
#' @param ... Additional arguments to be passed to the \code{\link[readr]{read_delim}} functions.
#' @examples
#' \dontrun{
#' data <- read_nm_tables(files = 'inst/extdata/sdtab001')
#' }
#' @export
read_nm_tables <- function(files = NULL,
                           combined = TRUE,
                           rm_duplicates = TRUE,
                           verbose = TRUE,
                           ...) {
  
  if (!is.null(files) && !is.nm.table.list(files)) {
    files <- dplyr::tibble(problem   = 1, 
                           file      = files,
                           firstonly = FALSE,
                           simtab    = FALSE)
  }
  
  if (is.null(files) || !any(file.exists(files$file))) {
    msg('No table file could be found.', verbose)
    return()
  }
  
  if (any(duplicated(files$file))) {
    msg('No tables imported due to duplicated names', verbose)
    return()
  }
  
  user_mode <- !is.nm.table.list(files)
  
  tables <- files %>% 
    dplyr::filter(file.exists(.$file)) %>% 
    purrr::by_row(~readr::read_lines(file = .$file, n_max = 3), .to = 'top') %>%
    purrr::by_row(read_args, verbose, .collate = 'rows') %>% #, ...) %>%
    dplyr::mutate(name = basename(.$file)) %>% 
    dplyr::select(dplyr::one_of('problem', 'name', 'simtab', 'firstonly', 'fun', 'params'))
  
  msg(c('Reading:\n', paste0(' - ', tables$name, '\t(prob no. ', tables$problem, 
                             dplyr::if_else(tables$simtab, ', simtab', ''),
                             dplyr::if_else(tables$firstonly, ', firstonly', ''),
                             ')', collapse = '\n')), verbose)
  
  tables <- tables %>% 
    dplyr::bind_cols(tables %>% 
                       dplyr::select(dplyr::one_of(c('fun', 'params'))) %>% 
                       {purrr::invoke_map(.$fun, .$params)} %>%
                       dplyr::tibble(data = .))
  
  if (!combined) return(purrr::set_names(x = purrr::map(tables$data, stats::na.omit), nm = tables$name))
  
  # Index datasets
  tables <- dplyr::bind_cols(tables,
                             dplyr::tibble(index = purrr::map(tables$data, colnames) %>% 
                                             purrr::set_names(tables$name),
                                           nrow  = purrr::map_dbl(tables$data, nrow)))
  
  # Combine tables with same number of rows
  tables <- tables %>% 
    purrr::slice_rows(c('problem', 'simtab', 'firstonly')) %>%  
    purrr::by_slice(combine_tables, verbose, .collate = 'rows')
  
  # Merge firsonly tables with main tables
  if (any(tables$firstonly)) {
    msg('Consolidating tables with `firstonly`', verbose)
    tables <- tables %>%
      purrr::slice_rows(c('problem', 'simtab')) %>%
      purrr::by_slice(merge_firstonly, verbose, .collate = 'rows')
  }
  
  # Remove duplicated columns to decrease xpdb size
  if (rm_duplicates) {
    tables <- dplyr::bind_cols(
      dplyr::select(.data = tables, dplyr::one_of(c('problem', 'simtab', 'index'))),
      purrr::by_row(.d = tables, .labels = FALSE, .to = 'data', 
                    ..f = ~ dplyr::select(.data = .$data[[1]], 
                                          dplyr::one_of(unique(unlist(.$index[[1]]$colnames)))))
    )
  }
  
  # If user mode return simple tibble
  if (user_mode) return(tables$data[[1]])
  
  tables
}

read_funs <- function(fun) {
  c(csv = readr::read_csv,
    csv2 = readr::read_csv2,
    table = readr::read_table,
    table2 = readr::read_table2)[fun]
}

read_args <- function(x, verbose, col_types, ...) {
  if (missing(col_types)) col_types <- readr::cols(.default = 'd')
  top <- x$top[[1]]
  fun <- dplyr::case_when(stringr::str_detect(top[3], '\\d,\\d+E[+-]\\d+\\s*;') ~ 'csv2',
                          stringr::str_detect(top[3], '\\d.\\d+E[+-]\\d+\\s*,') ~ 'csv', 
                          stringr::str_detect(top[3], '\\d,\\d+E[+-]\\d+\\s+') ~ 'table2',
                          TRUE ~ 'table')
  skip <- dplyr::if_else(stringr::str_detect(top[1], 'TABLE NO\\.\\s+\\d'), 1, 0)
  header <- dplyr::if_else(stringr::str_detect(top[1 + skip], '[A-z]{2,}+'), TRUE, FALSE)
  msg(c(basename(x$file), 'has no header. These tables are not supported in xpose'), verbose = !header & verbose)
  dplyr::tibble(fun = read_funs(fun),
                params = list(list(file = x$file, skip = skip, 
                                   col_names = header, col_types = col_types, ...)))
}

combine_tables <- function(x, verbose) {
  if (length(unique(x$nrow)) > 1) {
    msg(c('Missmatch in the number of rows between: ', 
          stringr::str_c(x$name, collapse = ', '), 
          '. These tables were dropped.'), verbose)
    return(dplyr::tibble(data = list(), index = list()))
    
  }
  dplyr::tibble(data = x$data %>%
                  dplyr::bind_cols() %>%
                  purrr::set_names(make.unique(names(.))) %>%
                  stats::na.omit() %>%
                  list(),
                
                # Get around purrr droping list names
                index = list(dplyr::tibble(tables = x$name, 
                                           colnames = x$index)))
}

merge_firstonly <- function(x, verbose) {
  if (nrow(x) == 1) {
    # No merge needed
    return(dplyr::tibble(data = x$data, index = x$index))
  } else if (nrow(x) != 2) {
    msg(c('Something went wrong while consolidating : ', 
          paste0(x[x$firstonly == TRUE, ]$index[[1]]$tables, 
                 collapse = ', ')), verbose) 
    return(dplyr::tibble(data = list(), index = list()))
  }
  dplyr::tibble(data = list(dplyr::full_join(x = x$data[x$firstonly == FALSE][[1]], 
                                             y = x$data[x$firstonly == TRUE][[1]])),
                index = x$index %>% 
                  dplyr::bind_rows() %>% 
                  list())
}
