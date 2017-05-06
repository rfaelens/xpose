#' NONMEM output table import function
#'
#' @description Quickly import NONMEM output tables into R. This function automatically 
#' detects the optimal settings to import the tables from nonmem.
#'
#' @param files A character vector of path to the files or a \code{nm_table_list} file created with \code{list_nm_tables}.
#' @param combined Logical value indicating whether multiple tables should be combined into a single one. If the number of rows 
#' does not match an error will be returned.
#' @param rm_duplicates Logical value indicating whether duplicated columns should be removed.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' @param ... Additional arguments to be passed to the \code{\link[readr]{read_delim}} functions.
#' @examples
#' \dontrun{
#' data <- read_nm_tables(files = 'inst/extdata/sdtab001')
#' }
#' @export
read_nm_tables <- function(files         = NULL,
                           combined      = TRUE,
                           rm_duplicates = TRUE,
                           quiet         = FALSE,
                           ...) {
  
  if (!is.null(files) && !is.nm.table.list(files)) {
    files <- dplyr::tibble(problem   = 1, 
                           file      = files,
                           firstonly = FALSE,
                           simtab    = FALSE)
  }
  
  if (is.null(files) || !any(file.exists(files$file))) {
    msg('No table file could be found.', quiet)
    return()
  }
  
  if (any(duplicated(files$file))) {
    msg('No tables imported due to duplicated names', quiet)
    return()
  }
  
  user_mode <- !is.nm.table.list(files)
  
  tables <- files %>% 
    dplyr::filter(file.exists(.$file))
  
  if (length(unique(tables$problem)) > 1) {
    tables %>% 
      dplyr::mutate(name = stringr::str_c(basename(.$file), dplyr::if_else(.$firstonly, ' (firstonly)', ''))) %>% 
      purrr::slice_rows('problem') %>% 
      purrr::by_slice(~stringr::str_c(.$name, collapse = ', '), 
                      .collate = 'rows', .to = 'string') %>% 
                      {stringr::str_c(' - $prob no.', .$problem, ': ', .$string, collapse = '\n')} %>% 
    {msg(c('Reading:\n', .), quiet)}
  } else {
    msg(c('Reading: ', paste0(basename(tables$file), dplyr::if_else(tables$firstonly, ' (firstonly)', ''),
                              collapse = ', ')), quiet)
  }
  
  tables <- tables %>% 
    purrr::by_row(~readr::read_lines(file = .$file, n_max = 3), .to = 'top') %>%
    purrr::by_row(~read_args(x = . , quiet, ...), .collate = 'rows') %>%
    dplyr::mutate(name = basename(.$file)) %>% 
    dplyr::select(dplyr::one_of('problem', 'name', 'simtab', 'firstonly', 'fun', 'params'))
  
  if (nrow(tables) == 0) {
    msg('\nNo table imported.', quiet)
    return() 
  }
  
  tables <- tables %>% 
    dplyr::bind_cols(tables %>% 
                       dplyr::select(dplyr::one_of(c('fun', 'params'))) %>% 
                       {purrr::invoke_map(.$fun, .$params)} %>%
                       dplyr::tibble(data = .))
  
  if (!combined) return(purrr::set_names(x = purrr::map(tables$data, tidyr::drop_na()), nm = tables$name))
  
  # Index datasets
  tables <- dplyr::bind_cols(tables,
                             dplyr::tibble(index = purrr::map(tables$data, colnames) %>% 
                                             purrr::set_names(tables$name),
                                           nrow  = purrr::map_dbl(tables$data, nrow)))
  
  # Combine tables with same number of rows
  tables <- tables %>% 
    purrr::slice_rows(c('problem', 'simtab', 'firstonly')) %>%  
    purrr::by_slice(combine_tables, quiet, .collate = 'rows')
  
  if (nrow(tables) == 0) {
    msg('\nNo table imported.', quiet)
    return() 
  }
  
  # Merge firsonly tables with main tables
  if (any(tables$firstonly)) {
    msg('Consolidating tables with `firstonly`', quiet)
    tables <- tables %>%
      purrr::slice_rows(c('problem', 'simtab')) %>%
      purrr::by_slice(merge_firstonly, quiet, .collate = 'rows')
  }
  
  if (nrow(tables) == 0) {
    msg('\nNo table imported.', quiet)
    return() 
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

read_args <- function(x, quiet, col_types, ...) {
  if (missing(col_types)) col_types <- readr::cols(.default = 'd')
  top <- x$top[[1]]
  
  if (is.na(top[3])) {
    msg(c('Dropped: ', basename(x$file), ' due to unexpected data format'), quiet)
    return(dplyr::tibble(fun = list(), params = list()))
  }
  
  fun <- dplyr::case_when(stringr::str_detect(top[3], '\\d,\\d+E[+-]\\d+\\s*;') ~ 'csv2',
                          stringr::str_detect(top[3], '\\d.\\d+E[+-]\\d+\\s*,') ~ 'csv', 
                          stringr::str_detect(top[3], '\\d,\\d+E[+-]\\d+\\s+') ~ 'table2',
                          TRUE ~ 'table')
  
  skip <- dplyr::if_else(stringr::str_detect(top[1], 'TABLE NO\\.\\s+\\d'), 1, 0)
  header <- dplyr::if_else(stringr::str_detect(top[1 + skip], '[A-z]{2,}+'), TRUE, FALSE)
  
  if (!header) {
    msg(c('Dropped: ', basename(x$file), ' due to missing headers.'), quiet = !header & quiet)
    return(dplyr::tibble(fun = list(), params = list()))
  }
  
  dplyr::tibble(fun = read_funs(fun),
                params = list(list(file = x$file, skip = skip, 
                                   col_names = header, col_types = col_types, ...)))
}

combine_tables <- function(x, quiet) {
  if (length(unique(x$nrow)) > 1) {
    msg(c('Dropped: ', stringr::str_c(x$name, collapse = ', '), 
          ' due to missmatch in row number.'), quiet)
    return(dplyr::tibble(data = list(), index = list()))
    
  }
  dplyr::tibble(data = x$data %>%
                  dplyr::bind_cols() %>%
                  purrr::set_names(make.unique(names(.))) %>%
                  tidyr::drop_na() %>%
                  list(),
                
                # Get around purrr droping list names
                index = list(dplyr::tibble(tables = x$name, 
                                           colnames = x$index)))
}

merge_firstonly <- function(x, quiet) {
  if (nrow(x) == 1) {
    # No merge needed
    return(dplyr::tibble(data = x$data, index = x$index))
  } else if (nrow(x) != 2) {
    msg(c(' * Something went wrong while consolidating: ', 
          paste0(x[x$firstonly == TRUE, ]$index[[1]]$tables, 
                 collapse = ', ')), quiet) 
    return(dplyr::tibble(data = list(), index = list()))
  }
  xdata   <- x$data[x$firstonly == FALSE][[1]]
  ydata   <- x$data[x$firstonly == TRUE][[1]]
  by_vars <- intersect(colnames(xdata), colnames(ydata))
  msg(c(' * Joining by: ', paste0(by_vars, collapse = ', ')), quiet)
  dplyr::tibble(data = list(dplyr::left_join(x  = xdata, 
                                             y  = ydata,
                                             by = by_vars)),
                index = x$index %>% 
                  dplyr::bind_rows() %>% 
                  list())
}
