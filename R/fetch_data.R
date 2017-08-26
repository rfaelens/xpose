#' Create options for data import
#' 
#' @description Provide a list of options to the general plotting functions such as 
#' \code{xplot_scatter} in order to create appropriate data input for ggplot2.
#' 
#' @param problem The problem to be used, by default returns the last one.
#' @param subprob The subproblem to be used, by default returns the last one.
#' @param source Define the location of the data in the xpdb. Should be either 'data' 
#' to use the output tables or the name of an output file attached to the xpdb.
#' @param simtab Only used when 'data' is defined as the source and `problem` is default. Should the data be coming 
#' from an estimation or a simulation table.
#' @param filter A function used to filter the data e.g. filter = function(x) x[x$TIME > 20, ] where x is the data.
#' @param tidy Logical, whether the data should be transformed to tidy data.
#' @param index_col Only used when 'tidy' is defined a \code{TRUE} and \code{value_col} is \code{NULL}. 
#' Column names to use as index when tidying the data.
#' @param value_col Only used when 'tidy' is defined a \code{TRUE} and \code{index_col} is \code{NULL}. 
#' Column names to be stacked when tidying the data.
#'
#' @seealso \code{{xplot_scatter}}
#' 
#' @examples
#' data_opt(problem = 1, source = 'data', simtab = TRUE)
#' 
#' @export
data_opt <- function(problem   = NULL, 
                     subprob   = NULL, 
                     source    = 'data', 
                     simtab    = FALSE,
                     filter    = NULL,
                     tidy      = FALSE,
                     index_col = NULL,
                     value_col = NULL) {
  list(problem = problem, subprob = subprob, source = source, 
       simtab = simtab, filter = filter, tidy = tidy, 
       index_col = index_col, value_col = value_col)
}

# Create shortcut functions on the fly to filter observations
only_obs <- function(xpdb, problem, quiet) {
  mdv_var <- xp_var(xpdb, problem, type = c('evid', 'mdv'))$col[1]
  fun <- function(x) {}
  if (!is.null(mdv_var)) {
    string <- c('Filtering data by ', mdv_var, ' == 0')
    body(fun) <- bquote({
      msg(.(string), .(quiet))
      x[x[, .(mdv_var)] == 0, ]
    })
  } else {
    string <- c('No `evid` or `mdv` variable available to filter the data.')
    body(fun) <- bquote({
      msg(.(string), .(quiet))
      x
    }) 
  }
  fun
}

# Create shortcut functions on the fly to remove duplicates
only_distinct <- function(xpdb, problem, facets, quiet) {
  if (is.formula(facets)) facets <- all.vars(facets)
  vars <- c(xp_var(xpdb, problem, type = c('id'))$col[1], facets)
  string <- c('Removing duplicated rows based on: ', stringr::str_c(vars, collapse = ', '))
  fun <- function(x) {}
  body(fun) <- bquote({
    msg(.(string), .(quiet))
    dplyr::distinct_(.data = x, .dots = .(vars), .keep_all = TRUE)
  })
  fun
}

# Main function to get the data from different source and prepare it for plotting
fetch_data <- function(xpdb, 
                       problem   = NULL, 
                       subprob   = NULL,
                       source    = 'data', 
                       simtab    = FALSE,
                       filter    = NULL,
                       tidy      = FALSE, 
                       index_col = NULL,
                       value_col = NULL,
                       quiet     = FALSE) {
  
  if (source == 'data') {
    if (is.null(problem)) problem <- last_data_problem(xpdb, simtab)
    if (is.na(problem)) {
      stop(c('No data associated with $prob no.', problem, ' could be found.'), call. = FALSE)
    }
    msg(c('Using data from $prob no.', problem), quiet)
    data <- get_data(xpdb, problem = problem)
  } else {
    if (!any(xpdb$files$extension == source)) {
      stop(c('File extension ', source, ' not found in model output files.'), call. = FALSE) 
    }
    if (is.null(problem)) problem <- last_file_problem(xpdb, source)
    if (is.null(subprob)) subprob <- last_file_subprob(xpdb, source, problem)
    msg(c('Using ', xpdb$files$name[xpdb$files$extension == source] , ' $prob no.', problem, ' subprob no.', subprob, '.'), quiet)
    data <- get_file(xpdb, file = NULL, ext = source, problem = problem, subprob = subprob, quiet = TRUE)
  }
  
  if (is.function(filter)) data <- filter(data)
  
  if (tidy) {
    if (!is.null(value_col)) { 
      index_col <- colnames(data)[!colnames(data) %in% value_col]
    }
    
    dplyr::if_else(length(index_col) > 5, 
                   stringr::str_c(stringr::str_c(index_col[1:5], collapse = ', '), 
                                  '... and', length(index_col) - 5 , 'more variables', sep = ' '),
                   stringr::str_c(index_col , collapse = ', ')) %>%
                   {msg(c('Tidying data by ', .), quiet)}
    data <- tidyr::gather_(data = data, key_col = 'variable', value_col = 'value',
                           gather_cols = colnames(data)[!colnames(data) %in% index_col])
  }
  
  # Add metadata to output
  attributes(data) <- c(attributes(data), 
                        list(problem = problem, simtab = simtab,
                             subprob = subprob, source = source))
  data
}
