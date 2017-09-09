# Check xpdb
check_xpdb <- function(xpdb, check = 'data') {
  # Check the class
  if (!is.xpdb(xpdb)) {
    stop('Bad input to the argument `xpdb`', call. = FALSE)
  }
  
  skip  <- ifelse(check == FALSE, TRUE, FALSE)
  check <- ifelse(!check %in% c('data', 'files', 'summary', 'special', 'code'), 
                  'files', check)
  
  # Check for the presence of data
  if (!skip && is.null(xpdb[[check]])) {
    stop('No `', check, '` slot could be found in this xpdb.', call. = FALSE)
  }
}

# Check plot type
check_plot_type <- function(user_input, allowed) {
  user_input  <- stringr::str_extract_all(user_input, pattern = '.')[[1]]
  not_allowed <- user_input[!user_input %in% allowed]
  if (length(not_allowed) > 0) {
    warning('Plot type ', stringr::str_c('"',not_allowed, '"', collapse = ', '), 
         ' not recognized.', call. = FALSE)
  }
}

# Check plot scales
check_scales <- function(scale, log) {
  if (is.null(log)) return('continuous')
  ifelse(stringr::str_detect(string = log, pattern = scale), 'log10', 'continuous')
}

# Add suffix contained in the theme the labs
append_suffix <- function(xpdb, string = NULL, type = NULL) {
  if (is.null(string)) return()
  stringr::str_c(string, xpdb$xp_theme[stringr::str_c(type, '_suffix')], sep = '')
}

# Add keyword values in template titles
parse_title <- function(string, xpdb, problem, quiet, extra_key = NULL, extra_value = NULL) {
  # Extract keywords from the string
  keyword <- string %>% 
    stringr::str_extract_all('@[[:alnum:]]+') %>% 
    purrr::flatten_chr() %>% 
    stringr::str_replace(stringr::fixed('@'), '')
  
  # Get the associated values in the summart
  values <- xpdb$summary[xpdb$summary$problem %in% c(0, problem) & 
                           xpdb$summary$label %in% keyword, ]
  values <- values[!duplicated(values$label, fromLast = TRUE), ]
  
  # If needed add extra values e.g. in xpose_save
  if (!is.null(extra_key) && any(extra_key %in% keyword)) {
    values <- dplyr::bind_rows(values,
                               dplyr::tibble(problem = 0,
                                             subp  = 0,
                                             label = extra_key,
                                             value = extra_value))
  }
  
  # Remove unmatched keywords from the list
  if (!all(keyword %in% values$label)) {
    keyword[!keyword %in% values$label] %>% 
      unique() %>% 
      stringr::str_c(collapse = ', ') %>%   
      {warning(c(., ' is not part of the available keywords. Check ?template_titles for a full list.'), call. = FALSE)}
    keyword <- keyword[keyword %in% values$label]
  }
  
  if (length(keyword) == 0) return(string)
  
  # Replaces values in the string
  string %>% 
    stringr::str_replace_all(stringr::str_c('@', keyword, collapse = '|'), 
                             '${values$value[values$label == \"\\0\"]}') %>% 
    stringr::str_replace_all('\\"@', '\\"') %>% 
    stringr::str_interp()
}

# Subset an xp_theme
filter_xp_theme <- function(xp_theme, regex = NULL, action = 'keep') {
  match <- stringr::str_detect(names(xp_theme), regex)
  if (action == 'drop') match <- !match
  xp_theme[match]
}

# Get last problem
all_data_problem <- function(xpdb) {
  prob_n <- xpdb$data$problem
  if (length(prob_n) == 0) return(NA_integer_)
  unique(prob_n)
}

# Get last problem that is not a simulation problem (unless simtab is TRUE)
last_data_problem <- function(xpdb, simtab = FALSE) {
  prob_n <- xpdb$data$simtab
  if (!simtab) prob_n <- !prob_n
  prob_n <- xpdb$data$problem[prob_n]
  if (length(prob_n) == 0) return(NA_integer_)
  max(prob_n)
}

# Get the default problem to be plotted if problem has not been supplied 
# (the last estimation problem, or last sim problem if only simulations)
default_plot_problem <- function(xpdb){
  last_data_problem(xpdb, simtab = all(xpdb$data$simtab))
}

# Get all file problem
all_file_problem <- function(xpdb, ext) {
  prob_n <- xpdb$files$problem[xpdb$files$extension == ext]
  prob_n <- unique(prob_n)
  if (length(prob_n) == 0) return(NA_integer_)
  prob_n
}

# Get last file problem
last_file_problem <- function(xpdb, ext) {
  prob_n <- all_file_problem(xpdb = xpdb, ext = ext)
  max(prob_n)
}

# Get file subproblem
last_file_subprob <- function(xpdb, ext, problem) {
  subprob_n <- xpdb$files$subprob[xpdb$files$extension == ext & xpdb$files$problem == problem]
  subprob_n <- unique(subprob_n)
  if (length(subprob_n) == 0) return(NA_integer_)
  max(subprob_n)
}

# Get only columns that have several unique values
drop_static_cols <- function(xpdb, problem, cols, quiet) {
  if (is.null(cols)) return()
  cols_rm <- get_data(xpdb, problem = problem) %>% 
    dplyr::select_(.dots = cols) %>%
    dplyr::select_if(.predicate = function(x) length(unique(x)) == 1) %>% 
    colnames()
  if (length(cols_rm) == 0) return(cols)
   dplyr::if_else(length(cols_rm) > 5, 
                 stringr::str_c(stringr::str_c(cols_rm[1:5], collapse = ', '), 
                                '... and', length(cols_rm) - 5 , 'more', sep = ' '),
                 stringr::str_c(cols_rm , collapse = ', ')) %>%
                 {msg(c('Static variables ', .,' will be dropped'), quiet)}
  dplyr::setdiff(x = cols, y = cols_rm)
}


# Get a variable name from xpose
xp_var <- function(xpdb, problem, col = NULL, type = NULL) {
  index <- xpdb$data[xpdb$data$problem == problem, ]$index[[1]]
  if (!is.null(type)) index <- index[index$type %in% type, ]
  if (!is.null(col)) index <- index[index$col %in% col, ]
  if (nrow(index) == 0) return()
  
  index %>% 
    dplyr::distinct_(.dots = 'col', .keep_all = TRUE) %>% 
    dplyr::select(dplyr::one_of('col', 'type', 'label', 'units')) %>% 
    dplyr::arrange_(.dots = c('type', 'col'))
}

# Set new default value for aesthetics
aes_c <- function(fun_aes, user_aes) {
  if (is.null(user_aes)) return(fun_aes)
  aes <- c(fun_aes[!names(fun_aes) %in% names(user_aes)], user_aes)
  structure(aes, class = 'uneval')
}

# Filter aesthetics
aes_filter <- function(mapping, drop = NULL, keep_only = NULL) {
  if (is.null(mapping)) return()
  if (!is.null(keep_only)) {
    aes <- mapping[names(mapping) %in% keep_only]
  } else {
    aes <- mapping[!names(mapping) %in% drop]
  }
  if (length(aes) == 0) return()
  aes
}

# Rename aesthetics
aes_rename <- function(mapping, from, to) {
  if (is.null(mapping)) return()
  if (!any(from %in% names(mapping))) return(mapping)
  names(mapping)[match(from, names(mapping))] <- to[which(from %in% names(mapping))]
  mapping
}
