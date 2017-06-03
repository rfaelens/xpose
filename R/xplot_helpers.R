# Check plot input variables
# check_vars <- function(vars, xpdb) {
#   if (!all(vars %in% colnames(xpdb$data))) {
#     vars[!vars %in% colnames(xpdb$data)]
#   }
# }

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
      {msg(c(., ' is not part of the available keywords. Check ?template_titles for a full list.'), quiet)}
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
last_problem <- function(xpdb, simtab = FALSE) {
  prob_n <- xpdb$data$simtab
  if (!simtab) prob_n <- !prob_n
  prob_n <- xpdb$data$problem[prob_n]
  if (length(prob_n) == 0) return(NA_integer_)
  max(prob_n)
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
