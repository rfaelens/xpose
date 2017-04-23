# Check plot input variables
check_vars <- function(vars, xpdb) {
  if (!all(vars %in% colnames(xpdb$data))) {
    vars[!vars %in% colnames(xpdb$data)]
  }
}

# Check plot scales
check_scales <- function(scale, log) {
  if (is.null(log)) return('continuous')
  ifelse(stringr::str_detect(string = log, pattern = scale), 'log10', 'continuous')
}

# Check plot titles
check_title <- function(x, default) {
  ifelse(is.null(x), default, x)
}

# Get key values in template titles
parse_title <- function(string, xpdb, extra_key = NULL, extra_value = NULL) {
  keys   <- unlist(regmatches(string, gregexpr('@[[:alnum:]]+_?[[:alnum:]]+', string)))
  values <- xpdb$summary[substring(keys, 2)]
  
  if (!is.null(extra_key) && any(keys == extra_key)) {
    values[keys == extra_key] <- extra_value
  }
  
  for (s in seq_along(keys)) {
    if (is.null(values[[s]])) { 
      warning(keys[s], ' not part of the available keywords for titles', call. = FALSE)
    } else {
      string <- gsub(keys[s], values[[s]], string) 
    }
  }
  string
}

# Generate template title
write_title <- function(x, xpdb) {
  if (!is.null(x) && x != FALSE) {
    parse_title(x, xpdb)
  } else { 
    return(NULL)
  }
}

