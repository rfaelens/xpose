# Messages
msg <- function(txt, verbose = FALSE) { # From ronkeizer
  if (verbose) { message(txt) }
}

# Check xpdb argument (return true false instead!!)
check_xpdb <- function(xpdb) {
  if (is.null(xpdb)) {
    stop('argument \"xpdb\" is missing with no default', call. = FALSE)
  }
  
  if (!is.null(xpdb) && class(xpdb) != 'xpose_data') {
    stop('argument \"xpdb\" must be of class xpose_data', call. = FALSE)
  }
}

# Check variables (return true false instead!!)
check_vars <- function(vars, xpdb) {
  if (!all(vars %in% colnames(xpdb$data))) {
    stop('requested variables ',
         paste(vars[!vars %in% colnames(xpdb$data)],
               collapse = ', '), ' not found in the data', call. = FALSE)
  }
}


# Check title
check_title <- function(x, default) {
  if (is.null(x)) {
    default 
  } else {
    x
  }
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

