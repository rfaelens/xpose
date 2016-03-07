# Messages
msg <- function(txt, verbose = FALSE) { # From ronkeizer
  if (verbose) { message(txt) }
}

# Check xpdb argument
check_xpdb <- function(xpdb) {
  if (is.null(xpdb)) {
    stop('argument \"xpdb\" is missing with no default')
  }

  if (!is.null(xpdb) && class(xpdb) != 'xpose_data') {
    stop('argument \"xpdb\" must be of class xpose_data')
  }
}

# Check variables
check_vars <- function(vars, xpdb) {
 if (!all(vars %in% colnames(xpdb$data))) {
   stop('suplied variables (ie. ',
        paste(vars, collapse = ', '), ') not found in the data')
 }
}