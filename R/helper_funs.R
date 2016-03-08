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
    stop('requested variables ',
         paste(vars[!vars %in% colnames(xpdb$data)],
               collapse = ', '), ' not found in the data')
  }
}


# Interactive title
parse_title <- function(string, xpdb) {
  # Add safety, how?
  attach(xpdb$mod_info, warn.conflicts = FALSE)
  return(string)
}

# Make titles
titlr <- function(plot_name, subfun, title, subtitle, xpdb) {
  if (is.null(title)) {
    title <- paste0(plot_name, ' (', xpdb[['modfile']], ')')
  } else if (!(is.logical(title) && title == FALSE)) {
    title <- parse_title(title, xpdb)
  }

  if (is.null(subtitle)) {
    subtitle <- xpdb$mod_info[[subfun]]
  } else if (!(is.logical(subtitle) && subtitle == FALSE)) {
    subtitle <- parse_title(subtitle, xpdb)
  }

  return( list(title, subtitle) )
}
