# Reports whether x is an xpose theme object
is.xp.theme <- function(x) {
  inherits(x, 'xpose_theme')
}

# Reports whether x is an xpose data object
is.xpdb <- function(x) {
  inherits(x, 'xpose_data')
}

# Check whether x is a formula
is.formula <- function(x) {
  inherits(x, 'formula')
}

# Message function with verbose option (from Ron Keizer)
msg <- function(txt, verbose = FALSE) {
  if (verbose) message(txt)
}

# Generate file paths
file_path <- function(dir, file) {
  if (is.null(dir)) {
    dir <- '.'
  } 
  file.path(dir, file)
}
