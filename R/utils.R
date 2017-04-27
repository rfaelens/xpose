# Reports whether x is an xpose theme object
is.xp.theme <- function(x) {
  inherits(x, 'xpose_theme')
}

# Reports whether x is an xpose data object
is.xpdb <- function(x) {
  inherits(x, 'xpose_data')
}

# Reports whether x is a model_file object
is.model.file <- function(x) {
  inherits(x, 'mod_file')
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
  if (is.null(dir)) return(file) 
  
  # Remove trailing forward slash
  dir <- stringr::str_replace(dir, '\\/+$', '')
  file.path(dir, file)
}

# Get file extension
get_extension <- function(x) {
  tmp <- stringr::str_extract(x, '\\.[[:alnum:]]+$')
  tmp[is.na(tmp)] <- ''
  tmp
}

# Update file extension
update_extension <- function(x, ext) {
  stringr::str_replace(x, '\\.[[:alnum:]]+$', ext)
}
