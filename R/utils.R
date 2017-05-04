# Reports whether x is an xpose theme object
is.xp.theme <- function(x) {
  inherits(x, 'xpose_theme')
}

# Reports whether x is an xpose data object
is.xpdb <- function(x) {
  inherits(x, 'xpose_data')
}

# Reports whether x is a nm_model object
is.nm.model <- function(x) {
  inherits(x, 'nm_model')
}

# Reports whether x is a nm.table.list object
is.nm.table.list <- function(x) {
  inherits(x, 'nm_table_list')
}

# Adds nm_table_list attribute to an object
as.nm.table.list <- function(x) {
  structure(x, class = c('nm_table_list', class(x)))
}

# Reports whether x is an xpose_plot object
is.xpose.plot <- function(x) {
  inherits(x, 'xpose_plot')
}

# Check whether x is a formula
is.formula <- function(x) {
  inherits(x, 'formula')
}

# Message function with quiet option (from Ron Keizer)
msg <- function(txt, quiet = TRUE) {
  if (!quiet) message(txt)
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

#' @importFrom purrr %>%
#' @export
purrr::`%>%`
