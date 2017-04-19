#' NONMEM output table import function
#'
#' @description Quickly import NONMEM output tables into R.
#' When both \code{skip} and \code{header} are \code{NULL},
#' \code{read_nm_tab} will automatically detect the optimal
#' settings to import the tables. When more than one files are
#' provided for a same NONMEM run, they will be combined into
#' a single \code{data.frame}.
#'
#' @param file Path to the file.
#' @param skip Number of lines to skip before reading data.
#' @param header Logical value indicating whether the file has a header row or not.
#' @param rm_duplicates Logical value indicating whether duplicated columns should be removed.
#' @param index Logical value indiacating whether the data should be returned as a simple data.frame or 
#' a list containing the data and an index of the colum names.
#'
#' @examples
#' \dontrun{
#' data <- read_nm_tab(file = '../models/pk/sdtab101')
#' }
#' @export
read_nm_tab <- function(file = NULL,
                        skip = NULL,
                        header = NULL,
                        rm_duplicates = FALSE,
                        index = FALSE) {
  
  if (is.null(file)) {
    stop('Argument `file` required.', call. = FALSE)
  }
  
  if (!any(file.exists(file))) {
    stop('File not found.', call. = FALSE)
  } else {
    file <- file[file.exists(file)]
  }
  
  # If auto mode required
  if (is.null(skip) & is.null(header)) {
    test    <- readLines(file[1], n = 3)
    skip    <- ifelse(grepl('TABLE NO', test[1]), 1, 0)
    header  <- ifelse(grepl('[a-zA-Z]', test[2]), TRUE, FALSE)
  }
  
  # Import data
  raw   <- lapply(file, readr::read_table, skip = skip, 
                  col_names = header, col_types = readr::cols())
  
  # Index datasets
  if (index) {
    index_dat <- lapply(raw, colnames)
    names(index_dat) <- basename(file)
  }
  
  # Compact data
  tab_file <- do.call('cbind', raw)
  tab_file <- as.data.frame(apply(tab_file, MARGIN = 2, FUN = as.numeric))
  
  # Drop rows with NA (in simtab)
  tab_file <- stats::na.omit(tab_file)
  
  if (rm_duplicates) {
    tab_file <- tab_file[, !duplicated(colnames(tab_file))]
  }
  
  if (index) {
    tab_file <- list(data  = tab_file, index = index_dat)
  }
  
  return(tab_file)
  
}
