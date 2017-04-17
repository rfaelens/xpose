#' NONMEM output table import function
#'
#' @description Quickly import NONMEM output tables into R.
#' When both \code{skip} and \code{header} are \code{NULL},
#' \code{read_nm_tab} will automatically detect the optimal
#' settings to import the tables. When more than one files are
#' provided for a same NONMEM run, they will be combined into
#' a single \code{data.frame}.
#'
#' @param file Full file name.
#' @param skip Number of lines to skip before reading data.
#' @param header Logical value indicating whether the file has header or not.
#' @param rm_duplicates Logical value indicating whether duplicated columns should be removed.
#' @param nonmem_tab Logical value indicading to the function whether the file is a
#' table or an additional NONMEM output file.
#' @param index Logical, when \code{nonmem_tab} is \code{TRUE} returns a list with the data and an
#' index of all filenames and their colnames.
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
                        nonmem_tab = TRUE,
                        index = FALSE) {
  
  # Check inputs
  if (is.null(file)) {
    stop('Argument \"file\" required.', call. = FALSE)
  }
  
  if (!any(file.exists(file))) {
    stop('File not found.', call. = FALSE)
  } else {
    file <- file[file.exists(file)]
  }
  
  if (nonmem_tab) {
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
    
  } else {
    # if nonmem_tab == FALSE
    # Search for final results only
    skip     <- max(grep('TABLE NO', readLines(file[1])))
    
    # Import all files
    tab_file <- do.call('cbind', lapply(file, utils::read.table, skip = skip,
                                        header = FALSE, fill = TRUE, as.is = TRUE))
    colnames(tab_file) <- tab_file[1, ]
    tab_file <- suppressWarnings(as.data.frame(apply(tab_file[-1, ], 2, as.numeric)))
  }
  
  if (rm_duplicates) {
    tab_file <- tab_file[, !duplicated(colnames(tab_file))]
  }
  
  if (index) {
    tab_file <- list(data  = tab_file, index = index_dat)
  }
  
  return(tab_file)
  
}
