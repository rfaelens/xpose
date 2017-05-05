#' NONMEM file parser
#'
#' @description Parse NONMEM files in R format
#'
#' @param file Full file name, with either .ext, .phi. .grd, .cor or .cov extension. Alternative argument to \code{dir}, \code{prefix},
#' \code{runno} and \code{suff}.
#' @param runno Run number to be evaluated.
#' @param dir Location of the model file.
#' @param prefix Prefix of the model file name.
#' @param suff Extension of the model file.
#' @param verbose Logical, if \code{TRUE} messages are printed to the console.
#'
#' @seealso \code{\link{xpose_data}}, \code{\link{read_nm_tables}}
#' @return A list of \code{\link[dplyr]{tibble}} tables of class \code{} containing the following columns: 
#' \itemize{
##'  \item{\strong{ext}}{: The .ext file}
##'  \item{\strong{phi}}{: The .phi file}
##'  \item{\strong{grd}}{: The .grd file}
##'  \item{\strong{cov}}{: The .cov file.}
##'  \item{\strong{cor}}{: The .cor file.}
##' }
#' @examples
#' \dontrun{
#' nm_file <- read_nm_model(dir = '../models/pk/', runno = '001')
#' }
#' @export

read_nm_files <- function(file    = NULL,
                         runno   = NULL,
                         dir     = NULL,
                         prefix  = 'run',
                         suff     = c('.ext', '.cor', '.cov', '.phi', '.grd'),
                         verbose = TRUE) {
  
  if (is.null(runno) && is.null(file)) {
    stop('Argument `runno` or `file` required.', call. = FALSE)
  }
  
  if (is.null(file)) {
    suff  <- match.arg(suff)
    file <- file_path(dir, paste0(prefix, runno, suff))
  }
  
  if (!file.exists(file)) { 
    warning('File ', basename(file), ' not found.', call. = FALSE) 
  }
  
  if (!file.exists(paste(dir,prefix,runno,'.ext',sep=""))) { 
    warning('File ', paste(dir,prefix,runno,'.ext',sep=""), ' not found.', call. = FALSE) 
    ext <- NULL
  }else{
    ext <- read_nm_file(paste(dir,prefix,runno,'.ext',sep=""), 'ITERATION', '.ext')
  }
  
  if (!file.exists(paste(dir,prefix,runno,'.phi',sep=""))) { 
    warning('File ', paste(dir,prefix,runno,'.phi',sep=""), ' not found.', call. = FALSE) 
    phi <- NULL
  }else{
    phi <- read_nm_file(paste(dir,prefix,runno,'.phi',sep=""), 'SUBJECT_NO', '.phi')
  }
  
  if (!file.exists(paste(dir,prefix,runno,'.grd',sep=""))) { 
    warning('File ', paste(dir,prefix,runno,'.grd',sep=""), ' not found.', call. = FALSE) 
    grd <- NULL
  }else{
    grd <- read_nm_file(paste(dir,prefix,runno,'.grd',sep=""), 'ITERATION', '.ext')
  }
  
  if (!file.exists(paste(dir,prefix,runno,'.cov',sep=""))) { 
    warning('File ',paste(dir,prefix,runno,'.cov',sep=""), ' not found.', call. = FALSE) 
    cov1 <- NULL
  }else{
    cov1 <- read_nm_file(paste(dir,prefix,runno,'.cov',sep=""), 'NAME', '.cor')
  }
  
  if (!file.exists(paste(dir,prefix,runno,'.cor',sep=""))) { 
    warning('File ', paste(dir,prefix,runno,'.cor',sep=""), ' not found.', call. = FALSE) 
    cor1 <- NULL
  }else{
    cor1 <- read_nm_file(paste(dir,prefix,runno,'.cor',sep=""), 'NAME', '.cov')
  }
  
  
  # As list
  nm_files <- list(ext= ext, phi= phi,grd= grd, cov=cov1,cor=cor1) 
  
  
  nm_files
  
}
