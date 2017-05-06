#' Import NONMEM output into R
#'
#' @description Gather model outputs into a R database
#'
#' @param file Full file name preferably a `.lst` file. Alternative argument to \code{dir}, \code{prefix},
#' \code{runno} and \code{ext}.
#' @param runno Run number to be evaluated.
#' @param dir Location of the model files.
#' @param prefix Prefix of the model file name.
#' @param ext Extension of the model file.
#' @param gg_theme A ggplot2 theme object (eg. \code{\link[ggplot2]{theme_classic}}).
#' @param xp_theme An xpose theme or vector of modifications to the xpose theme
#' (eg. \code{c(point_color = 'red', line_linetype = 'dashed')}).
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' @param ... Additional arguments to be passed to the \code{\link[readr]{read_delim}} functions.
#'
#' @examples
#' \dontrun{
#' xpdb <- xpose_data(dir = '../models/pk/', runno = '001')
#' }
#' @export
xpose_data <- function(file     = NULL,
                       runno    = NULL,
                       dir      = NULL,
                       prefix   = 'run',
                       ext      = '.lst',
                       gg_theme = theme_readable(),
                       xp_theme = theme_xp_default(),
                       quiet    = FALSE,
                       ...) {
  
  if (is.null(runno) && is.null(file)) {
    stop('Argument `runno` or `file` required.', call. = FALSE)
  }
  
  if (is.null(file)) {
    file <- file_path(dir, paste0(prefix, runno, ext))
  }
  
  if (ext %in% c('.lst', '.mod', '.ctl', '.txt')) {
    software <- 'nonmem'
    model_code <- read_nm_model(file, runno, dir, prefix, ext, quiet)
    tbl_names  <- list_nm_tables(model_code)
  } else {
    stop('Model file currently unsupported by xpose.', call. = FALSE)
  }  
  
  # Import estimation tables
  msg(c('Looking for ', software, ' table files'), quiet)
  if (software == 'nonmem') {
    data <- tbl_names %>%
      dplyr::filter(.$simtab == FALSE) %>% 
      as.nm.table.list() %>% 
      read_nm_tables(..., quiet = quiet)
  }
  
  # Import simulation tables
  msg(c('\nLooking for ', software, ' simulation files'), quiet)
  if (software == 'nonmem') {
    sim <- tbl_names %>%
      dplyr::filter(.$simtab == TRUE) %>% 
      as.nm.table.list() %>% 
      read_nm_tables(..., quiet = quiet)
  }
  
  # Generate model summary
  if (software == 'nonmem') {
  summary <- summarise_nm_model(file, model_code, software, rounding = xp_theme$rounding)
  }
  
  # Import output files
  msg(c('\nLooking for ', software, ' output files'), quiet)
  if (software == 'nonmem') {
  out_files <- update_extension(file, '') %>% 
    paste0(c('.ext', '.cor', '.cov', '.phi', '.grd')) %>% 
    read_nm_files(quiet = quiet)  
  }
  
  # Output xpose_data
  structure(list(code = model_code, summary = summary, files = out_files, 
                 data = data, sim = sim, gg_theme = gg_theme,
                 xp_theme = xp_theme, options = list(quiet = quiet)), 
            class = c('xpose_data', 'uneval'))
}
