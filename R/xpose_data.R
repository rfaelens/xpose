#' Import NONMEM output into R
#'
#' @description Import NONMEM output into a R database
#'
#' @param dir location of the model files
#' @param prefix prefix of the model file name
#' @param runno run number to be evaluated
#' @param ext model file extention
#' @param file full file name as an alternative to \code{dir}, \code{prefix},
#' @param rounding number of significant digits to be used on model info numerical
#' values (eg. OFV, shrinkage).
#' @param gg_theme a ggplot2 complete theme object (eg. \code{theme_classic()})
#' @param xp_theme a ggxpose theme or vector of modifications of the ggxpose theme#' \code{runno} and \code{ext}
#' @param verbose if \code{TRUE} messages are printed to the console
#'
#' @examples
#' \dontrun{
#' xpdb <- xpose_data(dir = '../models/pk/', runno = '001')
#' }
#' @export
xpose_data <- function(dir         = NULL,
                       prefix      = 'run',
                       runno       = NULL,
                       ext         = '.lst',
                       file        = NULL,
                       rounding    = NULL,
                       gg_theme    = theme_readable(),
                       xp_theme    = xp_theme_default,
                       verbose     = FALSE) {

  # Check inputs ------------------------------------------------------------
  if (is.null(runno) & is.null(file)) {
    stop('Argument \"runno\" or \"file\" required.')
  }

  if (!is.null(file)) {
    file_full <- file
    dir       <- paste0(dirname(file_full), '/')
  } else {

    if (!is.null(dir) && !substr(dir, nchar(dir), nchar(dir)) == '/') {
      dir <- paste0(dir, '/')
    }

    if (!ext %in% paste0('.', c('ctl', 'mod', 'lst', 'txt'))) {
      stop('Argument \"ext\" must be one of: \".ctl\", \".mod\", \".lst\" or \".txt\".')
    }

    file_full <- paste0(dir, prefix, runno, ext)
  }

  if (!file.exists(file_full)) {
    stop(paste('file', basename(file_full), 'not found.'))
  }


  # Import parsed model -----------------------------------------------------
  mod_file  <- parse_nm_model(file = file_full)


  # Import parsed patab -----------------------------------------------------
  tab_file  <- combine_nmtab(mod_file, dir, verbose)


  # Model info --------------------------------------------------------------
  mod_info  <- list(eps_shrink = shrinkage(mod_file, 'EPS',
                                           ifelse(is.null(rounding), xp_theme$rounding, rounding)),
                    eta_shrink = shrinkage(mod_file, 'ETA',
                                           ifelse(is.null(rounding), xp_theme$rounding, rounding)),
                    ofv        = ofv(model = mod_file),
                    run        = paste0(prefix, runno),
                    input_dat  = raw_dat(mod_file),
                    nobs       = n_oi(mod_file),
                    method     = m_est(mod_file)
  )




  # Model file name ---------------------------------------------------------
  mod_name <- gsub('\\.\\w+$', '', basename(file_full))

  # Create the qmd_info object ----------------------------------------------
  out <- structure(list(
    modfile  = mod_name,                               # modelfile
    descr    = mod_file$CODE[mod_file$ABREV == 'PRO'], # Model description
    mod      = mod_file,                               # Raw model file
    mod_info = mod_info,                               # Parsed model information
    data     = tab_file,                               # Output tables
    gg_theme = gg_theme,                               # ggplot theme
    xp_theme = xp_theme                                # xpose theme
  ), class = c('xpose_data', 'uneval'))

  return(out)

}
