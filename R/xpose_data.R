#' Import NONMEM output into R
#'
#' @description Import NONMEM output into a R database
#'
#' @param runno run number to be evaluated
#' @param dir location of the model files
#' @param prefix prefix of the model file name
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
xpose_data <- function(runno       = NULL,
                       dir         = NULL,
                       prefix      = 'run',
                       ext         = '.lst',
                       file        = NULL,
                       rounding    = NULL,
                       gg_theme    = theme_readable(),
                       xp_theme    = xp_theme_default(),
                       verbose     = FALSE) {

  # Check inputs ------------------------------------------------------------
  if (is.null(runno) & is.null(file)) {
    stop('Argument \"runno\" or \"file\" required.', call. = FALSE)
  }

  if (!is.null(file)) {
    file_full <- file
    dir       <- paste0(dirname(file_full), '/')
  } else {

    if (!is.null(dir) && !substr(dir, nchar(dir), nchar(dir)) == '/') {
      dir <- paste0(dir, '/')
    }

    if (!ext %in% paste0('.', c('ctl', 'mod', 'lst', 'txt'))) {
      stop('Argument \"ext\" must be one of: \".ctl\", \".mod\", \".lst\" or \".txt\".', call. = FALSE)
    }

    file_full <- paste0(dir, prefix, runno, ext)
  }

  if (!file.exists(file_full)) {
    stop(paste('file', basename(file_full), 'not found.'), call. = FALSE)
  }


  # Import parsed model -----------------------------------------------------
  mod_file  <- parse_nm_model(file = file_full)


  # Import parsed patab -----------------------------------------------------
  tab_out  <- combine_nmtab(mod_file, dir, verbose)


  # Model file name ---------------------------------------------------------
  mod_name <- gsub('\\.\\w+$', '', basename(file_full))


  # Model info --------------------------------------------------------------
  mod_info  <- list(descr      = descr(mod_file),     # Model description
                    dir        = dir,                 # Model directory
                    run        = mod_name,            # Model file name
                    ref        = NULL,                # Reference model
                    input_dat  = raw_dat(mod_file),   # Model input data used
                    nobs       = nobs(mod_file),      # Number of observations
                    nind       = nind(mod_file),      # Number of individuals
                    nsim       = NULL,                # Number of simulations
                    ssim       = NULL,                # Simulation seed
                    niter      = NULL,                # Number of iteration
                    software   = NULL,                # Software used (e.g. NONMEM)
                    version    = NULL,                # Software version (e.g. 7.3)
                    subroutine = NULL,                # Des solver
                    runtime    = NULL,                # Estimation/Sim runtime
                    covtime    = NULL,                # Covariance matrix runtime
                    warnings   = NULL,                # Run warnings (e.g. boundary)
                    errors     = NULL,                # Run errors (e.g termination error)
                    nsig       = NULL,                # Number of significant digits
                    condition  = NULL,                # Condition number
                    nnpde      = NULL,                # Number of NPDE
                    snpde      = NULL,                # NPDE seed number
                    ofv        = ofv(mod_file),       # Objective function value
                    method     = method(mod_file),    # Estimation method or sim
                    eps_shrink = shrinkage(mod_file, 'EPS', # Epsilon shrinkage
                                           ifelse(is.null(rounding), xp_theme$rounding, rounding)),
                    eta_shrink = shrinkage(mod_file, 'ETA', # Eta shrinkage
                                           ifelse(is.null(rounding), xp_theme$rounding, rounding))
  )

  # Create the qmd_info object ----------------------------------------------
  out <- structure(list(
    code      = mod_file,                               # Model code
    summary   = mod_info,                               # Run summary
    data      = tab_out$data,                           # Output tables
    tab_index = tab_out$index,                          # Index of tab files
    gg_theme  = gg_theme,                               # ggplot theme
    xp_theme  = xp_theme                                # xpose theme
  ), 
  class = c('xpose_data', 'uneval'))

  return(out)

}
