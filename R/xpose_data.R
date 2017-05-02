#' Import NONMEM output into R
#'
#' @description Import NONMEM output into a R database
#'
#' @param file Full file name as an alternative to \code{dir}, \code{prefix}.
#' @param runno Run number to be evaluated.
#' @param dir Location of the model files.
#' @param prefix Prefix of the model file name.
#' @param ext Model file extention.
#' @param rounding Number of significant digits to be used on model info numerical
#' values (eg. OFV, shrinkage).
#' @param gg_theme A ggplot2 complete theme object (eg. \code{theme_classic()}).
#' @param xp_theme A xpose theme or vector of modifications of the xpose theme' \code{runno} and \code{ext}.
#' @param verbose Logical, if \code{TRUE} messages are printed to the console.
#'
#' @examples
#' \dontrun{
#' xpdb <- xpose_data(dir = '../models/pk/', runno = '001')
#' }
#' @import ggplot2
#' @importFrom purrr %>%
#' @export
xpose_data <- function(file        = NULL,
                       runno       = NULL,
                       dir         = NULL,
                       prefix      = 'run',
                       ext         = '.lst',
                       rounding    = NULL,
                       gg_theme    = theme_readable(),
                       xp_theme    = theme_xp_default(),
                       verbose     = TRUE) {

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

  # Import model
  model  <- read_nm_model(file = file_full)

  # Import estimation tables
  msg('Looking for NONMEM table files.', verbose)
  tab_out <- model %>% 
    list_nm_tables() %>%
    #filter(.$simtab == FALSE) %>% 
    #as.nm.table.list() %>% 
    read_nm_tables()

  ####### Temp ################
  # Quick fix before rewriting xpose_data
  data  <- tab_out$data[[1]]
  index <- tab_out$index[[1]]
  #############################
  
  # Import simulation tables
  msg('Looking for NONMEM simulation table files.', verbose)
  
  # Model file name
  mod_name <- gsub('\\.\\w+$', '', basename(file_full))


  # Model info 
  mod_info  <- list(descr      = descr(model),     # Model description
                    dir        = dir,                 # Model directory
                    run        = mod_name,            # Model file name
                    ref        = NULL,                # Reference model
                    input_dat  = raw_dat(model),   # Model input data used
                    nobs       = nobs(model),      # Number of observations
                    nind       = nind(model),      # Number of individuals
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
                    ofv        = ofv(model),       # Objective function value
                    method     = method(model),    # Estimation method or sim
                    eps_shrink = shrinkage(model, 'EPS', # Epsilon shrinkage
                                           ifelse(is.null(rounding), xp_theme$rounding, rounding)),
                    eta_shrink = shrinkage(model, 'ETA', # Eta shrinkage
                                           ifelse(is.null(rounding), xp_theme$rounding, rounding))
  )

  # Create the qmd_info object ----------------------------------------------
  out <- structure(list(
    code      = model,                                  # Model code
    summary   = mod_info,                               # Run summary
    data      = data,                                   # Output tables
    tab_index = index,                                  # Index of tab files
    gg_theme  = gg_theme,                               # ggplot theme
    xp_theme  = xp_theme                                # xpose theme
  ), 
  class = c('xpose_data', 'uneval'))

  return(out)

}
