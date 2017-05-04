summarise_nm_model <- function(file, model_code, rounding) {
  list(descr      = descr(model_code),                      # Model description
       file       = basename(file),                         # Model file
       run        = update_extension(basename(file), ''),   # Model file name
       dir        = dirname(file),                          # Model directory
       ref        = NULL,                                   # Reference model
       inpdat     = raw_dat(model_code),                    # Model input data used
       nobs       = nobs(model_code),                       # Number of observations
       nind       = nind(model_code),                       # Number of individuals
       nsim       = NULL,                                   # Number of simulations
       ssim       = NULL,                                   # Simulation seed
       niter      = NULL,                                   # Number of iteration
       software   = NULL,                                   # Software used (e.g. NONMEM)
       version    = NULL,                                   # Software version (e.g. 7.3)
       subroutine = NULL,                                   # Des solver
       runtime    = NULL,                                   # Estimation/Sim runtime
       covtime    = NULL,                                   # Covariance matrix runtime
       warnings   = NULL,                                   # Run warnings (e.g. boundary)
       errors     = NULL,                                   # Run errors (e.g termination error)
       nsig       = NULL,                                   # Number of significant digits
       condition  = NULL,                                   # Condition number
       nnpde      = NULL,                                   # Number of NPDE
       snpde      = NULL,                                   # NPDE seed number
       ofv        = ofv(model_code),                        # Objective function value
       method     = method(model_code),                     # Estimation method or sim
       epsshr     = shrinkage(model_code, 'EPS', rounding), # Epsilon shrinkage
       etashr     = shrinkage(model_code, 'ETA', rounding)  # Eta shrinkage
  )
}

# Needs to handle MIXTURE + Multiple $PROBLEMS
shrinkage <- function(model, type, rounding) {
  string <- model$code[which.max(grepl(paste0(type, 'shrink'), model$code))]
  string <- suppressWarnings(as.numeric(unlist(strsplit(string, '(\\s+)'))))
  string <- round(string[!is.na(string)], rounding)
  names(string) <- seq_along(string)
  string <- string[string != 100]
  string <- paste0(string, ' % [', names(string),']', collapse = ', ')
  paste(type, 'shrink:', string)
}

descr <- function(model) {
  string <- model$code[model$subroutine == 'pro']
  paste(string, collapse = ', ')
}

ofv <- function(model) {
  string <- model$code[which.max(grepl('#OBJV', model$code))]
  if (!is.null(string)) {
    string <- gsub('[^\\d\\.-]+', '', string, perl = TRUE)
    string <- paste(string, collapse = ', ')
  }
  return(string)
}

raw_dat <- function(model) {
  gsub('\\s+.*$', '', model$code[model$subroutine == 'dat'][1])
}

nobs <- function(model) {
  gsub('\\D', '', model$code[grepl('TOT. NO. OF OBS RECS', model$code)])
}

nind <- function(model) {
  gsub('\\D', '', model$code[grepl('TOT. NO. OF INDIVIDUALS', model$code)])
}

method <- function(model) {
  string <- gsub('.*METHOD=(\\w+)\\s+.*', '\\1', model$code[grepl('METHOD=', model$code)])
  inter  <- ifelse(grepl('INTER', model$code[grepl('METHOD=', model$code)]), '-I', '')
  lapl   <- ifelse(grepl('LAPL', model$code[grepl('METHOD=', model$code)]), ' Laplace', '')
  
  if (any(grepl('\\d', string))) {
    string[grepl('\\d', string)] <- c('FO', 'FOCE')[as.numeric(string[grepl('\\d', string)]) + 1]
  }
  string <- paste0(string, inter, lapl)
  paste(string, collapse = ', ')
}

