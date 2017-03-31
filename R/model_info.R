# Needs to handle MIXTURE + Multiple $PROBLEMS
shrinkage <- function(model, type, rounding) {
  string <- model$CODE[which.max(grepl(paste0(type, 'shrink'), model$CODE))]
  string <- suppressWarnings(as.numeric(unlist(strsplit(string, '(\\s+)'))))
  string <- round(string[!is.na(string)], rounding)
  names(string) <- seq_along(string)
  string <- string[string != 100]
  string <- paste0(string, ' % [', names(string),']', collapse = ', ')
  paste(type, 'shrink:', string)
}


descr <- function(model) {
  string <- model$CODE[model$ABREV == 'PRO']
  paste(string, collapse = ', ')
}


ofv <- function(model) {
  string <- model$CODE[which.max(grepl('#OBJV', model$CODE))]
  if (!is.null(string)) {
    string <- gsub('[^\\d\\.-]+', '', string, perl = TRUE)
    string <- paste(string, collapse = ', ')
  }
  return(string)
}


raw_dat <- function(model) {
  gsub('\\s+.*$', '', model$CODE[model$ABREV == 'DAT'][1])
}


nobs <- function(model) {
  gsub('\\D', '', model$CODE[grepl('TOT. NO. OF OBS RECS', model$CODE)])
}


nind <- function(model) {
  gsub('\\D', '', model$CODE[grepl('TOT. NO. OF INDIVIDUALS', model$CODE)])
}


method <- function(model) {
  string <- gsub('.*METHOD=(\\w+)\\s+.*', '\\1', model$CODE[grepl('METHOD=', model$CODE)])
  inter  <- ifelse(grepl('INTER', model$CODE[grepl('METHOD=', model$CODE)]), '-I', '')
  lapl   <- ifelse(grepl('LAPL', model$CODE[grepl('METHOD=', model$CODE)]), ' Laplace', '')
  
  if (any(grepl('\\d', string))) {
    string[grepl('\\d', string)] <- c('FO', 'FOCE')[as.numeric(string[grepl('\\d', string)]) + 1]
  }
  string <- paste0(string, inter, lapl)
  paste(string, collapse = ', ')
}

