# Needs to handle MIXTURE + Multiple $PROBLEMS
shrinkage <- function(model, type, rounding) {

  string <- model$CODE[which.max(grepl(paste0(type, 'shrink'), model$CODE))]
  string <- suppressWarnings(as.numeric(unlist(strsplit(string, '(\\s+)'))))
  string <- round(string[!is.na(string)], rounding)
  names(string) <- seq_along(string)
  string <- string[string != 100]
  string <- paste0(string, ' % [', names(string),']', collapse = ', ')
  string <- paste(type, 'shrink:', string)
  return(string)
}

ofv <- function(model) {
  string <- model$CODE[which.max(grepl('#OBJV', model$CODE))]
  if (!is.null(string)) {
    string <- gsub('[^\\d\\.-]+', '', string, perl = TRUE)
    string <- paste('OBJ:', string, collapse = '\n')
  }
  return(string)
}

raw_dat <- function(model) {
  string <- gsub('\\s+.*$', '', model$CODE[model$ABREV == 'DAT'][1])
  string <- paste('Data:' , string)
  return(string)
}

n_oi <- function(model) {
  nobs <- gsub('\\D', '',
               model$CODE[grepl('TOT. NO. OF OBS RECS', model$CODE)])
  nind <- gsub('\\D', '',
               model$CODE[grepl('TOT. NO. OF INDIVIDUALS', model$CODE)])
  string <- paste(nobs, 'recs. from' , nind, 'ind.')
  return(string)
}

m_est <- function(model) {
  string <- gsub('.*METHOD=(\\w+)\\s+.*', '\\1', model$CODE[grepl('METHOD=', model$CODE)])
  inter  <- ifelse(grepl('INTER', model$CODE[grepl('METHOD=', model$CODE)]), '-I', '')

  if (any(grepl('\\d', string))) {
    string[grepl('\\d', string)] <- c('FO', 'FOCE')[as.numeric(string[grepl('\\d', string)]) + 1]
  }
  string <- paste0(string, inter)
  string <- paste('Method:', paste(string, collapse = ', '))
  return(string)
}
