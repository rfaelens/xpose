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
  string <- gsub('[^\\d\\.-]+', '', string, perl = TRUE)
  string <- paste('OBJ:', string, collapse = '\n')
  return(string)
}
