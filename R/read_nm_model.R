#' NONMEM model file parser
#'
#' @description Parse NONMEM model files in R format
#'
#' @param file Full file name preferably a `.lst` file. Alternative argument to \code{dir}, \code{prefix},
#' \code{runno} and \code{ext}.
#' @param runno Run number to be evaluated.
#' @param dir Location of the model file.
#' @param prefix Prefix of the model file name.
#' @param ext Extension of the model file.
#' @param verbose Logical, if \code{TRUE} messages are printed to the console.
#'
#' @seealso \code{\link{xpose_data}}, \code{\link{read_nm_tables}}
#' @return A \code{\link[dplyr]{tibble}} of class \code{model} containing the following columns: 
#' \itemize{
##'  \item{\strong{problem}}{: a numeric identifier for the $PROBLEM associated with the code.}
##'  \item{\strong{level}}{: a unique numeric identifier to each subroutine block associated with the code.}
##'  \item{\strong{subroutine}}{: a character identifier named after the 3 first letters of the subroutine name e.g. "$THETA" and 
##'  "$TABLE" will become "the" and "tab" respectively. In addtion all output from the .lst is labeled "lst", the general nonmem 
##'  output e.g. NM-TRAN messages are labeled "oth". With priors thp, tpv, omp, opd, sip, spd abreviations are given to the THETAP, 
##'  etc.}
##'  \item{\strong{code}}{: the code without comments or subroutine names e.g. "$THETA 0.5 ; TVCL" will return 0.5.}
##'  \item{\strong{comment}}{: the last comment of a record e.g. "0.5 ; Clearance (L/h) ; TVCL" will return "TVCL".}
##' }
#' @examples
#' \dontrun{
#' nm_model <- read_nm_model(dir = '../models/pk/', runno = '001')
#' }
#' @export
read_nm_model <- function(file    = NULL,
                          runno   = NULL,
                          dir     = NULL,
                          prefix  = 'run',
                          ext     = c('.lst', '.mod', '.ctl', '.txt'),
                          verbose = TRUE) {
  
  if (is.null(runno) && is.null(file)) {
    stop('Argument `runno` or `file` required.', call. = FALSE)
  }
  
  if (is.null(file)) {
    ext  <- match.arg(ext)
    file <- file_path(dir, paste0(prefix, runno, ext))
  }
  
  if (!file.exists(file)) { 
    stop('File ', basename(file), ' not found.', call. = FALSE) 
  }
  
  model <- readr::read_lines(file)
  
  if (!any(stringr::str_detect(model, '^\\s*\\$PROB.+')) && get_extension(file) == '.lst') {
    # Attempt to recover the model in .mod if misssing from .lst
    file <- update_extension(file, '.mod')
    
    if (file.exists(file)) {
      msg('No model code was found in `.lst` file using `.mod` instead.', verbose)
      model <- readr::read_lines(file)
    }
  }
  
  # Return error if input is bad
  if (!any(stringr::str_detect(model, '^\\s*\\$PROB.+'))) {
    stop(basename(file), ' is not a NONMEM model.', call. = FALSE)
  }
  
  model <- dplyr::tibble(code = model) %>% 
    dplyr::filter(!stringr::str_detect(.$code, '^;[^;]*$|^$')) %>% 
    dplyr::mutate(code = stringr::str_replace_all(.$code, '\\t+|\\s{2,}', ' ')) %>% 
    dplyr::mutate(
      problem     = findInterval(seq_along(.$code), which(stringr::str_detect(.$code, '^\\s*\\$PROB.+'))),
      level       = findInterval(seq_along(.$code), which(stringr::str_detect(.$code, '^\\s*\\$.+'))),
      subroutine  = stringr::str_match(.$code, '^\\s*\\$(\\w+)')[, 2]) %>% 
    tidyr::fill(dplyr::one_of('subroutine'))
  
  # Generate abreviated subroutine names
  special <- c('THETAI', 'THETAR', 'THETAP', 'THETAPV', 'OMEGAP', 'OMEGAPD', 'SIGMAP', 'SIGMAPD')
  match_special <- match(model$subroutine[model$subroutine %in% special], special)
  
  model$subroutine[model$subroutine %in% special] <- c('thi', 'thr', 'thp', 'tpv', 
                                                       'omp', 'opd', 'sip', 'spd')[match_special]
  model$subroutine <- stringr::str_extract(tolower(model$subroutine), '[a-z]{1,3}')
  
  # Format lst part
  if (any(stringr::str_detect(model$code, 'NM-TRAN MESSAGES'))) {
    lst_rows <- which(stringr::str_detect(model$code, 'NM-TRAN MESSAGES')):nrow(model)
    model[lst_rows, 'problem'] <- findInterval(seq_along(lst_rows), 
                                               which(stringr::str_detect(model[lst_rows, ]$code, 
                                                                         '^\\s*PROBLEM NO\\.:\\s*\\d+$')))
    model[lst_rows, 'level'] <- model[lst_rows[1], ]$level + 1 + model[lst_rows, ]$problem
    model[lst_rows, 'subroutine']  <- 'lst'
  }
  
  # Handle other special cases
  if (any(stringr::str_detect(model$code, '#CPUT'))) {
    cput_row <- which(stringr::str_detect(model$code, '#CPUT'))
    model[cput_row, 'problem'] <- 0
    model[cput_row:nrow(model), 'level'] <- model[cput_row:nrow(model), ]$level + 1
  }
  
  if (any(stringr::str_detect(model$code, 'Stop Time'))) {
    end_rows <- which(stringr::str_detect(model$code, 'Stop Time')):nrow(model)
    model[end_rows, 'problem'] <- 0
    model[end_rows, 'level'] <- model[end_rows[1], ]$level + 1
  }
  
  model[is.na(model$subroutine) | (model$problem == 0 & model$subroutine == 'lst'), 'subroutine'] <- 'oth'
  
  # Remove the subroutine names from the code
  model$code <- stringr::str_replace(model$code, '^\\s*\\$\\w+\\s*', '')
  
  # Remove empty code lines
  model <- model[!stringr::str_detect(model$code, '^(\\s|\\t)*$') | model$subroutine == 'pro', ]
  
  # Extract comments
  code_rows <- !model$subroutine %in% c('lst', 'oth')
  model[code_rows, 'comment'] <- stringr::str_match(model[code_rows, ]$code, ';\\s*([^;]*)$')[, 2]
  model[code_rows, 'code'] <- stringr::str_replace(model[code_rows, ]$code, '\\s*;.*$', '')
  
  # Remove na values and output
  tidyr::replace_na(model, replace = list(code = '', comment = '')) %>% 
    structure(file     = basename(file),
              dir      = dirname(file),
              software = 'nonmem',
              class    = c('nm_model', 'tbl_df', 'tbl', 'data.frame'))
}
