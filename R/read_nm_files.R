#' NONMEM output file import function
#'
#' @description Quickly import NONMEM output files into R.
#'
#' @param files Full model file names. Alternative argument to \code{dir}, \code{prefix},
#' \code{runno} and \code{ext}.
#' @param runno Run number to be evaluated.
#' @param dir Location of the model files.
#' @param prefix Prefix of the model file names.
#' @param ext A vector of the file extension to import. By default '.ext', '.cor', '.cov', '.phi' and '.grd' 
#' files are listed.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#'
#' @seealso \code{\link{xpose_data}}, \code{\link{read_nm_tables}}
#' @examples
#' \dontrun{
#' ext_file <- read_nm_files(files = 'run001.ext')
#' }
#' @export

read_nm_files <- function(files  = NULL,
                          runno  = NULL,
                          dir    = NULL,
                          prefix = 'run',
                          ext    = c('.ext', '.cor', '.cov', '.phi', '.grd'),
                          quiet  = FALSE) {
  
  if (is.null(runno) && is.null(files)) {
    stop('Argument `runno` or `file` required.', call. = FALSE)
  }
  
  if (is.null(files)) {
    files <- file_path(dir, paste0(prefix, runno, ext))
  }
  
  files <- sort(unique(files))
  bases <- basename(files)
  
  if (!any(file.exists(files))) {
    msg('No output file could be found.', quiet)
    return()
  } else if (any(file.exists(files))) {
    msg(c('Reading: ', paste0(bases[file.exists(files)], collapse = ', ')), quiet)
  }
  
  out <- files %>% 
    dplyr::tibble(path = ., name = basename(.)) %>% 
    dplyr::filter(file.exists(.$path)) %>% 
    purrr::by_row(~readr::read_lines(file = .$path), .to = 'raw') %>%
    purrr::by_row(~parse_nm_files(dat = ., quiet), .to = 'tmp')
  
  out <- out %>% 
    dplyr::bind_cols(dplyr::tibble(drop = purrr::map_lgl(out$tmp, purrr::is_null)))
  
  if (all(out$drop)) {
    msg('\nNo output file imported.', quiet)
    return()
  }  
  
  out %>% 
    dplyr::filter(!.$drop) %>% 
    tidyr::unnest(quote(tmp)) %>% 
    dplyr::select(dplyr::one_of('name', 'prob', 'subprob', 'method', 'data'))
}

parse_nm_files <- function(dat, quiet) {
  x <- dplyr::tibble(raw = unlist(dat$raw), prob = NA, subprob = NA, method = NA, header = FALSE)
  tab_rows <- which(stringr::str_detect(x$raw, '^\\s*TABLE NO'))
  
  if (length(tab_rows) == 0) {
    msg(c('Dropping ', dat$name, ' due to inappropriate format.'), quiet)
    return()
  }
  
  x[tab_rows, ]$prob <- stringr::str_match(x[tab_rows, ]$raw, '\\s+Problem=(\\d+)')[,2]
  x[tab_rows, ]$subprob <- stringr::str_match(x[tab_rows, ]$raw, '\\s+Subproblem=(\\d+)')[,2]
  x[tab_rows, ]$method  <- dplyr::case_when(stringr::str_detect(x[tab_rows, ]$raw, 'First Order Conditional') ~ 'FOCE',
                                            stringr::str_detect(x[tab_rows, ]$raw, 'Laplace Conditional') ~ 'LCE', 
                                            stringr::str_detect(x[tab_rows, ]$raw, 'Iterative Two Stage') ~ 'ITS',
                                            stringr::str_detect(x[tab_rows, ]$raw, 'Importance Sampling') ~ 'IMP',
                                            stringr::str_detect(x[tab_rows, ]$raw, 'Stochastic Approximation') ~ 'SAEM',
                                            TRUE ~ 'BAYES')
  
  # Assumes that header are always present
  x[tab_rows + 1, ]$header <- TRUE
  
  # Guess column separator from the first problem only
  sep <- dplyr::case_when(
    stringr::str_detect(x[tab_rows[1] + 1, ]$raw, ';[A-z]+') ~ ';[A-z]|[A-z];',
    stringr::str_detect(x[tab_rows[1] + 1, ]$raw, ',[A-z]+') ~ ',[A-z]|[A-z],',
    TRUE ~ '\\s+')
  
  x %>% 
    tidyr::fill(dplyr::one_of('prob', 'subprob', 'method')) %>% 
    dplyr::slice(-tab_rows) %>%
    dplyr::mutate(raw = stringr::str_trim(.$raw, side = 'both')) %>% 
    purrr::slice_rows(c('prob', 'subprob', 'method')) %>% 
    purrr::by_slice(raw_to_tibble, sep, quiet, file = dat$name, .to = 'data')
}  

raw_to_tibble <- function(x, sep, quiet, file) {
  header <- x$raw[x$header] %>% 
    stringr::str_split(pattern = sep) %>% 
    purrr::flatten_chr()
  
  if (any(is.na(header))) {
    msg(c('Issue encountered while parsing ', file), quiet)
    return()
  }
  
  x[!x$header, ] %>%   
    tidyr::separate(col  = 'raw', sep  = sep, into = header) %>% 
    dplyr::select(-ncol(.))
}
