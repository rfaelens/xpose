#' NONMEM output file import function
#'
#' @description Quickly import NONMEM output files into R.
#'
#' @param files Full model file names. Alternative argument to \code{dir}, \code{prefix},
#' \code{runno} and \code{ext}.
#' @param runno Run number to be evaluated.
#' @param dir Location of the model files.
#' @param prefix Prefix of the model file names.
#' @param ext A vector of the file extension to import. By default '.ext', '.cor', '.cov', '.phi', '.grd', '.shk'
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
                          ext    = c('.ext', '.cor', '.cov', '.phi', '.grd', '.shk'),
                          quiet  = FALSE) {
  
  if (is.null(runno) && is.null(files)) {
    stop('Argument `runno` or `file` required.', call. = FALSE)
  }
  
  if (is.null(files)) {
    files <- file_path(dir, paste0(prefix, runno, make_extension(ext)))
  }
  
  files <- sort(unique(files))
  bases <- basename(files)
  
  msg('\nLooking for nonmem output files', quiet)
  
  if (!any(file.exists(files))) {
    msg('No output file could be found.', quiet)
    return()
  } else if (any(file.exists(files))) {
    msg(c('Reading: ', stringr::str_c(bases[file.exists(files)], collapse = ', ')), quiet)
  }
  
  out <- files %>% 
    dplyr::tibble(path = ., name = basename(.)) %>% 
    dplyr::filter(file.exists(.$path)) %>% 
    dplyr::mutate(grouping = 1:n(),
                  raw = purrr::map(.$path, ~readr::read_lines(file = .))) %>% 
    dplyr::group_by_(.dots = 'grouping') %>% 
    tidyr::nest() %>% 
    dplyr::mutate(tmp = purrr::map(.$data, ~parse_nm_files(dat = ., quiet))) %>% 
    dplyr::mutate(drop = purrr::map_lgl(.$tmp, is.null)) 
  
  if (all(out$drop)) {
    msg('\nNo output file imported.', quiet)
    return()
  }  
  
  out %>% 
    dplyr::filter(!.$drop) %>% 
    tidyr::unnest_(unnest_cols = 'data') %>% 
    tidyr::unnest_(unnest_cols = 'tmp') %>% 
    dplyr::mutate(extension = get_extension(.$name, dot = FALSE)) %>% 
    dplyr::select(dplyr::one_of('name', 'extension', 'problem', 'subprob', 'method', 'data'))
}

parse_nm_files <- function(dat, quiet) {
  if (length(unlist(dat$raw)) == 0) {
    tab_rows <- NULL 
  } else {
    x <- dplyr::tibble(raw = unlist(dat$raw), problem = NA, subprob = NA, method = NA, header = FALSE)
    tab_rows <- which(stringr::str_detect(x$raw, '^\\s*TABLE NO'))
  }
  
  if (length(tab_rows) == 0) {
    msg(c('Dropping ', dat$name, ' due to inappropriate format.'), quiet)
    return()
  }
  
  x[tab_rows, ]$problem <- stringr::str_match(x[tab_rows, ]$raw, '\\s+Problem=(\\d+)')[,2]
  x[tab_rows, ]$subprob <- stringr::str_match(x[tab_rows, ]$raw, '\\s+Subproblem=(\\d+)')[,2]
  x[tab_rows, ]$method  <- dplyr::case_when(stringr::str_detect(x[tab_rows, ]$raw, 'First Order Conditional') ~ 'foce',
                                            stringr::str_detect(x[tab_rows, ]$raw, 'Laplacian Conditional') ~ 'lce', 
                                            stringr::str_detect(x[tab_rows, ]$raw, 'Iterative Two Stage') ~ 'its',
                                            stringr::str_detect(x[tab_rows, ]$raw, 'Importance Sampling') ~ 'imp',
                                            stringr::str_detect(x[tab_rows, ]$raw, 'Stochastic Approximation') ~ 'saem',
                                            stringr::str_detect(x[tab_rows, ]$raw, 'Markov-Chain') ~ 'bayes',
                                            TRUE ~ 'na')
  
  # Assumes that header are always present
  x[tab_rows + 1, ]$header <- TRUE
  
  # Guess column separator from the first problem only
  sep <- dplyr::case_when(
    stringr::str_detect(x[tab_rows[1] + 1, ]$raw, ';[A-z]+') ~ ';[A-z]|[A-z];',
    stringr::str_detect(x[tab_rows[1] + 1, ]$raw, ',[A-z]+') ~ ',[A-z]|[A-z],',
    TRUE ~ '\\s+')
  
  x %>% 
    tidyr::fill(dplyr::one_of('problem', 'subprob', 'method')) %>% 
    dplyr::slice(-tab_rows) %>%
    dplyr::mutate(problem = as.numeric(.$problem),
                  subprob = as.numeric(.$subprob),
                  raw = stringr::str_trim(.$raw, side = 'both')) %>% 
    dplyr::group_by_(.dots = c('problem', 'subprob', 'method')) %>% 
    tidyr::nest() %>% 
    dplyr::mutate(data = purrr::map(.$data, ~raw_to_tibble(., sep, quiet, file = dat$name)))
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
    dplyr::select(-ncol(.)) %>% 
    purrr::set_names(stringr::str_replace_all(colnames(.), '[\\s\\(\\),]', '.')) %>% 
    dplyr::mutate_if(colnames(.) == 'NAME', function(x) stringr::str_replace_all(x, '[\\s\\(\\),]', '.')) %>% 
    dplyr::mutate_if(colnames(.) != 'NAME', as.numeric)
}
