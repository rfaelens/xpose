#' List NONMEM output tables
#'
#' @description List NONMEM output tables file names from a \code{mod_file} object.
#'
#' @param mod_file A mod_file object generated with \code{\link{read_nm_model}}.
#' @param dir Location of the model files.
#'
#' @seealso \code{\link{read_nm_model}}, \code{\link{read_nm_tables}}
#' @examples
#' \dontrun{
#' file <- 'inst/extdata/run001.mod'
#' read_nm_model(file = file) %>% 
#'   list_nm_tables(dir = dirname(file)) %>%
#'   read_nm_tables()
#'   }
#' @export
list_nm_tables <- function(mod_file = NULL,
                        dir      = NULL) {
  
  if (is.null(mod_file) || !is.model.file(mod_file)) {
    stop('Object of class `mod_file` required.', call. = FALSE)
  }
  
  tab_code <- mod_file$CODE[mod_file$ABREV == 'TAB']
  
  if (length(tab_code) == 0) return()

  tab_file <- stringr::str_match(string = mod_file$CODE[mod_file$ABREV == 'TAB'], 
                                  pattern = '\\s+FILE\\s*=\\s*([^\\s]+)')[, 2]
  tab_file <- tab_file[!is.na(tab_file)]
  
  if (length(tab_file) == 0) return()

  file_path(dir, tab_file)
}
