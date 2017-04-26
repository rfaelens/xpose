#' List NONMEM output tables
#'
#' @description List NONMEM output tables file names from a \code{mod_file} object.
#'
#' @param mod_file A mod_file object generated with \code{\link{read_nm_model}}.
#'
#' @seealso \code{\link{read_nm_model}}, \code{\link{read_nm_tables}}
#' @examples
#' \dontrun{
#' read_nm_model('inst/extdata/run001.mod') %>% 
#'   list_nm_tables() %>%
#'   read_nm_tables()
#'   }
#' @export
list_nm_tables <- function(mod_file = NULL) {
  
  if (is.null(mod_file) || !is.model.file(mod_file)) {
    stop('Object of class `mod_file` required.', call. = FALSE)
  }
  
  tab_code <- mod_file[mod_file$subroutine == 'tab', ]$code
  
  if (length(tab_code) == 0) return()
  
  tab_file <- stringr::str_match(tab_code, '\\s+FILE\\s*=\\s*([^\\s]+)')[, 2]
  tab_file <- tab_file[!is.na(tab_file)]
  
  if (length(tab_file) == 0) return()
  
  file_path(attr(mod_file, 'dir'), tab_file)
}
