#' List NONMEM output tables
#'
#' @description List NONMEM output tables file names from a \code{nm_model} object.
#'
#' @param nm_model An xpose nm_model object generated with \code{\link{read_nm_model}}.
#'
#' @seealso \code{\link{read_nm_model}}, \code{\link{read_nm_tables}}
#' @examples
#' \dontrun{
#' read_nm_model('inst/extdata/run001.mod') %>% 
#'   list_nm_tables() %>%
#'   read_nm_tables()
#'   }
#' @export
list_nm_tables <- function(nm_model = NULL) {
  
  if (is.null(nm_model) || !is.nm.model(nm_model)) {
    stop('Object of class `nm_model` required.', call. = FALSE)
  }
  
  table_list <- nm_model %>% 
    
    # Get NM code associated with each table
    dplyr::filter(.$problem > 0, .$subroutine == 'tab') %>% 
    purrr::slice_rows(c('problem', 'level')) %>% 
    purrr::by_slice(~stringr::str_c(.$code, collapse = ' ')) %>% 
    purrr::set_names(c('problem', 'level', 'string')) %>% 
    tidyr::unnest() %>% 
    
    # Find table names and firstonly option
    dplyr::mutate(file = stringr::str_match(.$string, '\\s+FILE\\s*=\\s*([^\\s]+)')[, 2]) %>% 
    dplyr::filter(!is.na(.$file)) %>% 
    dplyr::mutate(file = file_path(attr(nm_model, 'dir'), .$file),
                  firstonly = stringr::str_detect(.$string, 'FIRSTONLY')) %>% 
    dplyr::select(dplyr::one_of('problem', 'file', 'firstonly'))
  
  # Add simtab flag
  sim_flag <- nm_model %>% 
    dplyr::filter(.$problem > 0) %>% 
    purrr::slice_rows('problem') %>% 
    purrr::by_slice(~!any(stringr::str_detect(.$subroutine, 'est'))) %>% 
    tidyr::unnest() %>% 
    purrr::set_names(c('problem', 'simtab'))
  
  # Merge and output
  table_list %>% 
    dplyr::left_join(sim_flag, by = 'problem') %>% 
    structure(class = c('nm_table_list', 'tbl_df', 'tbl', 'data.frame'))
}
