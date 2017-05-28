#' Manually define nonmem tables to be imported
#' 
#' @description Manually provide names of the table files to be imported by \code{xpose_data}.
#'
#' @param tab_names Provide the name of the tables to import e.g. 'sdtab', 'patab', 'cotab', 
#' 'catab' for NONMEM.
#' @param tab_suffix Default is '', but can be changed to any character string to be used as 
#' suffix in the table names.
#' @param sim_suffix Default is 'sim', but can be changed to any character string to be used as 
#' suffix in the simulation table names e.g. sdtab001sim.
#'
#' @seealso \code{\link{xpose_data}}
#' @examples 
#' \dontrun{
#' # Import all names specified by default as in xpose4
#' xpose_data(runno = '001', manual_import = manual_nm_import())
#' 
#' # Import a specific table name
#' xpose_data(runno = '001', manual_import = manual_nm_import(tab_names = 'mytab'))
#' }
#' @export
manual_nm_import <- function(tab_names = c('sdtab', 'mutab', 'patab', 'catab', 'cotab', 
                                           'mytab', 'extra', 'xptab', 'cwtab'),
                             tab_suffix = '', sim_suffix = 'sim') {
  
  list(tab_suffix = tab_suffix, sim_suffix = sim_suffix, tab_names = tab_names)
}


# Creates an nm_table_list from manually defined table name patterns
list_nm_tables_manual <- function(file, prefix, tab_list) {
  file_path(dirname(file), stringr::str_c(tab_list$tab_names, get_runno(file, prefix))) %>% 
    dplyr::tibble(problem = 1, file = ., firstonly = FALSE, simtab = NA) %>% 
    tidyr::expand(problem = .$problem, file = .$file, firstonly = .$firstonly, simtab = c(FALSE, TRUE)) %>% 
    dplyr::mutate(file = dplyr::if_else(.$simtab, stringr::str_c(.$file, tab_list$sim_suffix),
                                        stringr::str_c(.$file, tab_list$tab_suffix))) %>% 
    dplyr::filter(file.exists(.$file)) %>% 
    as.nm.table.list()
}
