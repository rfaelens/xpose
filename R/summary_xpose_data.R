#' Summarizing xpose_data
#'
#' @description This function returns a summary of an \code{\link{xpose_data}} 
#' to the console.
#' @param object An \code{xpose_data} object generated with \code{\link{xpose_data}}.
#' @param problem The problem to be used, by default returns the last one for each label.
#' @param ... Ignored in this function
#'
#' @method summary xpose_data
#' @examples
#' summary(xpdb_ex_pk)
#'
#' @export
summary.xpose_data <- function(object, problem = NULL, ...) {
  x <- get_summary(object, problem) %>% 
    dplyr::filter(.$value != 'na', !.$label %in% c()) %>% 
    dplyr::select(dplyr::one_of('problem', 'descr', 'value')) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(grouping = as.character(.$problem)) %>% 
    dplyr::group_by_(.dots = 'grouping') %>% 
    tidyr::nest() %>% 
    {purrr::map(.$data, function(x) {
      cat('\nSummary for problem no.', x$problem[1])
      as.data.frame(x[, c('descr', 'value')]) %>%
        purrr::set_names(rep('', 2)) %>%
        print(row.names = FALSE, right = FALSE)
      })}
}
