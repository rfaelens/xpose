#' Set variable type
#'
#' @description Function designed to change the type associated with variables.
#' 
#' @param xpdb An \code{xpose_data} object.
#' @param problem The problem number to which the edits will be applied.
#' @param ... Specifications of the edits to be made to the xpdb index. Edits are made as 
#' type and variable pairs e.g. idv = 'TAD' will assign TAD to the type idv (independent variable).
#' @param auto_factor If \code{TRUE} new columns assigned to the type 'catcov' will be converted to
#' factor.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' 
#' @section Recognized variable types:
#' \itemize{
#'   \item amt: Dose amount
#'   \item catcov: Categorical covariates
#'   \item contcov: Continuous covariates
#'   \item dv: Dependent variable
#'   \item dvid: DV identifier
#'   \item eta: Eta
#'   \item evid: Event identifier
#'   \item id: Subject identifier
#'   \item idv: Independent variable
#'   \item ipred: Model individual predictions
#'   \item mdv: Missing dependent variable
#'   \item na: Not attributed
#'   \item occ: Occasion flag
#'   \item param: Model parameter
#'   \item pred: Model typical predictions
#'   \item res: Residuals
#'  }
#'  
#' @return An xpose_data object
#' @seealso \code{\link{list_vars}}
#' @examples
#' xpdb_ex_pk <- set_vars_type(xpdb_ex_pk, problem = 1, idv = 'TAD')
#' @export
set_vars_type <- function(xpdb, problem = NULL, ..., auto_factor = TRUE, quiet) {
  
  if (!is.xpdb(xpdb)) {
    stop('Valid `xpdb` input required.', call. = FALSE)
  }
  
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  dat <- xpdb$data
  
  if (!is.null(problem) && !all(problem %in% dat$problem)) {
    stop('Problem no.', stringr::str_c(problem[!problem %in% dat$problem], collapse = ', '), 
         ' not found in model output data.', call. = FALSE)
  }
  if (is.null(problem)) problem <- unique(dat$problem)
  
  args <- c(...)
  if (is.null(args)) return(xpdb)
  
  args <- args %>%
    dplyr::data_frame(col = ., type = names(.)) %>% 
    dplyr::mutate(type = stringr::str_replace(.$type, '\\d$', ''))
  
  xpdb$data <- dat %>% 
    dplyr::mutate(grouping = .$problem) %>% 
    dplyr::group_by_(.dots = 'grouping') %>% 
    tidyr::nest(.key = 'tmp') %>% 
    dplyr::mutate(out = purrr::map_if(.$tmp, .$grouping %in% problem, function(x, args, quiet) {
      # Get the index
      index <- x$index[[1]]
      
      # Check for missmatches
      if (any(!args$col %in% index$col)) {
        msg(c('In $prob no.', x$problem, ' columns: ',
              stringr::str_c(args$col[!args$col %in% index$col], collapse = ', '),
              ' not present in the data.'), quiet)
        args <- dplyr::filter(.data = args, args$col %in% index$col)
      }
      
      # Remove previous index when only one variable can be used at the time
      single_type <- c('amt', 'dv', 'dvid', 'evid', 'id', 'idv', 'ipred', 'mdv', 'pred')
      single_type <- single_type[single_type %in% args$type]
      if (length(single_type) > 0) index$type[index$type %in% single_type] <- 'na'
      
      # Replace the matching values
      index$type[match(args$col, index$col)] <- args$type
      x$index[[1]] <- index
      
      # Change categorical covariates to factor
      if (any(args$type == 'catcov') && auto_factor) {
        tables <- x$data[[1]]
        col_to_factor <- colnames(tables) %in% args$col[args$type == 'catcov']
        x$data[[1]] <- tables %>%   
          dplyr::mutate_if(col_to_factor, as.factor)
      }
      
      # Output new index
      x
    }, args = args, quiet = quiet)) %>% 
    tidyr::unnest_(unnest_cols = 'out') %>% 
    dplyr::select(dplyr::one_of('problem', 'simtab', 'data', 'index'))
  
  xpdb
}
#set_var_label()
#set_var_unit()
