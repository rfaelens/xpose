#' Generate a list of options for VPC data generation
#' 
#' @description Provide a list of options to \code{vpc_data} function.
#' 
#' @param bins Binning method, can be one of 'density', 'time', 'data', 'none', or one of the approaches 
#' available in \code{classInterval()} such as 'jenks' (default), 'pretty', or a numeric vector specifying 
#' the bin separators.
#' @param n_bins When using the 'auto' binning method, what number of bins to aim for.
#' @param bin_mid Specify how to is the mid bin value calculated, can be either 'mean' for the mean of all 
#' timepoints (default) or 'middle' to use the average of the bin boundaries.
#' @param pred_corr Option reserved to continuous VPC. Logical, should a prediction correction (pcVPC) of the data be used.
#' @param pred_corr_lower_bnd Option reserved to continuous VPC. Lower bound for the prediction-correction.
#' @param pi Option reserved to continuous VPC. Simulated prediction interval to plot. Default is c(0.05, 0.95).
#' @param ci Confidence interval around the percentiles to plot. Default is c(0.05, 0.95)
#' @param lloq Number or NULL indicating lower limit of quantification. Default is NULL.               
#' @param uloq Number or NULL indicating upper limit of quantification. Default is NULL.
#' @param rtte Option reserved to time-to-event VPC. Is the data repeated time-to-event (RTTE) \code{TRUE} or 
#' single time-to-event (TTE) \code{FALSE}.
#' @param rtte_calc_diff Option reserved to time-to-event VPC. Should the time be recalculated? When simulating in NONMEM, 
#' you will probably need to set this to \code{TRUE} to recalculate the TIME to the relative time between events (unless you 
#' output the time difference between events and specify that as independent variable in the index.
#' @param kmmc Option reserved to time-to-event VPC. Either NULL for regular TTE VPC (default), or a variable name 
#' for a KMMC plot (e.g. 'WT').
#' @param events Option reserved to time-to-event VPC. Numeric vector describing which events to show a VPC for when 
#' repeated TTE data, e.g. c(1:4). Default is \code{NULL}, which shows all events.
#' @param reverse_prob Option reserved to time-to-event VPC. Should the probability be reversed (i.e. plot 1-probability).
#' @param as_percentage Should the Y-scale be in percent (0-100) \code{TRUE} (default), or standard (0-1) \code{FALSE}.
#'
#' @seealso \code{vpc_data} \code{\link[vpc]{vpc}}
#' 
#' @examples
#' vpc_opt()
#' 
#' @export
vpc_opt <- function(bins                = 'jenks',
                    n_bins              = 'auto',
                    bin_mid             = 'mean',
                    pred_corr           = NULL, 
                    pred_corr_lower_bnd = 0,
                    pi                  = c(0.05, 0.95), 
                    ci                  = c(0.05, 0.95), 
                    lloq                = NULL, 
                    uloq                = NULL,
                    rtte                = FALSE,
                    rtte_calc_diff      = TRUE,
                    events              = NULL,
                    kmmc                = NULL,
                    reverse_prob        = FALSE,
                    as_percentage       = TRUE) {
  
  list(bins = bins, n_bins = n_bins, bin_mid = bin_mid, pred_corr = pred_corr, 
       pred_corr_lower_bnd = pred_corr_lower_bnd, pi = pi, ci = ci, lloq = lloq, 
       uloq = uloq, rtte = rtte, rtte_calc_diff = rtte_calc_diff, events = events, 
       kmmc = kmmc, reverse_prob = reverse_prob, as_percentage = as_percentage)
}


# Get columns info from PsN
get_psn_vpc_cols <- function(psn_cmd) {
  if (stringr::str_detect(psn_cmd, '-idv')) {
    idv <- stringr::str_match(psn_cmd, '-idv=\\s*([^\\s]+)')[1, 2]
  } else {
    idv <- 'TIME'
  }
  
  if (stringr::str_detect(psn_cmd, '-dv')) {
    dv <- stringr::str_match(psn_cmd, '-dv=\\s*([^\\s]+)')[1, 2]
  } else {
    dv <- 'DV'
  }
  
  list(id = 'ID', idv = idv, dv = dv, pred = 'PRED')
}


# Gets strata info from PsN command
get_psn_vpc_strat <- function(psn_cmd) {
  if (stringr::str_detect(psn_cmd, '-stratify_on')) {
    psn_cmd %>% 
    {stringr::str_match(string = ., pattern = '-stratify_on=\\s*([^\\s]+)')[1, 2]} %>% 
      stringr::str_split(',') %>% 
      purrr::flatten_chr()
  }
}
