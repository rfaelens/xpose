#' Visual predictive checks
#'
#' @description Generate visual predictive checks (VPC)
#' 
#' @param xpdb An xpose database object.
#' @param mapping List of aesthetics mappings to be used for the xpose plot 
#' (e.g. \code{point_color}).
#' @param type String setting the type of plot to be used points 'p',
#' line 'l', area 'a' and text 't' or any combination of the four.
#' @param smooth Should the bins be smoothed (connect bin midpoints, default) or shown as rectangular boxes.

# @param facets Either a character string to use \link[ggplot2]{facet_wrap} 
# or a formula to use \link[ggplot2]{facet_grid}.

#' @param title Plot title. Use \code{NULL} to remove.
#' @param subtitle Plot subtitle. Use \code{NULL} to remove.
#' @param caption Page caption. Use \code{NULL} to remove.
#' @param log String assigning logarithmic scale to axes, can be either '', 
#' 'x', y' or 'xy'.       
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' @param ... any additional aesthetics.

#' @inheritSection xplot_scatter Layers mapping
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{vpc_data}
#' @examples
#' \dontrun{
#' xpdb_ex_pk %>% 
#'  vpc_data(xpdb_ex_pk) %>% 
#'  vpc()
#' }
#' @export
vpc <- function(xpdb,
                mapping     = NULL,
                type        = 'alp',
                smooth      = TRUE,
                #facets      = NULL,
                title       = 'Visual predictive checks | @run',
                subtitle    = 'Nsim: @nsim, PI: @vpcpi',
                caption     = '@dir',
                log         = NULL,
                quiet,
                ...) {
  
}


#' Visual predictive checks data
#'
#' @description Generate visual predictive checks (VPC) data
#' 
#' @param xpdb An xpose database object.
#' @param vpc_opt A list of options regarding binning, pi and ci computation. 
#' For more information see \code{\link{vpc_opt_set}}.
#' @param stratify Either a character string or a formula to stratify the data.
#' @param psn_folder Specify a PsN-generated VPC-folder.
#' @param obs_problem Alternative to the option `psn_folder`. The $problem number to be used for observations.
#' By default returns the last estimation problem.
#' @param sim_problem Alternative to the option `psn_folder`. The $problem number to be used for simulations.
#' By default returns the last simulation problem.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' @param ... any additional aesthetics.
#' 
#' @seealso \code{vpc} \code{vpc_opt_set} \code{\link[vpc]{vpc}}
#' @examples
#' \dontrun{
#' xpdb_ex_pk %>% 
#'  vpc_data(xpdb_ex_pk) %>% 
#'  vpc()
#' }
#' @export
vpc_data <- function(xpdb,
                     vpc_opt     = NULL,
                     type        = c('continuous', 'categorical', 'censored', 'time-to-event'),
                     stratify    = NULL,
                     psn_folder  = NULL,
                     obs_problem = NULL,
                     sim_problem = NULL,
                     quiet) {
  
  # Check input
  if (!is.xpdb(xpdb)) { 
    msg('Bad input to the argument`xpdb`', ifelse(missing(quiet), TRUE, quiet))
    return()
  }
  
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  # Get raw data
  if (is.null(psn_folder)) {
    # When using xpdb tables
    if (is.null(obs_problem)) obs_problem <- last_data_problem(xpdb, simtab = FALSE)
    if (is.null(sim_problem)) sim_problem <- last_data_problem(xpdb, simtab = TRUE)
    msg(c('Using xpdb simulation problem ', ifelse(is.na(sim_problem), '<na>', sim_problem), 
          ' and observation problem ', ifelse(is.na(obs_problem), '<na>', obs_problem), '.'), quiet)
    obs_data <- get_data(xpdb, problem = obs_problem) 
    sim_data <- get_data(xpdb, problem = sim_problem)
    obs_cols <- xp_var(xpdb, obs_problem, type = c('id', 'idv', 'dv', 'pred'))
    obs_cols <- purrr::set_names(obs_cols$col, nm = obs_cols$type)
    sim_cols <- xp_var(xpdb, sim_problem, type = c('id', 'idv', 'dv', 'pred'))
    sim_cols <- purrr::set_names(sim_cols$col, nm = sim_cols$type)
  } else {
    # When using PsN
    msg('Importing PsN generated data', quiet)
    obs_data <- read_nm_tables(files = file.path(psn_folder, 'm1', dir(stringr::str_c(psn_folder, 'm1', sep = .Platform$file.sep), 
                                                                       pattern = 'original.npctab')[1]), quiet = TRUE)
    sim_data <- read_nm_tables(files = file.path(psn_folder, 'm1', dir(stringr::str_c(psn_folder, 'm1', sep = .Platform$file.sep), 
                                                                       pattern = 'simulation.1.npctab')[1]), quiet = TRUE)
    obs_cols <- NULL
    sim_cols <- NULL
  } 
  
  if (is.null(obs_data) && is.null(sim_data)) {
    msg('At least a simulation or an observation dataset are required to create the VPC.', quiet)
    return()
  }
  
  # Prep for data processing
  if (is.null(vpc_opt)) vpc_opt <- vpc_opt_set()
  if (is.formula(stratify)) stratify <- all.vars(stratify)
  
  # Get the type of vpc
  type <- match.arg(type)
  
  # Generate vpc data
  if (type == 'continuous') {
    vpc_dat <- vpc::vpc(obs = obs_data, sim = sim_data, psn_folder = psn_folder, bins = vpc_opt$bins, 
                        n_bins = vpc_opt$n_bins, bin_mid = vpc_opt$bin_mid, obs_cols = obs_cols, 
                        sim_cols = sim_cols, stratify = stratify, pred_corr = vpc_opt$pred_corr, 
                        pred_corr_lower_bnd = vpc_opt$pred_corr_lower_bnd, pi = vpc_opt$pi, ci = vpc_opt$ci, 
                        uloq = vpc_opt$uloq, lloq = vpc_opt$lloq, smooth = smooth, vpcdb = TRUE, verbose = !quiet) 
  } else if (type == 'categorical') {
    vpc_dat <- vpc::vpc_cat(obs = obs_data, sim = sim_data, psn_folder = psn_folder, bins = vpc_opt$bins, 
                            n_bins = vpc_opt$n_bins, bin_mid = vpc_opt$bin_mid, obs_cols = obs_cols, 
                            sim_cols = sim_cols, stratify = stratify, ci = vpc_opt$ci, 
                            uloq = vpc_opt$uloq, lloq = vpc_opt$lloq, smooth = smooth, vpcdb = TRUE, verbose = !quiet) 
  } else if (type == 'censored') {
    vpc_dat <- vpc::vpc_cens(obs = obs_data, sim = sim_data, psn_folder = psn_folder, bins = vpc_opt$bins, 
                             n_bins = vpc_opt$n_bins, bin_mid = vpc_opt$bin_mid, obs_cols = obs_cols, 
                             sim_cols = sim_cols, stratify = stratify, ci = vpc_opt$ci, 
                             uloq = vpc_opt$uloq, lloq = vpc_opt$lloq, smooth = smooth, vpcdb = TRUE, verbose = !quiet) 
  } else {
    vpc_dat <- vpc::vpc_tte(obs = obs_data, sim = sim_data, psn_folder = psn_folder, bins = vpc_opt$bins, 
                            n_bins = vpc_opt$n_bins, obs_cols = obs_cols, sim_cols = sim_cols, stratify = stratify, 
                            ci = vpc_opt$ci, smooth = smooth, rtte = vpc_opt$rtte, rtte_calc_diff = vpc_opt$rtte_calc_diff, 
                            events = vpc_opt$events, kmmc = vpc_opt$kmmc, reverse_prob = vpc_opt$reverse_prob, 
                            as_percentage = vpc_opt$as_percentage, vpcdb = TRUE, verbose = !quiet) 
  } 
  vpc_dat  
}


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
#' vpc_opt_set()
#' 
#' @export
vpc_opt_set <- function(bins                = 'jenks',
                        n_bins              = 'auto',
                        bin_mid             = 'mean',
                        obs_cols            = NULL,
                        sim_cols            = NULL,
                        pred_corr           = FALSE, 
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
  
  list(bins = bins, n_bins = n_bins, bin_mid = bin_mid,
       pred_corr = pred_corr, pred_corr_lower_bnd = pred_corr_lower_bnd,
       pi = pi, ci = ci, lloq = lloq, uloq = uloq, rtte = rtte,
       rtte_calc_diff = rtte_calc_diff, events = events, kmmc = kmmc,
       reverse_prob = reverse_prob, as_percentage = as_percentage)
}
