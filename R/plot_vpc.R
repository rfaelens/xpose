#' Visual predictive checks
#'
#' @description Generate visual predictive checks (VPC)
#' 
#' @param xpdb An xpose database object.
#' @param vpc_opt A list of options regarding binning, pi and ci computation. 
#' For more information see \code{\link{vpc_opt_set}}.
#' @param mapping List of aesthetics mappings to be used for the xpose plot 
#' (e.g. \code{point_color}).
#' @param type String setting the type of plot to be used points 'p',
#' line 'l', area 'a' and text 't' or any combination of the four.
#' @param smooth Should the bins be smoothed (connect bin midpoints, default) or shown as rectangular boxes.
#' @param facets Either a character string to use \link[ggplot2]{facet_wrap} 
#' or a formula to use \link[ggplot2]{facet_grid}.
#' @param title Plot title. Use \code{NULL} to remove.
#' @param subtitle Plot subtitle. Use \code{NULL} to remove.
#' @param caption Page caption. Use \code{NULL} to remove.
#' @param log String assigning logarithmic scale to axes, can be either '', 
#' 'x', y' or 'xy'.       
#' @param psn_folder Specify a PsN-generated VPC-folder.
#' @param obs_problem Alternative to the option `psn_folder`. The $problem number to be used for observations.
#' By default returns the last estimation problem.
#' @param sim_problem Alternative to the option `psn_folder`. The $problem number to be used for simulations.
#' By default returns the last simulation problem.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' @param ... any additional aesthetics.

#' @inheritSection xplot_scatter Layers mapping
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link[vpc]{vpc}}
#' @examples
#' \dontrun{
#' # Simple vpc using simulated data from the xpdb
#' vpc(xpdb_ex_pk)
#' }
#' @export
vpc <- function(xpdb,
                vpc_opt     = NULL, 
                mapping     = NULL,
                #group      = 'variable',
                type        = 'alp',
                smooth      = TRUE,
                facets      = NULL,
                title       = 'Visual predictive checks | @run',
                subtitle    = 'Nsim: @nsim, PI: @vpcpi',
                caption     = '@dir',
                log         = NULL,
                psn_folder  = NULL,
                obs_problem = NULL,
                sim_problem = NULL,
                quiet,
                ...) {
  
  # Check input
  if (!is.xpdb(xpdb)) { 
    msg('Bad input to the argument`xpdb`', ifelse(missing(quiet), TRUE, quiet))
    return()
  }
  
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  if (is.null(psn_folder)) {
    if (is.null(obs_problem)) obs_problem <- last_data_problem(xpdb, simtab = FALSE)
    if (is.null(sim_problem)) sim_problem <- last_data_problem(xpdb, simtab = TRUE)
    msg(c('Using simulation problem ', ifelse(is.na(sim_problem), '<na>', sim_problem), 
          ' and observation problem ', ifelse(is.na(obs_problem), '<na>', obs_problem), '.'), quiet)
    obs_data   <- get_data(xpdb, problem = obs_problem) 
    sim_data   <- get_data(xpdb, problem = sim_problem)
  } else {
    msg('Importing PsN generated data', quiet)
    obs_data <- read_nm_tables(files = file.path(psn_folder, 'm1', dir(stringr::str_c(psn_folder, 'm1', sep = .Platform$file.sep), 
                                                                       pattern = 'original.npctab')[1]), quiet = TRUE)
    sim_data <- read_nm_tables(files = file.path(psn_folder, 'm1', dir(stringr::str_c(psn_folder, 'm1', sep = .Platform$file.sep), 
                                                                       pattern = 'simulation.1.npctab')[1]), quiet = TRUE)
  } 
  
  if (is.null(obs_data) && is.null(sim_data)) {
    msg('At least a simulation or an observation dataset are required to create the VPC.', quiet)
    return()
  }
  
  if (is.null(vpc_opt)) vpc_opt <- vpc_opt_set()
  
  if (is.formula(facets)) {
    stratify <- all.vars(facets)
  } else {
    stratify <- facets
  }
  
  # Generate vpc data
  vpc_dat <- vpc::vpc(obs = obs_data, sim = sim_data, psn_folder = psn_folder, bins = vpc_opt$bins, 
                      n_bins = vpc_opt$n_bins, bin_mid = vpc_opt$bin_mid, obs_cols = vpc_opt$obs_cols, 
                      sim_cols = vpc_opt$sim_cols, stratify = stratify, pred_corr = vpc_opt$pred_corr, 
                      pred_corr_lower_bnd = vpc_opt$pred_corr_lower_bnd, pi = vpc_opt$pi, ci = vpc_opt$ci, 
                      uloq = vpc_opt$uloq, lloq = vpc_opt$lloq, smooth = smooth, vpcdb = TRUE, verbose = !quiet) 
  vpc_dat  
}


#' Create options for data import
#' 
#' @description Provide a list of options to the general plotting functions such as 
#' \code{xplot_scatter} in order to create appropriate data input for ggplot2.
#' 
#' @param bins Binning method, can be one of 'density', 'time', 'data', 'none', or one of the approaches 
#' available in \code{classInterval()} such as 'jenks' (default), 'pretty', or a numeric vector specifying 
#' the bin separators.
#' @param n_bins When using the 'auto' binning method, what number of bins to aim for.
#' @param bin_mid Specify how to is the mid bin value calculated, can be either 'mean' for the mean of all 
#' timepoints (default) or 'middle' to use the average of the bin boundaries.
#' @param obs_cols Observation dataset column names (list elements: 'dv', 'idv', 'id', 'pred')
#' @param sim_cols Simulation dataset column names (list elements: 'dv', 'idv', 'id', 'pred')
#' @param pred_corr Logical, should a prediction correction (pcVPC) of the data be used.
#' @param pred_corr_lower_bnd Lower bound for the prediction-correction.
#' @param ci Confidence interval around the percentiles to plot. Default is (0.05, 0.95)
#' @param pi Simulated prediction interval to plot. Default is c(0.05, 0.95).
#' @param lloq Number or NULL indicating lower limit of quantification. Default is NULL.               
#' @param uloq Number or NULL indicating upper limit of quantification. Default is NULL.
#'
#' @seealso \code{{vpc}}
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
                        ci                  = c(0.05, 0.95), 
                        pi                  = c(0.05, 0.95), 
                        lloq                = NULL, 
                        uloq                = NULL) {
  
  list(bins = bins, n_bins = n_bins, bin_mid = bin_mid,
       obs_cols = obs_cols, sim_cols = sim_cols, # Temporary?
       pred_corr = pred_corr, 
       pred_corr_lower_bnd = pred_corr_lower_bnd,
       pi = pi, ci = ci, lloq = lloq, uloq = uloq)
}
