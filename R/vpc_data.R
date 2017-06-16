#' Visual predictive checks data
#'
#' @description Generate visual predictive checks (VPC) data
#' 
#' @param xpdb An xpose database object.
#' @param vpc_opt A list of options regarding binning, pi and ci computation. 
#' For more information see \code{\link{vpc_opt_set}}.
#' @param vpc_type A string specifying the type of VPC to be created, can be one of: 
#' 'continuous', 'categorical', 'censored' or 'time-to-event'.
#' @param stratify Either a character string or a formula to stratify the data.
#' @param psn_folder Specify a PsN-generated VPC-folder.
#' @param obs_problem Alternative to the option `psn_folder`. The $problem number to 
#' be used for observations. By default returns the last estimation problem.
#' @param sim_problem Alternative to the option `psn_folder`. The $problem number to 
#' be used for simulations. By default returns the last simulation problem.
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
                     vpc_type    = c('continuous', 'categorical', 'censored', 'time-to-event'),
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
  
  # Set vpc_data options
  if (is.null(vpc_opt)) vpc_opt <- vpc_opt_set()
  
  # Prepare the stratification
  facets <- stratify
  if (is.formula(stratify)) stratify <- all.vars(stratify)
  
  # Get raw data
  if (is.null(psn_folder)) {
    # When using xpdb tables
    if (is.null(obs_problem)) obs_problem <- last_data_problem(xpdb, simtab = FALSE)
    if (is.null(sim_problem)) sim_problem <- last_data_problem(xpdb, simtab = TRUE)
    msg(c('Using xpdb simulation problem ', ifelse(is.na(sim_problem), '<na>', sim_problem), 
          ' and observation problem ', ifelse(is.na(obs_problem), '<na>', obs_problem), '.'), quiet)
    obs_data <- get_data(xpdb, problem = obs_problem) 
    sim_data <- get_data(xpdb, problem = sim_problem)
    if (is.null(vpc_opt$obs_cols)) {
      obs_cols <- xp_var(xpdb, obs_problem, type = c('id', 'idv', 'dv', 'pred'))
      obs_cols <- purrr::set_names(obs_cols$col, nm = obs_cols$type)
    }
    if (is.null(vpc_opt$sim_cols)) {
      sim_cols <- xp_var(xpdb, sim_problem, type = c('id', 'idv', 'dv', 'pred'))
      sim_cols <- purrr::set_names(sim_cols$col, nm = sim_cols$type)
    }
  } else {
    # When using PsN
    msg('Importing PsN generated data', quiet)
    if (!dir.exists(file_path(psn_folder, 'm1')) &
        file.exists(file_path(psn_folder, 'm1.zip'))) {
      msg('Unziping PsN m1 folder.', quiet)
      utils::unzip(zipfile = file_path(psn_folder, 'm1.zip'), 
                   exdir = file_path(psn_folder, ''))
    }
    obs_data <- read_nm_tables(files = file.path(psn_folder, 'm1', dir(stringr::str_c(psn_folder, 'm1', sep = .Platform$file.sep), 
                                                                       pattern = 'original.npctab')[1]), quiet = TRUE)
    sim_data <- read_nm_tables(files = file.path(psn_folder, 'm1', dir(stringr::str_c(psn_folder, 'm1', sep = .Platform$file.sep), 
                                                                       pattern = 'simulation.1.npctab')[1]), quiet = TRUE)
    if (file.exists(file_path(psn_folder, 'command.txt'))) {
      psn_cmd  <- readr::read_lines(file = file.path(psn_folder, 'command.txt'))
      obs_cols <- get_psn_vpc_cols(psn_cmd)
      sim_cols <- obs_cols
      if (is.null(stratify)) {
        stratify <- get_psn_vpc_strat(psn_cmd)
        facets   <- stratify
      }
      if (is.null(vpc_opt$pred_corr)) {
        vpc_opt$pred_corr <- dplyr::if_else(stringr::str_detect(psn_cmd, '-predcorr'), TRUE, FALSE)
      }
    } else {
      msg('File `command.txt` not found. Using default column names: ID, TIME, DV, PRED.', quiet)
      obs_cols <- c(id = 'ID', idv = 'TIME', dv = 'DV', pred = 'PRED')
      sim_cols <- obs_cols
    }
  } 
  
  if (is.null(obs_data) && is.null(sim_data)) {
    msg('No data table found.', quiet)
    return()
  }
  
  if (is.null(vpc_opt$pred_corr)) vpc_opt$pred_corr <- FALSE
  
  # Get the type of vpc
  vpc_type <- match.arg(vpc_type)
  
  # Info on stratification
  msg(c('Using ', stringr::str_c(stratify, collapse = ', '), 
        ' for stratification.'), quiet)
  
  # Generate vpc data
  if (vpc_type == 'continuous') {
    vpc_dat <- vpc::vpc(obs = obs_data, sim = sim_data, psn_folder = psn_folder, bins = vpc_opt$bins, 
                        n_bins = vpc_opt$n_bins, bin_mid = vpc_opt$bin_mid, obs_cols = obs_cols, 
                        sim_cols = sim_cols, stratify = stratify, pred_corr = vpc_opt$pred_corr, 
                        pred_corr_lower_bnd = vpc_opt$pred_corr_lower_bnd, pi = vpc_opt$pi, ci = vpc_opt$ci, 
                        uloq = vpc_opt$uloq, lloq = vpc_opt$lloq, smooth = FALSE, vpcdb = TRUE, verbose = !quiet) 
  } else if (vpc_type == 'categorical') {
    vpc_dat <- vpc::vpc_cat(obs = obs_data, sim = sim_data, psn_folder = psn_folder, bins = vpc_opt$bins, 
                            n_bins = vpc_opt$n_bins, bin_mid = vpc_opt$bin_mid, obs_cols = obs_cols, 
                            sim_cols = sim_cols, stratify = stratify, ci = vpc_opt$ci, 
                            uloq = vpc_opt$uloq, lloq = vpc_opt$lloq, smooth = FALSE, vpcdb = TRUE, verbose = !quiet) 
  } else if (vpc_type == 'censored') {
    vpc_dat <- vpc::vpc_cens(obs = obs_data, sim = sim_data, psn_folder = psn_folder, bins = vpc_opt$bins, 
                             n_bins = vpc_opt$n_bins, bin_mid = vpc_opt$bin_mid, obs_cols = obs_cols, 
                             sim_cols = sim_cols, stratify = stratify, ci = vpc_opt$ci, 
                             uloq = vpc_opt$uloq, lloq = vpc_opt$lloq, smooth = FALSE, vpcdb = TRUE, verbose = !quiet) 
  } else {
    msg('Time-to-event VPC are not yet available in xpose.', TRUE)
    return()
    # vpc_dat <- vpc::vpc_tte(obs = obs_data, sim = sim_data, psn_folder = psn_folder, bins = vpc_opt$bins, 
    #                         n_bins = vpc_opt$n_bins, obs_cols = obs_cols, sim_cols = sim_cols, stratify = stratify, 
    #                         ci = vpc_opt$ci, smooth = FALSE, rtte = vpc_opt$rtte, rtte_calc_diff = vpc_opt$rtte_calc_diff, 
    #                         events = vpc_opt$events, kmmc = vpc_opt$kmmc, reverse_prob = vpc_opt$reverse_prob, 
    #                         as_percentage = vpc_opt$as_percentage, vpcdb = TRUE, verbose = !quiet) 
  } 
  
  # Format vpc output
  xpdb$special <- vpc_dat %>%
    purrr::discard(names(.) %in% c('sim', 'stratify_original', 'stratify_color', 'facet', 'as_percentage')) %>%
    purrr::map_at('vpc_dat', function(x) {
      x <- x %>% 
        tidyr::gather(key = 'tmp', value = 'value', dplyr::matches('\\.(low|med|up)')) %>% 
        tidyr::separate_('tmp', into = c('simulations', 'ci'), sep = '\\.') %>% 
        tidyr::spread_(key_col = 'ci', value_col = 'value') %>% 
        dplyr::mutate(simulations = factor(.$simulations, levels = c('q5', 'q50', 'q95'),
                                           labels = c(stringr::str_c(min(vpc_opt$pi)*100, 'th percentile'), 
                                                      'Median', stringr::str_c(max(vpc_opt$pi)*100, 'th percentile')))) %>% 
        dplyr::mutate(strat = as.numeric(interaction(.$strat, .$simulations)))
      
      if ('strat1' %in% colnames(x)) {
        x$strat1 <- stringr::str_replace(x$strat1, stringr::str_c(vpc_dat$stratify[1], '='), '')
        colnames(x)[colnames(x) == 'strat1'] <- vpc_dat$stratify[1]
      }
      if ('strat2' %in% colnames(x)) {
        x$strat2 <- stringr::str_replace(x$strat2, stringr::str_c(vpc_dat$stratify[2], '='), '')
        colnames(x)[colnames(x) == 'strat2'] <- vpc_dat$stratify[2]
      }
      x
    }) %>% 
    purrr::map_at('aggr_obs', function(x) {
      x <- x %>% 
        tidyr::gather(key = 'observations', value = 'value', dplyr::one_of('obs5', 'obs50', 'obs95')) %>% 
        dplyr::mutate(observations = factor(.$observations, levels = c('obs5', 'obs50', 'obs95'),
                                            labels = c(stringr::str_c(min(vpc_opt$pi)*100, 'th percentile'), 
                                                       'Median', stringr::str_c(max(vpc_opt$pi)*100, 'th percentile')))) %>% 
        dplyr::mutate(strat = as.numeric(interaction(.$strat, .$observations)))
      
      if ('strat1' %in% colnames(x)) {
        x$strat1 <- stringr::str_replace(x$strat1, stringr::str_c(vpc_dat$stratify[1], '='), '')
        colnames(x)[colnames(x) == 'strat1'] <- vpc_dat$stratify[1]
      }
      if ('strat2' %in% colnames(x)) {
        x$strat2 <- stringr::str_replace(x$strat2, stringr::str_c(vpc_dat$stratify[2], '='), '')
        colnames(x)[colnames(x) == 'strat2'] <- vpc_dat$stratify[2]
      }
      x
    }) %>% 
    c(.,list(vpc_opt = vpc_opt, facets = facets, psn_folder = psn_folder,
             obs_problem = obs_problem, sim_problem = sim_problem, 
             obs_cols = obs_cols, sim_cols = sim_cols)) %>%
             {dplyr::tibble(problem = 0, method = 'vpc', type = vpc_type, data = list(.))} %>%
    dplyr::bind_rows(xpdb$special) %>%
    dplyr::distinct_(.dots = c('problem', 'method', 'type'), .keep_all = TRUE)
  
  xpdb
}
