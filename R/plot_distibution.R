#' Distribution plots of ETA and parameters
#'
#' @description Histograms and density plots of the ETA and parameter values.
#' 
#' @inheritParams dv_vs_pred
#' @param type String setting the type of plot to be used. Can be histogram 'h',
#' density 'd', rug 'r' or any combination of the three.
#' @param guide Should the guide (e.g. reference distribution) be displayed.
#' @param drop_static Should columns that only have a single unique value 
#' (i.e. static) be dropped.
#' 
#' @inheritSection xplot_distrib Layers mapping
#' @inheritSection xplot_scatter Faceting
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_distrib}}
#' @examples
#' # Histogram of parameters
#' prm_distrib(xpdb_ex_pk, type = 'h')
#' 
#' # Density plot of etas with a rug
#' eta_distrib(xpdb_ex_pk, type = 'dr')
#' 
#' # Histogram of residuals by occasion
#' res_distrib(xpdb_ex_pk, type = 'hr', res = c('IWRES', 'CWRES'))
#' 
#' # Density plot of continuous covariates
#' cov_distrib(xpdb_ex_pk, type = 'd')
#' @name distrib_plot
#' @export
prm_distrib <- function(xpdb,
                        mapping  = NULL,
                        drop_static = TRUE,
                        type     = 'hr',
                        facets   = NULL,
                        title    = 'Parameter distribution | @run',
                        subtitle = 'Based on @nind individuals',
                        caption  = '@dir',
                        log      = NULL,
                        guide    = FALSE,
                        problem,
                        quiet,
                        ...) {
  # Check input
  check_xpdb(xpdb, check = 'data')
  if (missing(problem)) problem <- default_plot_problem(xpdb)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  if (is.null(facets)) facets <- 'variable'
  
  prm_col <- xp_var(xpdb, problem, type = 'param')$col
  if (drop_static) {
    prm_col <- drop_static_cols(xpdb, problem, cols = prm_col, quiet = quiet)
  }
  if (is.null(prm_col)) {
    stop('No parameter column found in the xpdb data index.', call. = FALSE)
  }
  
  xplot_distrib(xpdb = xpdb, quiet = quiet,
                opt = data_opt(problem = problem, 
                               filter = only_distinct(xpdb, problem, facets, quiet),
                               tidy = TRUE, value_col = prm_col),
                mapping = aes_c(aes_string(x = 'value'), mapping), 
                type = type, guide = guide, facets = facets,
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]), ...)
}

#' @rdname distrib_plot
#' @export
eta_distrib <- function(xpdb,
                        mapping  = NULL,
                        drop_static = TRUE,
                        type     = 'hr',
                        facets   = NULL,
                        title    = 'Eta distribution | @run',
                        subtitle = 'Based on @nind individuals, Eta shrink: @etashk',
                        caption  = '@dir',
                        log      = NULL,
                        guide    = FALSE,
                        problem,
                        quiet,
                        ...) {
  # Check input
  check_xpdb(xpdb, check = 'data')
  if (missing(problem)) problem <- default_plot_problem(xpdb)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  if (is.null(facets)) facets <- 'variable'
  
  eta_col <- xp_var(xpdb, problem, type = 'eta')$col
  if (drop_static) {
    eta_col <- drop_static_cols(xpdb, problem, cols = eta_col, quiet = quiet)
  }
  if (is.null(eta_col)) {
    stop('No eta column found in the xpdb data index.', call. = FALSE)
  }
  
  xplot_distrib(xpdb = xpdb, quiet = quiet,
                opt = data_opt(problem = problem, 
                               filter = only_distinct(xpdb, problem, facets, quiet), 
                               tidy = TRUE, value_col = eta_col,
                               post_processing = reorder_etas_factors()),
                mapping = aes_c(aes_string(x = 'value'), mapping), 
                type = type, guide = guide, facets = facets,
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]), ...)
}

#' @param res Only used for \code{res_distrib}. Defines the type of residual to be used. Default is "CWRES".
#' @rdname distrib_plot
#' @export
res_distrib <- function(xpdb,
                        mapping  = NULL,
                        res      = 'CWRES',
                        type     = 'hr',
                        facets   = NULL,
                        title    = '@x distribution | @run',
                        subtitle = 'Based on @nobs observations',
                        caption  = '@dir',
                        log      = NULL,
                        guide    = FALSE,
                        problem,
                        quiet,
                        ...) {
  # Check input
  check_xpdb(xpdb, check = 'data')
  if (missing(problem)) problem <- default_plot_problem(xpdb)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  if (is.null(xp_var(xpdb, problem, col = res))) {
    stop('No ', stringr::str_c(res, collapse = ', '), 
         ' column found in the xpdb data index.', call. = FALSE)
  }
  
  if (length(res) > 1) {
    if (is.null(facets)) facets <- 'variable'
    opt <- data_opt(problem = problem, 
                    filter = only_obs(xpdb, problem, quiet),
                    tidy = TRUE, value_col = res)
    vars <- aes_c(aes_string(x = 'value'), mapping)
  } else {
    opt <- data_opt(problem = problem, 
                    filter = only_obs(xpdb, problem, quiet))
    vars <- aes_c(aes_string(x = toupper(res)), mapping)
  }
  
  xplot_distrib(xpdb = xpdb, quiet = quiet,
                opt = opt, mapping = vars, 
                type = type, guide = guide, facets = facets,
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]), ...)
}

#' @rdname distrib_plot
#' @export
cov_distrib <- function(xpdb,
                        mapping  = NULL,
                        drop_static = TRUE,
                        type     = 'hr',
                        facets   = NULL,
                        title    = 'Continuous covariates distribution | @run',
                        subtitle = 'Based on @nind individuals',
                        caption  = '@dir',
                        log      = NULL,
                        guide    = FALSE,
                        problem,
                        quiet,
                        ...) {
  # Check input
  check_xpdb(xpdb, check = 'data')
  if (missing(problem)) problem <- default_plot_problem(xpdb)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  if (is.null(facets)) facets <- 'variable'
  
  cov_col <- xp_var(xpdb, problem, type = 'contcov')$col
  if (drop_static) {
    cov_col <- drop_static_cols(xpdb, problem, cols = cov_col, quiet = quiet)
  }
  if (is.null(cov_col)) {
    stop('No continuous covariate column found in the xpdb data index.', call. = FALSE)
  }
  
  xplot_distrib(xpdb = xpdb, quiet = quiet,
                opt = data_opt(problem = problem, 
                               filter = only_distinct(xpdb, problem, facets, quiet), 
                               tidy = TRUE, value_col = cov_col),
                mapping = aes_c(aes_string(x = 'value'), mapping), 
                type = type, guide = guide, facets = facets,
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]), ...)
}
