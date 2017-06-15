#' QQ plots of ETA and residuals
#'
#' @description QQ plots of the ETA and model residuals.
#' 
#' @inheritParams dv_vs_pred
#' @param type String setting the type of plot to be points 'p'.
#' @param guides Should the guides (e.g. reference line) be displayed.
#' 
#' @inheritSection xplot_qq Layers mapping
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_distrib}}
#' @examples
#' # QQ plot of parameters
#' prm_qq(xpdb_ex_pk)
#' 
#' # QQ plot of eta
#' eta_qq(xpdb_ex_pk)
#' 
#' # QQ plot of residuals
#' res_qq(xpdb_ex_pk, res = c('IWRES', 'CWRES'))
#' 
#' # QQ plot of continuous covariates
#' cov_qq(xpdb_ex_pk)
#' 
#' @name qq_plot
#' @export
prm_qq <- function(xpdb,
                   mapping  = NULL,
                   type     = 'p',
                   facets   = NULL,
                   title    = 'QQ plot of parameters | @run',
                   subtitle = 'Based on @nind individuals',
                   caption  = '@dir',
                   log      = NULL,
                   guides   = TRUE,
                   problem,
                   quiet,
                   ...) {
  if (missing(problem)) problem <- last_data_problem(xpdb, simtab = FALSE)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  if (is.null(facets)) facets <- 'variable'
  
  prm_col <- xp_var(xpdb, problem, type = 'param')$col
  if (is.null(prm_col)) {
    msg('No parameter column found in the xpdb data index.', quiet)
    return()
  }
  
  xplot_qq(xpdb = xpdb, quiet = quiet,
           data_opt = data_opt_set(problem = problem, 
                                   filter = only_distinct(xpdb, problem, facets, quiet), 
                                   tidy = TRUE, value_col = prm_col),
           mapping = aes_c(aes_string(sample = 'value'), mapping), 
           type = type, guides = guides, panel_facets = facets,
           xscale = check_scales('x', log), 
           yscale = check_scales('y', log), 
           title = title, subtitle = subtitle, caption = caption,
           plot_name = as.character(match.call()[[1]]), 
           xscale_name = 'Quantiles of normal', 
           yscale_name = 'Quantiles of parameter', ...)
}

#' @rdname qq_plot
#' @export
eta_qq <- function(xpdb,
                   mapping  = NULL,
                   type     = 'p',
                   facets   = NULL,
                   title    = 'QQ plot of etas | @run',
                   subtitle = 'Based on @nind individuals, Eta shrink: @etashk',
                   caption  = '@dir',
                   log      = NULL,
                   guides   = TRUE,
                   problem,
                   quiet,
                   ...) {
  if (missing(problem)) problem <- last_data_problem(xpdb, simtab = FALSE)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  if (is.null(facets)) facets <- 'variable'
  eta_col <- xp_var(xpdb, problem, type = 'eta')$col
  
  if (is.null(eta_col)) {
    msg('No eta column found in the xpdb data index.', quiet)
    return()
  }
  
  xplot_qq(xpdb = xpdb, quiet = quiet,
           data_opt = data_opt_set(problem = problem, 
                                   filter = only_distinct(xpdb, problem, facets, quiet), 
                                   tidy = TRUE, value_col = eta_col),
           mapping = aes_c(aes_string(sample = 'value'), mapping), 
           type = type, guides = guides, panel_facets = facets,
           xscale = check_scales('x', log), 
           yscale = check_scales('y', log), 
           title = title, subtitle = subtitle, caption = caption,
           plot_name = as.character(match.call()[[1]]), 
           xscale_name = 'Quantiles of normal', 
           yscale_name = 'Quantiles of etas', ...)
}

#' @param res Only used for \code{res_qq}. Defines the type of residual to be used. Default is "CWRES".
#' @rdname qq_plot
#' @export
res_qq <- function(xpdb,
                   mapping  = NULL,
                   res      = 'CWRES',
                   type     = 'p',
                   facets   = NULL,
                   title    = 'QQ plot of @sample | @run',
                   subtitle = 'Based on @nind individuals',
                   caption  = '@dir',
                   log      = NULL,
                   guides   = TRUE,
                   problem,
                   quiet,
                   ...) {
  if (missing(problem)) problem <- last_data_problem(xpdb, simtab = FALSE)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  if (length(res) > 1) {
    if (is.null(facets)) facets <- 'variable'
    data_opt <- data_opt_set(problem = problem, 
                             filter = only_obs(xpdb, problem, quiet),
                             tidy = TRUE, value_col = res)
    vars <- aes_c(aes_string(sample = 'value'), mapping)
  } else {
    data_opt <- data_opt_set(problem = problem, 
                             filter = only_obs(xpdb, problem, quiet))
    vars <- aes_c(aes_string(sample = toupper(res)), mapping)
  }
  
  xplot_qq(xpdb = xpdb, quiet = quiet,
           data_opt = data_opt, mapping = vars,
           type = type, guides = guides, panel_facets = facets,
           xscale = check_scales('x', log), 
           yscale = check_scales('y', log), 
           title = title, subtitle = subtitle, caption = caption,
           plot_name = as.character(match.call()[[1]]), ...) +
    labs(x = 'Quantiles of normal', 
         y = 'Quantiles of @sample')
}

#' @name qq_plot
#' @export
cov_qq <- function(xpdb,
                   mapping  = NULL,
                   type     = 'p',
                   facets   = NULL,
                   title    = 'QQ plot of continuous covariates | @run',
                   subtitle = 'Based on @nind individuals',
                   caption  = '@dir',
                   log      = NULL,
                   guides   = TRUE,
                   problem,
                   quiet,
                   ...) {
  if (missing(problem)) problem <- last_data_problem(xpdb, simtab = FALSE)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  if (is.null(facets)) facets <- 'variable'
  cov_col <- xp_var(xpdb, problem, type = 'contcov')$col
  
  if (is.null(cov_col)) {
    msg('No continuous covariate column found in the xpdb data index.', quiet)
    return()
  }
  
  xplot_qq(xpdb = xpdb, quiet = quiet,
           data_opt = data_opt_set(problem = problem, 
                                   filter = only_distinct(xpdb, problem, facets, quiet), 
                                   tidy = TRUE, value_col = cov_col),
           mapping = aes_c(aes_string(sample = 'value'), mapping), 
           type = type, guides = guides, panel_facets = facets,
           xscale = check_scales('x', log), 
           yscale = check_scales('y', log), 
           title = title, subtitle = subtitle, caption = caption,
           plot_name = as.character(match.call()[[1]]), 
           xscale_name = 'Quantiles of normal', 
           yscale_name = 'Quantiles of covariates', ...)
}
