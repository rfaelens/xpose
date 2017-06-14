#' Distribution plots of ETA and parameters
#'
#' @description Histograms and density plots of the ETA and parameter values.
#' 
#' @inheritParams dv_vs_pred
#' @inheritSection xplot_distrib Layers mapping
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_distrib}}
#' @examples
#' # Histogram of parameters
#' prm_distrib(xpdb_ex_pk, type = 'h')
#' 
#' # Density plot of etas with a rug
#' eta_distrib(xpdb_ex_pk, type = 'dr')
#' 
#' @export
prm_distrib <- function(xpdb,
                        mapping  = NULL,
                        type     = 'hr',
                        facets   = NULL,
                        title    = 'Parameter distribution | @run',
                        subtitle = '@nind individuals, @nobs observations',
                        caption  = '@dir',
                        log      = NULL,
                        guides   = FALSE,
                        problem,
                        ...) {
  if (missing(problem)) problem <- last_data_problem(xpdb, simtab = FALSE)
  if (is.null(facets)) facets <- 'variable'
  prm_col <- xp_var(xpdb, problem, type = 'param')$col
  
  if (is.null(prm_col)) {
    msg('No parameter column found in the xpdb data index.', FALSE)
    return()
  }
  
  xplot_distrib(xpdb = xpdb, 
                data_opt = data_opt_set(problem = problem, 
                                        filter = only_obs(xpdb, problem), 
                                        tidy = TRUE, value_col = prm_col),
                mapping = aes_c(aes_string(x = 'value'), mapping), 
                type = type, guides = guides, panel_facets = facets,
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]), ...)
}

#' @rdname prm_distrib
#' @export
eta_distrib <- function(xpdb,
                        mapping  = NULL,
                        type     = 'hr',
                        facets   = NULL,
                        title    = 'Eta distribution | @run',
                        subtitle = '@nind individuals, @nobs observations',
                        caption  = '@dir',
                        log      = NULL,
                        guides   = FALSE,
                        problem,
                        ...) {
  if (missing(problem)) problem <- last_data_problem(xpdb, simtab = FALSE)
  if (is.null(facets)) facets <- 'variable'
  eta_col <- xp_var(xpdb, problem, type = 'eta')$col
  
  if (is.null(eta_col)) {
    msg('No eta column found in the xpdb data index.', FALSE)
    return()
  }
  
  xplot_distrib(xpdb = xpdb, 
                data_opt = data_opt_set(problem = problem, 
                                        filter = only_obs(xpdb, problem), 
                                        tidy = TRUE, value_col = eta_col),
                mapping = aes_c(aes_string(x = 'value'), mapping), 
                type = type, guides = guides, panel_facets = facets,
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]), ...)
}

#' @param res Only used for \code{res_distrib}. Defines the type of residual to be used. Default is "CWRES".
#' @rdname prm_distrib
#' @export
res_distrib <- function(xpdb,
                        mapping  = NULL,
                        res      = 'CWRES',
                        type     = 'hr',
                        facets   = NULL,
                        title    = '@x distribution | @run',
                        subtitle = '@nind individuals, @nobs observations',
                        caption  = '@dir',
                        log      = NULL,
                        guides   = FALSE,
                        problem,
                        ...) {
  if (missing(problem)) problem <- last_data_problem(xpdb, simtab = FALSE)
  
  xplot_distrib(xpdb = xpdb, 
                data_opt = data_opt_set(problem = problem, 
                                        filter = only_obs(xpdb, problem)),
                mapping = aes_c(aes_string(x = toupper(res)), mapping), 
                type = type, guides = guides, panel_facets = facets,
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]), ...)
}
