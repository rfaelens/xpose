#' Distribution plots of the ETA
#'
#' @description Histograms or density plots plots ETA.
#' 
#' @inheritParams dv_vs_pred
#' @inheritSection xplot_distrib Layers mapping
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_distrib}}
#' @examples
#' # Histogram
#' eta_distrib(xpdb_ex_pk, type = 'h')
#' 
#' # Density plot with a rug
#' eta_distrib(xpdb_ex_pk, type = 'dr')
#' 
#' @export
eta_distrib <- function(xpdb,
                        mapping  = NULL,
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
  if (is.null(facets)) facets <- 'variable'
  eta_prm <- xp_var(xpdb, problem, type = 'eta')$col
  
  if (is.null(eta_prm)) {
    msg('No eta value found in the xpdb.', FALSE)
    return()
  }
  
  xplot_distrib(xpdb = xpdb, 
                data_opt = data_opt_set(problem = problem, 
                                        filter = only_obs(xpdb, problem), tidy = TRUE, 
                                        value_col = xp_var(xpdb, problem, type = 'eta')$col),
                mapping = aes_c(aes_string(x = 'value'), mapping), 
                type = type, guides = guides, panel_facets = facets,
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]),
                guides_slope = 0, ...)
}
