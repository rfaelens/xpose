#' Template titles
#'
#' @description Template titles can be used to create highly informative diagnostics plots. 
#' They can be applied to any plot title, subtitle, caption and the filename when saving 
#' with the \code{\link{xpose_save}} function. 
#' 
#' Template titles are defined via a single string containing key variables staring 
#' with a @ (e.g. @ofv) which will be replaced by their actual value when rendering the plot.
#' For example '@run, @nobs observations in @nind subjects' would become 'run001, 
#' 1022 observations in 74 subjects'
#' 
#' Many key variables are available:
#' \describe{
#'  \item{@descr}{Model description}
#'  \item{@dir}{Model directory}
#'  \item{@run}{Model file name}
#'  \item{@ref}{Reference model}
#'  \item{@input_dat}{Model input data used}
#'  \item{@nobs}{Number of observations}
#'  \item{@nind}{Number of individuals}
#'  \item{@nsim}{Number of simulations}
#'  \item{@ssim}{Simulation seed}
#'  \item{@niter}{Number of iteration}
#'  \item{@software}{Software used (e.g. NONMEM)}
#'  \item{@version}{Software version (e.g. 7.3)}
#'  \item{@subroutine}{Differencial equation solver}
#'  \item{@runtime}{Estimation/Sim runtime}
#'  \item{@covtime}{Covariance matrix runtime}
#'  \item{@warnings}{Run warnings (e.g. boundary)}
#'  \item{@errors}{Run errors (e.g termination error)}
#'  \item{@nsig}{Number of significant digits}
#'  \item{@condition}{Condition number}
#'  \item{@nnpde}{Number of NPDE}
#'  \item{@snpde}{NPDE seed number}
#'  \item{@ofv}{Objective function value}
#'  \item{@method}{Estimation method or sim}
#'  \item{@eps_shrink}{Epsilon shrinkage}
#'  \item{@eta_shrink}{Eta shrinkage}
#'  \item{@plotfun}{Name of the plot function. Can only be used in the filename argument of \code{\link{xpose_save}}}
#' }
#' 
#' @examples
#' \dontrun{
#' dv_vs_ipred(xpdb, 
#'             title = 'DV vs. IPRED',
#'             subtitle = '@ofv, @nind subjects, @nobs obs.',
#'             caption = '@run, ©descr')
#' }
#' 
#' @name template_titles
NULL
