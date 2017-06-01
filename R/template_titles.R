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
#'  \item{@condn}{Condition number}
#'  \item{@covtime}{Covariance matrix runtime}
#'  \item{@data}{Model input data used}
#'  \item{@descr}{Model description}
#'  \item{@dir}{Model directory}
#'  \item{@epsshk}{Epsilon shrinkage}
#'  \item{@errors}{Run errors (e.g termination error)}
#'  \item{@esampleseed}{ESAMPLE seed number (used in NPDE)}
#'  \item{@etashk}{Eta shrinkage}
#'  \item{@file}{Model file name}
#'  \item{@label}{Model label}
#'  \item{@method}{Estimation method or sim}
#'  \item{@nesample}{Number of ESAMPLE (used in NPDE)}
#'  \item{@nind}{Number of individuals}
#'  \item{@niter}{Number of iteration}
#'  \item{@nobs}{Number of observations}
#'  \item{@nsig}{Number of significant digits}
#'  \item{@nsim}{Number of simulations}
#'  \item{@ofv}{Objective function value}
#'  \item{@probn}{Problem number}
#'  \item{@plotfun}{Name of the plot function. Can only be used in the filename argument of \code{\link{xpose_save}}}
#'  \item{@ref}{Reference model}
#'  \item{@run}{Model run name}
#'  \item{@runtime}{Estimation/Sim runtime}
#'  \item{@software}{Software used (e.g. NONMEM)}
#'  \item{@simseed}{Simulation seed}
#'  \item{@subroutine}{Differencial equation solver}
#'  \item{@version}{Software version (e.g. 7.3)}
#'  \item{@warnings}{Run warnings (e.g. boundary)}
#' }
#' @seealso \link{xpose_save}
#' @examples
#' # Defined when creating a plot
#' dv_vs_ipred(xpdb_ex_pk, 
#'             title = 'DV vs. IPRED',
#'             subtitle = '@ofv, @nind subjects, @nobs obs.',
#'             caption = '@run, @descr')
#'             
#' # Any label can be modified later on
#' dv_vs_ipred(xpdb_ex_pk, aes(point_color = SEX, 
#'                             line_color = SEX)) + 
#'  labs(title = 'This runs is: @descr', 
#'       color = 'Color scale for @run',
#'       x = 'IPRED for @nind subjects',
#'       subtitle = NULL)
#' 
#' @name template_titles
NULL
