#' Modify labels using template titles
#'
#' @description Function based on ggplot2 \link[ggplot2]{labs} enabling the 
#' use of \code{\link{template_titles}} along with the ggplot2 \code{+} operator.
#'
#' @param ... A list of new name-value pairs. The name should either be an aesthetic, 
#' or one of "title", "subtitle", or "caption". The value should be a text containing 
#' \code{\link{template_titles}} keywords for the axes, plot title, subtitle or caption.
#' @export
#' @seealso \link[ggplot2]{ggplot2}, \link[ggplot2]{labs}, \link[ggplot2]{+.gg}
#' @examples
#' \dontrun{
#' dv_vs_ipred(xpdb_ex_pk) + 
#'   xpose_labs(title = 'DV vs. IPRED | @run', 
#'              caption = '@dir', 
#'              x = 'Individual Predictions (IPRED)')
#' }
xpose_labs <- function(...) {
  args <- list(...)
  rename_aes <- utils::getFromNamespace('rename_aes', 'ggplot2')
  if (is.list(args[[1]])) 
    args <- args[[1]]
  args <- rename_aes(args)
  structure(args, class = "xpose_labels")
}

#' @describeIn xpose_labs add an \code{xpose_labs} layer to an xpose plot.
#' @param e1 An object of class \code{\link{ggplot}}, \code{xpose_plot}, or a \code{\link{theme}}.
#' @param e2 A plot component.
#' @method + gg
#' @export
"+.gg" <- function(e1, e2) {
  # Import internal ggplot2 functions
  add_theme <- utils::getFromNamespace('add_theme', 'ggplot2')
  add_ggplot <- utils::getFromNamespace('add_ggplot', 'ggplot2')
  e2name <- deparse(substitute(e2))
  if (ggplot2::is.theme(e1)) { 
    add_theme(e1, e2, e2name)
  } else if (ggplot2::is.ggplot(e1)) {
    if (is.xpose.plot(e1) & inherits(e2, 'xpose_labels')) {
      update_xpose_labels(e1, e2)
    } else {
      add_ggplot(e1, e2, e2name)
    }
  } else if (ggplot2::is.ggproto(e1)) {
    stop("Cannot add ggproto objects together.",
         " Did you forget to add this object to a ggplot object?",
         call. = FALSE)
  }
}

#' Update axis/legend labels for xpose plots
#'
#' @description  Function based on \link[ggplot2]{update_labels} enabling the 
#' use of \code{\link{template_titles}} using the ggplot2 \code{+} operator.
#' @param p xpose plot object to modify
#' @param labels named list of new labels
#' 
update_xpose_labels <- function(p, labels) {
  plot_clone <- utils::getFromNamespace('plot_clone', 'ggplot2')
  p <- plot_clone(p)
  if (!is.xpose.plot(p)) stop('This function is dedicated to xpose plots')
  labels <- purrr::map(labels, ~parse_title(., list(summary = p$xpose$summary)))
  p$labels <- c(labels,  p$labels[setdiff(names(p$labels), names(labels))])
  p
}
