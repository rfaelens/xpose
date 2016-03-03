#' Create ggxpose theme
#'
#' @description Create a ggxpose theme.
#' This function will update the theme of an xpdb object.
#' All plots generated with this xpdb will automatically use the
#' defined ggxpose (\code{xp_theme}) and ggplot2 (\code{gg_theme}) themes.
#'
#' @param xpdb a ggxpose database object
#' @param gg_theme a ggplot2 complete theme object (eg. \code{theme_classic()})
#' @param xp_theme an vector of modifications of the ggxpose theme
#' (eg. \code{c(point_color = 'red', line_linetype = 'dashed')})
#'
#' @return An xpdb object
#' @examples
#' \dontrun{
#' xpdb <- xpose_theme(xpdb     = xpdb,
#'                     gg_theme = theme_bw(),
#'                     xp_theme = c(point_color = 'blue',
#'                                  line_color  = 'blue'))
#' }
#' @export
xpose_theme <- function(xpdb     = NULL,
                        gg_theme = NULL,
                        xp_theme = NULL) {

  # Check xpdb
  if (is.null(xpdb)) {
    stop('Argument \"xpdb\" required.')
  }

  # Assign gg_theme
  if (!is.null(gg_theme) && class(gg_theme)[1] == 'theme') {
    xpdb$gg_theme <- gg_theme
  }

  if (!is.null(gg_theme) && class(gg_theme)[1] != 'theme') {
    msg('Bad input for argument \"gg_theme\".', TRUE)
  }

  # Assign xp_theme
  if (!is.null(xp_theme) && !is.null(names(xp_theme))) {
    for (x in seq_along(xp_theme)) {
      xpdb$xp_theme[[names(xp_theme[x])]] <- xp_theme[[x]]
    }
  }

  if (!is.null(xp_theme) && is.null(names(xp_theme))) {
    msg('Bad input for argument \"xp_theme\".', TRUE)
  }

  return(xpdb)
}
