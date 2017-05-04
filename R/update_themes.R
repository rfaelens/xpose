#' Create xpose theme
#'
#' @description Create an xpose theme.
#' This function will update the theme of an xpdb object.
#' All plots generated with this xpdb will automatically use the
#' defined xpose (\code{xp_theme}) and ggplot2 (\code{gg_theme}) themes.
#'
#' @param xpdb a xpose database object
#' @param gg_theme a ggplot2 complete theme object (eg. \code{\link[ggplot2]{theme_classic()}})
#' @param xp_theme a xpose theme or vector of modifications of the xpose theme
#' (eg. \code{c(point_color = 'red', line_linetype = 'dashed')})
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#'
#' @return An xpdb object
#' @examples
#' \dontrun{
#' xpdb <- update_themes(xpdb     = xpdb,
#'                       gg_theme = theme_bw(),
#'                       xp_theme = c(point_color = 'blue',
#'                                    line_color  = 'blue'))
#' }
#' @export
update_themes <- function(xpdb     = NULL,
                          gg_theme = NULL,
                          xp_theme = NULL,
                          quiet) {
  
  if (!is.xpdb(xpdb)) {
    stop('Valid `xpdb` input required.', call. = FALSE)
  }
  
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  # Replace/Update gg_theme
  if (is.theme(gg_theme)) {
    if (attr(gg_theme, 'complete')) {
      xpdb$gg_theme <- gg_theme
    } else {
      xpdb$gg_theme <- xpdb$gg_theme + gg_theme
    }  
  } else if (!is.null(gg_theme) & !is.theme(gg_theme)) {
    msg('`gg_theme` argument not used. Reason: invalid input.', quiet)
  }
  
  # Replace/Update xp_theme
  if (!is.null(xp_theme)) { 
    if (is.xp.theme(xp_theme)) {
      xpdb$xp_theme <- xp_theme
      
    } else if (!is.null(names(xp_theme))) {
      for (x in seq_along(xp_theme)) {
        xpdb$xp_theme[[names(xp_theme[x])]] <- xp_theme[[x]] # Beware drops NULL elements
      }
    } else {
      msg('`xp_theme` argument not used. Reason: invalid input.', quiet)
    }
  }
  xpdb
}


