#' Create ggxpose theme
#'
#' @description Create a ggxpose theme.
#' This function will update the theme of an xpdb object.
#' All plots generated with this xpdb will automatically use the
#' defined ggxpose (\code{xp_theme}) and ggplot2 (\code{gg_theme}) themes.
#'
#' @param xpdb a ggxpose database object
#' @param gg_theme a ggplot2 complete theme object (eg. \code{theme_classic()})
#' @param xp_theme a ggxpose theme or vector of modifications of the ggxpose theme
#' (eg. \code{c(point_color = 'red', line_linetype = 'dashed')})
#'
#' @return An xpdb object
#' @examples
#' \dontrun{
#' xpdb <- update_themes(xpdb     = xpdb,
#'                     gg_theme = theme_bw(),
#'                     xp_theme = c(point_color = 'blue',
#'                                  line_color  = 'blue'))
#' }
#' @export
update_themes <- function(xpdb     = NULL,
                          gg_theme = NULL,
                          xp_theme = NULL) {
  
  # Check xpdb
  if (is.null(xpdb)) {
    stop('Argument `xpdb` required.', call. = FALSE)
  }
  
  # Replace/Update gg_theme
  if (!is.null(gg_theme) && is.theme(gg_theme)) {
    if (attr(gg_theme, 'complete')) {
      xpdb$gg_theme <- gg_theme
    } else {
      xpdb$gg_theme <- xpdb$gg_theme + gg_theme
    }  
  }
  
  if (!is.null(gg_theme) && !is.theme(gg_theme)) {
    msg('Bad input for argument `gg_theme`.', TRUE)
  }
  
  # Replace/Update xp_theme
  if (!is.null(xp_theme)) { 
    if (inherits(xp_theme, 'xpose_theme')) {
      xpdb$xp_theme <- xp_theme
      
    } else if (!is.null(names(xp_theme))) {
      for (x in seq_along(xp_theme)) {
        # Beware drops NULL elements
        xpdb$xp_theme[[names(xp_theme[x])]] <- xp_theme[[x]] 
      }
    } else {
      msg('Bad input for argument `xp_theme`.', TRUE)
    }
  }
  return(xpdb)
}


