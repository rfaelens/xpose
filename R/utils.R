# Reports whether x is an xpose theme object
is.xp.theme <- function(x) {
  inherits(x, 'xpose_theme')
}

# Reports whether x is an xpose data object
is.xpdb <- function(x) {
 inherits(x, 'xpose_data')
}
