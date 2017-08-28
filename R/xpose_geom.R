# Transform arguments from xpose to ggplot2 (e.g. point_color = to color = )
parse_arg <- function(x = NULL, name) {
  if (is.null(x)) return()
  x[stringr::str_detect(names(x), stringr::str_c('^', name, '_'))] %>% 
    purrr::set_names(nm = stringr::str_replace(names(.), stringr::str_c(name, '_'), '')) %>% 
    purrr::set_names(nm = stringr::str_replace(names(.), 'color', 'colour'))
}

# Combine the arguments from the user and xp_theme
update_args <- function(thm_arg, name, ...) {
  thm_arg <- parse_arg(x = thm_arg, name = name)
  usr_arg <- parse_arg(x = list(...), name = name)
  usr_changes <- intersect(names(thm_arg), names(usr_arg))
  thm_arg[usr_changes] <- usr_arg[usr_changes]
  c(thm_arg, usr_arg[!names(usr_arg) %in% names(thm_arg)])
}

# Call the ggplot2 function with its arguments
xp_map <- function(arg, mapping, ggfun) {
  x <- do.call(ggfun, arg[!names(arg) %in% names(mapping)])
  if (!is.null(mapping)) x$mapping <- mapping
  x
}

# Generic ggplot2 layer for xpose_plots
xp_geoms <- function(mapping = NULL, xp_theme, name, ggfun, ...) {
  if (!is.null(mapping)) mapping <- parse_arg(mapping, name)
  thm_arg <- filter_xp_theme(xp_theme, stringr::str_c('^', name, '_')) 
  arg     <- update_args(thm_arg, name, ...)
  xp_map(arg, mapping, ggfun)
}

# Generic panel function for xpose_plots
xpose_panels <- function(xp_theme, extra_args) {
  if (!is.formula(extra_args$facets)) {
    thm_arg  <- xp_theme[c('facets', 'nrow', 'ncol', 'scales', 'shrink', 
                           'labeller', 'as.table', 'drop', 'dir', 
                           'switch', 'strip.position', 'page')]
    facet_fun <- 'facet_wrap_paginate'
  } else {
    thm_arg  <- xp_theme[c('facets', 'margins', 'scales', 'space', 'shrink', 
                           'labeller', 'as.table', 'switch', 'drop', 'ncol', 
                           'nrow', 'page', 'byrow')]
    facet_fun <- 'facet_grid_paginate'
  }
  usr_changes <- intersect(names(thm_arg), names(extra_args))
  thm_arg[usr_changes] <- extra_args[usr_changes]
  do.call(facet_fun, thm_arg)
}
