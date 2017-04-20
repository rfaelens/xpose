parse_arg <- function(x = NULL, name) {
  
  if (is.null(x)) return(NULL)
  
  x <- x[grepl(paste0('^', name, '_'), names(x))]
  names(x) <- gsub(paste0(name, '_'), '', names(x))
  names(x) <- gsub('color', 'colour', names(x))
  x
}

update_args <- function(arg, name, ...) {
  arg     <- parse_arg(x = arg, name = name)
  usr_arg <- parse_arg(x = list(...), name = name)
  usr_changes <- intersect(names(arg), names(usr_arg))
  arg[usr_changes] <- usr_arg[usr_changes]
  arg <- append(arg, usr_arg[setdiff(names(usr_arg), names(arg))])
  arg
}

xp_map <- function(arg, mapping, ggfun) {
  arg  <- arg[!names(arg) %in% names(mapping)]
  x    <- do.call(ggfun, arg)
  if (!is.null(mapping)) { 
    x$mapping <- mapping
  }
  x
}

xp_geoms <- function(mapping = NULL, xp_theme, group = NULL, name, ggfun, ...) {
  
  if (!is.null(mapping)) {
    mapping <- parse_arg(mapping, name)
  }
  
  arg <- xp_theme[grepl(paste0('^', name, '_'), names(xp_theme))]
  arg <- update_args(arg, name, ...)
  out <- xp_map(arg, mapping, ggfun)
  
  # Add grouping
  if (ggfun == 'geom_line' && !is.null(group) && !'group' %in% names(out$mapping)) {
    out$mapping <- structure(append(out$mapping, aes_string(group = group)),
                             class = 'uneval')
  }
  out
}
