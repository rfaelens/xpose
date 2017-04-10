panel_layout <- function(plot = NULL, facets = NULL, nrow = 2, ncol = 2, scales = 'fixed') {

  if (is.null(plot)) {   # Check plot argument
    stop('Argument \"plot\" required', call. = FALSE)
  }

  if (is.null(facets)) {   # Check facets argument
    message('Argument \"facets\" not provided. Ploting single panel', call. = FALSE)
    return(plot)
  }

  if (!all(facets %in% colnames(plot$data))) { # Ensure facets exists
    stop(paste('The facets:', facets, 'could not be found in the data'), call. = FALSE)
  }

  if (is.null(ncol) | is.null(nrow)) {   # Check ncol and nrow arguments
    stop('Arguments \"ncol\" and \"nrow\" required', call. = FALSE)
  }

  # Get info on layout
  n_panel_tot <- nrow(unique(plot$data[, facets, drop = FALSE]))
  n_layout    <- ncol*nrow

  if (n_panel_tot > n_layout) {   # Check layout
    stop('nrow * ncol >= n is not TRUE, use \"facet_multiple()\" instead', call. = FALSE)
  }

  n_missing  <- n_layout - n_panel_tot
  nrow_last  <- max(which(n_panel_tot >= seq(1, n_layout, by = ncol)))
  panel_last <- n_panel_tot - ncol*(nrow_last - 1)

  if (n_missing == 0 || nrow_last == nrow & nrow != 1) {
    plot <- plot + facet_wrap(facets = facets, ncol = ncol, scales = scales)
    return(plot)
  }

  # Clean up factors
  plot$data[, 'ghosts'] <- factor(interaction(plot$data[, facets]))

  # Add ghost panels
  plot$data[, 'ghosts'] <- factor(x = plot$data[, 'ghosts'],
                                  levels = c(levels(plot$data[, 'ghosts']),
                                             paste0('ghost=', 1:n_missing)))

  plot <- plot + facet_wrap(facets = 'ghosts', ncol = ncol, drop = FALSE, scales = scales)

  drop_grobs <- function(sep = '', ...) {
    if (scales %in% c('free', 'free_x')) {
      tmp <- paste('axis_b', (n_panel_tot + 1):n_layout, sep = sep)
    }else if (panel_last < ncol) {
      tmp <- paste('axis_b', which((1:(nrow * ncol)) > (panel_last + (nrow - 1) * ncol)), sep = sep)
    } else {
      tmp <- NULL
    }
    tmp2 <- as.vector(outer(c('panel', 'strip_t', 'axis_l'), (n_panel_tot + 1):n_layout, paste, sep = sep))
    return(c(tmp, tmp2))
  }

  # Convert ggplot to grob
  g <- ggplotGrob(plot)

  # Remove empty panels
  g$grobs[names(g$grobs) %in% drop_grobs()] <- NULL

  # Remove unwanted axes
  g$layout <- g$layout[!g$layout$name %in% drop_grobs(sep = '-'),]

  # Move bottom axis closer to panels
  g$layout[g$layout$name %in%
             paste0('axis_b-', (1:panel_last) + ((nrow - 1) * ncol)), 'b'] <-
    seq(1, 40, by = 4)[nrow_last + 1]

  # Print the plot
  grid::grid.newpage()
  grid::grid.draw(g)

}
