#' Allows \code{facet_wrap()} to work on multiple pages
#'
#' @description Allows \code{ggplot2} panels to be plotted over multiple pages.
#'
#' @param plot a ggplot object
#' @param by variables to facet by
#' @param nrow number of rows
#' @param ncol number of columns
#' @param scales should scales be fixed ("fixed", the default), free ("free"),
#' or free in one dimension ("free_x", "free_y")
#'
#' @seealso \code{\link{facet_wrap}}
#' @examples
#' \dontrun{
#' p <- ggplot(diamonds, aes(x = price, y = carat, color = cut)) +
#' geom_point(alpha = 0.5) +
#' labs(x = 'Price', y = 'Carat', title = 'Diamonds')
#'
#' multiple_pages(plot = p, by = 'color', ncol = 2, nrow = 2)
#' }
#' @importFrom ggplot2 %+%
#' @export
#'
multiple_pages <- function(plot = NULL, by = NULL, ncol = 2, nrow = 2, scales = 'fixed') {

  if (is.null(plot)) {   # Check plot argument
    stop('Argument \"plot\" required', call. = FALSE)
  }

  if (is.null(by)) {   # Check by argument
    message('Argument \"by\" not provided. Ploting single panel')
    return(plot)
  }

  if (!all(by %in% colnames(plot$data))) {   # Ensure by exists
    stop(paste('The by:', by, 'could not be found in the data'), call. = FALSE)
  }

  if (is.null(ncol) | is.null(nrow)) {   # Check ncol and nrow arguments
    stop('Arguments \"ncol\" and \"nrow\" required', call. = FALSE)
  }

  # Get info on layout
  n_panel_tot <- nrow(unique(plot$data[, by, drop = FALSE]))
  n_layout    <- ncol*nrow
  n_pages     <- ceiling(n_panel_tot/n_layout)
  plot        <- plot + ggplot2::facet_wrap(facets = by, ncol = ncol, scales = scales)

  # When no multiple page needed
  if (n_pages == 1) {
    return(plot)
  }

  # Extract ggplot2 data and title
  data   <- plot$data
  title  <- plot$labels$title

  # Work with the scales
  if (!scales %in% c('free', 'free_x') &&                             # if scale fixed on x
     is.numeric(eval(plot$mapping$x, data)) &&                       # and x is numeric
     length(grep('xmax', plot$scales$scales, fixed = TRUE)) == 0) {  # and x-scale hasn't been defined in ggplot2
    plot$coordinates$limits$x <- range(eval(plot$mapping$x, data))
  }

  if (!scales %in% c('free', 'free_y') &&                             # if scale fixed on y
     is.numeric(eval(plot$mapping$y, data)) &&                       # and y is numeric
     length(grep('ymax', plot$scales$scales, fixed = TRUE)) == 0) {  # and y-scale hasn't been defined in ggplot2
    plot$coordinates$limits$y <- range(eval(plot$mapping$y, data))
  }

  # Prepare the grouping
  data$groups <- findInterval(unclass(interaction(data[,by])),
                              seq(from = 1, by = n_layout, length.out = n_pages)[-1]) + 1

  # Plot each page
  for (i in seq_along(1:n_pages)) {
    plot <- plot %+% data[data$groups == i,] +
      ggplot2::ggtitle(label = bquote(atop(bold(.(title)), atop(italic(Page~.(i)~of~.(n_pages))))))

    # For last page call panel_layout
    if (i == n_pages) {
      plot <- panel_layout(plot = plot, facets = by, ncol = ncol, nrow = nrow, scales = scales)
    }

    # Print plots
    if (!is.null(plot)) {
      print(plot)
    }
  } # End for loop

}
