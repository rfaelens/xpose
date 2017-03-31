#' ggplot2 theme
#'
#' @description Theme intended to make ggplot2 more readable when used
#' in presentation or papers. Background and grid lines were dimed to bring the 
#' focus on the data.
#'
#' @param base_size base font size
#' @param legend.position the position of legends ("none", "left", "right", "bottom",
#' "top", or two-element numeric vector)
#'
#' @examples
#' \dontrun{
#' ggplot(diamonds, aes(x = price, y = carat, color = cut)) +
#'  geom_point(alpha = 0.5) +
#'  theme_readable(legend.position = 'top')
#' }
#' @export
#'
theme_readable <- function(base_size = 11, legend.position = 'right') {
  ggplot2::theme(
    # Text size
    text = ggplot2::element_text(size = base_size),
    
    # Title
    plot.title = ggplot2::element_text(size = ggplot2::rel(1.1), face = 'bold'),
    
    # Panel title
    strip.text = ggplot2::element_text(face = 'bold'),
    
    # Panel background color
    strip.background = ggplot2::element_rect(fill = 'grey88'),
    
    # Axis title
    axis.title = ggplot2::element_text(face = 'bold'),
    
    # Axis labels
    axis.text = ggplot2::element_text(size = ggplot2::rel(1.1), color = 'black'),
    
    # Legend
    legend.position   = legend.position,
    legend.key        = ggplot2::element_blank(),
    legend.background = ggplot2::element_blank(),
    
    # Background color
    panel.background = ggplot2::element_rect(color = NA, fill = 'grey95'),
    
    # Plot margin
    plot.margin = grid::unit(c(0.01 ,0.01, 0.01, 0.01), 'npc'),
    
    # Minor grid
    panel.grid.minor = ggplot2::element_blank(),
    
    # Major grid
    panel.grid.major = ggplot2::element_line(color = 'grey88', size = 0.25)
  )
}
