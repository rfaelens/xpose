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
  theme(
    # Text size
    text = element_text(size = base_size),
    
    # Title
    plot.title = element_text(size = rel(1.1), face = 'bold'),
    
    # Panel title
    strip.text = element_text(face = 'bold'),
    
    # Panel background color
    strip.background = element_rect(fill = 'grey88'),
    
    # Axis title
    axis.title = element_text(face = 'bold'),
    
    # Axis labels
    axis.text = element_text(size = rel(1.1), color = 'black'),
    
    # Legend
    legend.position   = legend.position,
    legend.key        = element_blank(),
    legend.background = element_blank(),
    
    # Background color
    panel.background = element_rect(color = NA, fill = 'grey95'),
    
    # Plot margin
    plot.margin = grid::unit(c(0.01 ,0.01, 0.01, 0.01), 'npc'),
    
    # Minor grid
    panel.grid.minor = element_blank(),
    
    # Major grid
    panel.grid.major = element_line(color = 'grey88', size = 0.25)
  )
}
