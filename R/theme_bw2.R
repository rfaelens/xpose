#' ggplot2 theme
#'
#' @description Black and whithe theme intended to make ggplot2 more readable when used
#' in presentation or papers. Inspired by a theme from Gunnar Yngman.
#'
#' @param base_size base font size
#' @param base_family base font family
#' @param legend_position the position of legends ("none", "left", "right", "bottom",
#' "top", or two-element numeric vector)
#'
#' @examples
#' \dontrun{
#' ggplot(diamonds, aes(x = price, y = carat, color = cut)) +
#'  geom_point(alpha = 0.5) +
#'  theme_bw2(legend_position = 'top')
#' }
#' @export
#'
theme_bw2 <- function(base_size = 11, base_family = '', legend_position = 'right') {
  theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    theme(plot.title = element_text(face = 'bold', size = rel(1.2), hjust = 0, 
                                    vjust = 1, margin = margin(b = base_size/2 * 1.2)),
          strip.text = element_text(face = 'bold', color = 'white'),
          strip.background = element_rect(color = 'black', fill = 'black'),
          axis.title = element_text(face = 'bold'),
          legend.position = legend_position,
          legend.key = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = 'grey90', size = 0.25),
          plot.margin = grid::unit(c(0.01 ,0.01, 0.01, 0.01), 'npc'),
          panel.background = element_rect(color = 'black', fill = 'white', size = 0.1),
          complete = TRUE)
}
