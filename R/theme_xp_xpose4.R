#' Xpose4 theme for xpose
#'
#' @description An xp_theme that makes xpose look like xpose4.
#'
#' @examples
#' # Make xpose look like xpose4
#' \dontrun{
#'  xpose_data(dir = 'inst/models/', runno = '001') %>%
#'    xpose_theme(gg_theme = theme_bw(),
#'                xp_theme = theme_xp_xpose4()) %>%
#'    dv_vs_ipred()
#' }
#' @export
theme_xp_xpose4 <- function() {
  structure(list(
    
    # General
    rounding        = 1,
    
    # Guide
    guides_alpha    = 1,
    guides_color    = 'black',
    guides_linetype = 'solid',
    guides_size     = 0.5,
    
    # Line
    line_alpha      =  1,
    line_color      = 'blue',
    line_linetype   = 'solid',
    line_size       =  0.5,
    
    # Point
    point_alpha     = 1,
    point_color     = 'blue',
    point_fill      = NA,
    point_shape     = 1,
    point_size      = 2,
    point_stroke    = 0.5,
    
    # Smooth
    smooth_alpha    = 0.4,
    smooth_color    = 'red',
    smooth_fill     = 'red',
    smooth_linetype = 1,
    smooth_method   = 'loess',
    smooth_se       = FALSE,
    smooth_size     = 1,
    smooth_weight   = 1,
    
    # Panel
    panel_ncol      = NULL,
    panel_nrow      = NULL,
    panel_scales    = 'fixed',
    panel_shrink    = TRUE,
    panel_labeller  = 'label_value',
    panel_as.table  = TRUE,
    panel_switch    = NULL,
    panel_drop      = TRUE,
    panel_dir       = 'h',
    
    # Text
    text_alpha      = 1,
    text_angle      = 0,
    text_color      = 'grey20',
    text_family     = '',
    text_fontface   = 'bold',
    text_hjust      = -0.07,
    text_lineheight = 1.2,
    text_size       = 3.88,
    text_vjust      = 2.5
    
  ), class = c('xpose_theme', 'uneval'))
}
