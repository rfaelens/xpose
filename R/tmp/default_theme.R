xp_theme_default <- structure(list(
    # General
    rounding        = 1,

    # Guide
    guide_alpha     = 0.8,
    guide_color     = 'grey60',
    guide_linetype  = 'solid',
    guide_size      = 1,

    # Line
    line_alpha      =  0.8,
    line_color      = 'grey25',
    line_linetype   = 'solid',
    line_size       =  0.5,

    # Point
    point_alpha     = 0.8,
    point_color     = 'grey25',
    point_fill      = NA,
    point_shape     = 19,
    point_size      = 1.5,
    point_stroke    = 0.5,

    # Smooth
    smooth_alpha    = 0.4,
    smooth_color    = 'coral',
    smooth_fill     = 'coral',
    smooth_linetype = 1,
    smooth_method   = 'loess',
    smooth_se       = TRUE,
    smooth_size     = 1,
    smooth_weight   = 1,

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

    ),class = c('xpose_theme', 'uneval'))

save(xp_theme_default,file = 'data/xp_theme_default.RData')
