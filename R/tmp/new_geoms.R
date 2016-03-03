library(ggxpose)
xpdb    <- xpose_data(dir = 'inst/models/', runno = '037')

xp_geoms(mapping = NULL, group = 'ID', xp_theme = xpdb$xp_theme,
        name = 'abline', ggfun = 'geom_abline',
        abline_slope = 2, abline_color = 'red', abline_size = 1,
        line_color = 'blue', line_size = 999, line_linetype = 'dashed',
        point_alpha = 2)

arg <- update_args(arg, name,
            text_alpha =99, text_size = 999, text_color = 'pink', text_label = 'hello world',
            abline_color = 'red', abline_size = 1,line_color = 'blue', line_size = 999, line_linetype = 'dashed', point_alpha = 2)

xp_annotate_text(xp_theme = xpdb$xp_theme, name ='text', x = 1, y =2, text_alpha =2, text_size = 999, text_color = 'pink', label = 'hello world')

