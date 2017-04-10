## ---- include = FALSE----------------------------------------------------
library(ggxpose)

xpdb <- xpdb_ex_pk

knitr::opts_chunk$set(fig.dpi = 96,
                      fig.align = 'center', 
                      fig.height = 4, 
                      fig.width = 4,
                      out.width = '75%')

## ----demo type, echo = FALSE, fig.height = 2.5, fig.width = 6, out.width = '100%'----
gridExtra::grid.arrange(
  dv_vs_ipred(xpdb, title = "type = \'p\'", subtitle = FALSE, caption = FALSE, type = 'p'),
  dv_vs_ipred(xpdb, title = "type = \'l\'", subtitle = FALSE, caption = FALSE, type = 'l'),
  dv_vs_ipred(xpdb, title = "type = \'s\'", subtitle = FALSE, caption = FALSE, type = 's'),
  ncol = 3)

## ----demo titles---------------------------------------------------------
# Using template titles
dv_vs_ipred(xpdb,
            title    = 'DV vs. IPRED (@run, obj: @ofv)',
            subtitle = 'Based on: @nind subjects and @nobs records',
            caption  = 'Dir: @dir')

# Disabling all titles
dv_vs_ipred(xpdb, title = FALSE, subtitle = FALSE, caption = FALSE)

## ----demo aesthetics-----------------------------------------------------
dv_vs_ipred(xpdb, 
            # Change points aesthetics
            point_color = 'blue', point_alpha = 0.5, 
            point_stroke = 0, point_size = 1.5, 
            # Change lines aesthetics 
            line_alpha = 0.5, line_size = 0.5, 
            line_color = 'orange', line_linetype = 'solid', 
            # Change smooth aesthetics
            smooth_method = 'lm')

## ----demo mapping--------------------------------------------------------
dv_vs_ipred(xpdb, type = 'p',
            ggplot2::aes(point_color = as.factor(SEX))) 

## ----demo panels---------------------------------------------------------
dv_vs_ipred(xpdb, by = 'SEX')

## ----demo layers---------------------------------------------------------
dv_vs_ipred(xpdb) +
  geom_rug(alpha = 0.2, color = 'grey50',
           sides = 'lb', size = 0.4) +
  annotate(geom = 'text', 
           color = 'darkred',
           label = 'LLOQ', x = -4, y = -4) +
  annotate(geom = 'rect',
           alpha = 0.2, fill = 'red',
           xmin = -Inf, xmax = -3,
           ymin = -Inf, ymax = -3)

## ----scales demo---------------------------------------------------------
dv_vs_ipred(xpdb, 
            xscale_breaks = c(-4, -2, 0),
            xscale_labels = c('Low', 'Med', 'High'),
            xscale_expand = c(0.2, 0),
            xscale_name = 'Individual model prediction')

## ----demo themes xpdb, eval = FALSE--------------------------------------
#  # While creating the xpdb
#  xpdb <- xpose_data(runno = '001',
#                     gg_theme = theme_classic(),
#                     xp_theme = xp_theme_xpose4())
#  
#  # Update a pre-existing xpdb
#  xpdb <- xpose_theme(xpdb     = xpdb,
#                      gg_theme = theme_bw(),
#                      xp_theme = c(point_color = 'dodgerblue4',
#                                   line_color  = 'dodgerblue4'))

## ----demo themes plot----------------------------------------------------
# For a single plot only
dv_vs_ipred(xpdb        = xpdb, 
            gg_theme    = theme_readable(), 
            point_color = 'darkblue')

## ----demo gg_theme, echo = FALSE, fig.height = 6, fig.width = 6, out.width = '100%'----
gridExtra::grid.arrange(
  dv_vs_ipred(xpdb, subtitle = 'gg_theme = theme_readable()', title = FALSE, caption = '', gg_theme = theme_readable()),
  dv_vs_ipred(xpdb, subtitle = 'gg_theme = theme_grey()', title = FALSE, caption = '', gg_theme = theme_grey()),
  dv_vs_ipred(xpdb, subtitle = 'gg_theme = theme_bw2()', title = FALSE, caption = FALSE, gg_theme = theme_bw2()),
  dv_vs_ipred(xpdb, subtitle = 'gg_theme = theme_dark()', title = FALSE, caption = FALSE, gg_theme = theme_dark()),
  ncol = 2)

## ----demo xp_theme, echo = FALSE, fig.height = 4, fig.width = 6, out.width = '100%'----
gridExtra::grid.arrange(
  dv_vs_ipred(xpose_theme(xpdb = xpdb, xp_theme = xp_theme_default()),
              subtitle = 'xp_theme = xp_theme_default()', title = FALSE, caption = ''),
  dv_vs_ipred(xpose_theme(xpdb = xpdb, xp_theme = xp_theme_xpose4()),
              subtitle = 'xp_theme = xp_theme_xpose4()\nwith gg_theme = theme_bw2()', title = FALSE, caption = '', gg_theme = theme_bw2()),
  ncol = 2)

