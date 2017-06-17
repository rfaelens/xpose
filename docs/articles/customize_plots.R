## ---- include = FALSE----------------------------------------------------
library(xpose)

xpdb <- xpdb_ex_pk %>% vpc_data()

knitr::opts_chunk$set(fig.dpi = 96,
                      fig.align = 'center', 
                      fig.height = 4, 
                      fig.width = 4,
                      out.width = '50%',
                      comment = '')

## ----demo type scatter, echo = FALSE, fig.height = 6, fig.width = 6, out.width = '75%'----
gridExtra::grid.arrange(
  dv_vs_ipred(xpdb, title = "type = \'p\'", subtitle = NULL, caption = NULL, type = 'p'),
  dv_vs_ipred(xpdb, title = "type = \'l\'", subtitle = NULL, caption = NULL, type = 'l'),
  dv_vs_ipred(xpdb, title = "type = \'s\'", subtitle = NULL, caption = NULL, type = 's'),
  dv_vs_ipred(xpdb, title = "type = \'t\'", subtitle = NULL, caption = NULL, type = 't'),
  ncol = 2)

## ----demo type distrib, echo = FALSE, fig.height = 3.2, fig.width = 6, out.width = '75%'----
gridExtra::grid.arrange(
  res_distrib(xpdb, title = "type = \'h\'", subtitle = NULL, caption = NULL, type = 'h'),
  res_distrib(xpdb, title = "type = \'d\'", subtitle = NULL, caption = NULL, type = 'd') + labs(y = NULL),
  res_distrib(xpdb, title = "type = \'r\'", subtitle = NULL, caption = NULL, type = 'r'),
  ncol = 3)

## ----demo type vpc, echo = FALSE, fig.height = 6, fig.width = 6, out.width = '75%'----
gridExtra::grid.arrange(
  vpc(xpdb, title = "type = \'a\'", subtitle = NULL, caption = NULL, type = 'a') + theme(legend.position = 'none') ,
  vpc(xpdb, title = "type = \'l\'", subtitle = NULL, caption = NULL, type = 'l') + theme(legend.position = 'none'),
  vpc(xpdb, title = "type = \'p\'", subtitle = NULL, caption = NULL, type = 'p'),
  vpc(xpdb, title = "type = \'t\'", subtitle = NULL, caption = NULL, type = 't'),
  ncol = 2)

## ----demo titles---------------------------------------------------------
# Using template titles
dv_vs_ipred(xpdb,
            title    = '@y vs. @x (@run, obj: @ofv)',
            subtitle = 'Based on: @nind subjects and @nobs records',
            caption  = 'Dir: @dir')

# Disabling all titles
dv_vs_ipred(xpdb, title = NULL, subtitle = NULL, caption = NULL)

# Edit title suffix from the xp_theme for a specific plot
dv_vs_ipred(xpdb, title = 'A title', xp_theme = c(title_suffix = ' | a suffix for @run'))

## ----demo xpose_labs-----------------------------------------------------
dv_vs_ipred(xpdb) +
  labs(title    = '@descr',
       subtitle = NULL,
       caption  = 'Based on @nobs observations',
       x        = 'Individual model predictions (@x)',
       y        = 'Observations (@y) for @nind subjects')


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
dv_vs_ipred(xpdb, type = 'p', aes(point_color = SEX))

## ----demo panels, fig.width = 6, fig.height = 6, out.width = '75%'-------
# Example with a string
dv_vs_ipred(xpdb, facets = c('SEX', 'OCC'))

# Example with a formula
dv_vs_ipred(xpdb, facets = SEX~OCC, panel_labeller = 'label_both')

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
            xscale_name = 'Individual model predictions')

## ----demo themes xpdb, eval = FALSE--------------------------------------
#  # While creating the xpdb
#  xpdb <- xpose_data(runno = '001',
#                     gg_theme = theme_classic(),
#                     xp_theme = theme_xp_xpose4())
#  
#  # Update a pre-existing xpdb
#  xpdb <- update_themes(xpdb     = xpdb,
#                        gg_theme = theme_bw(),
#                        xp_theme = c(point_color = 'dodgerblue4',
#                                     line_color  = 'dodgerblue4'))

## ----demo gg_theme, echo = FALSE, fig.height = 6, fig.width = 6, out.width = '75%'----
gridExtra::grid.arrange(
  dv_vs_ipred(xpdb, subtitle = 'theme_grey() [default in ggplot2]', title = NULL, caption = '') + theme_grey(),
  dv_vs_ipred(xpdb, subtitle = 'theme_readable() [default in xpose]', title = NULL, caption = '') + theme_readable(),
  dv_vs_ipred(xpdb, subtitle = 'theme_bw2()', title = NULL, caption = NULL) + theme_bw2(),
  dv_vs_ipred(xpdb, subtitle = 'theme_dark()', title = NULL, caption = NULL) + theme_dark(),
  ncol = 2)

## ----demo xp_theme, echo = FALSE, fig.height = 3.2, fig.width = 6, out.width = '75%'----
gridExtra::grid.arrange(
  dv_vs_ipred(update_themes(xpdb = xpdb, xp_theme = theme_xp_default()),
              subtitle = 'xp_theme = theme_xp_default()\nwith theme_bw2()', title = NULL, caption = '') + theme_bw2(),
  dv_vs_ipred(update_themes(xpdb = xpdb, xp_theme = theme_xp_xpose4()),
              subtitle = 'xp_theme = theme_xp_xpose4()\nwith theme_bw2()', title = NULL, caption = '') + theme_bw2(),
  ncol = 2)

