library(ggxpose)
library(gridExtra)

xpdb <- xpose_data(dir = 'inst/models/', runno = '037')

# Default -----------------------------------------------------------------
P1_A <- dv_vs_ipred(xpdb)
P1_B <- dv_vs_pred(xpdb)

jpeg('./inst/img/default.jpg', width = 8, height = 3.5, units = 'in', res = 80)
grid.arrange(P1_A, P1_B, ncol = 2)
dev.off()


# Plot type ---------------------------------------------------------------
P2_A <- dv_vs_ipred(xpdb, type = 'p', title = "type = 'p'" , subtitle = FALSE)
P2_B <- dv_vs_ipred(xpdb, type = 'l', title = "type = 'l'" , subtitle = FALSE)
P2_C <- dv_vs_ipred(xpdb, type = 's', title = "type = 's'" , subtitle = FALSE)

jpeg('./inst/img/type.jpg', width = 8, height = 3.5, units = 'in', res = 80)
grid.arrange(P2_A, P2_B, P2_C, ncol = 3)
dev.off()


# Guides ------------------------------------------------------------------
P3_A <- dv_vs_ipred(xpdb, guides = T, type = 'p',
                    title = "guides = TRUE" , subtitle = FALSE)
P3_B <- dv_vs_ipred(xpdb, guides = F, type = 'p',
                    title = "guides = FALSE" , subtitle = FALSE)

jpeg('./inst/img/guides.jpg', width = 8, height = 3.5, units = 'in', res = 80)
grid.arrange(P3_A, P3_B, ncol = 2)
dev.off()


# Titles ------------------------------------------------------------------
P4_A <- dv_vs_ipred(xpdb, title = NULL, subtitle = NULL)
P4_B <- dv_vs_ipred(xpdb, title = 'Hello', subtitle = 'World !!!')
P4_C <- dv_vs_ipred(xpdb, title = FALSE)

jpeg('./inst/img/titles.jpg', width = 8, height = 3.5, units = 'in', res = 80)
grid.arrange(P4_A, P4_B, P4_C, ncol = 3)
dev.off()


# Aes ---------------------------------------------------------------------
P5_A <- dv_vs_ipred(xpdb, point_color = 'dodgerblue3',
                    point_alpha = 0.5, point_stroke = 0,
                    point_size = 2.5, line_alpha = 0.5,
                    line_size = 0.5, line_color = 'dodgerblue3',
                    line_linetype = 'solid', smooth_method = 'lm')


P5_B <- dv_vs_ipred(xpdb, aes(smooth_color = as.factor(MEAL),
                              smooth_fill = as.factor(MEAL)),
                    layers = list(scale_color_discrete(name = 'Study'),
                                  scale_fill_discrete(name = 'Study')))

jpeg('./inst/img/aes_%d.jpg', width = 4, height = 3.5, units = 'in', res = 80)
print(P5_A)
print(P5_B)
dev.off()


# Grouping variable -------------------------------------------------------
P6 <- dv_vs_ipred(xpdb, aes(smooth_group = MEAL))

jpeg('./inst/img/group.jpg', width = 4, height = 3.5, units = 'in', res = 80)
print(P6)
dev.off()


# Panels ------------------------------------------------------------------
P7 <- dv_vs_ipred(xpdb, by = 'MEAL')

jpeg('./inst/img/panels.jpg', width = 8, height = 7, units = 'in', res = 80)
print(P7)
dev.off()

# Layers ------------------------------------------------------------------
P8 <- dv_vs_ipred(xpdb,
                  layers = list(geom_density(aes(x = IPRED, ..scaled..*0.2),
                                             fill  = 'blue',
                                             color = NA,
                                             alpha = 0.2,
                                             inherit.aes = FALSE),
                                geom_rug(alpha = 0.1,
                                         color = 'grey50',
                                         sides = 'b',
                                         size = 0.4)))

jpeg('./inst/img/layers.jpg', width = 4, height = 3.5, units = 'in', res = 80)
print(P8)
dev.off()

# Scales ------------------------------------------------------------------
P9 <- dv_vs_ipred(xpdb, xscale_breaks = c(0, 0.15, 0.3),
                  xscale_labels = c('Low', 'Med', 'High'),
                  xscale_expand = c(0.2,0),
                  xscale_name = 'Individual model prediction (mg/L)')

jpeg('./inst/img/scales.jpg', width = 4, height = 3.5, units = 'in', res = 80)
print(P9)
dev.off()

# Themes ------------------------------------------------------------------
P10_A <- dv_vs_ipred(xpdb,  gg_theme = theme_readable(),
                     title = 'gg_theme = theme_readable()', subtitle = 'Default in ggxpose')
P10_B <- dv_vs_ipred(xpdb,  gg_theme = theme_grey(),
                     title = 'gg_theme = theme_grey()', subtitle = 'Default in ggplot2')
P10_C <- dv_vs_ipred(xpdb,  gg_theme = theme_bw(),
                     title = 'gg_theme = theme_bw()', subtitle = '')
P10_D <- dv_vs_ipred(xpdb,  gg_theme = theme_dark(),
                     title = 'gg_theme = theme_dark()', subtitle = '')

jpeg('./inst/img/themes.jpg', width = 8, height = 7, units = 'in', res = 80)
grid.arrange(P10_A, P10_B, P10_C, P10_D, ncol = 2)
dev.off()


# Multiple pages ----------------------------------------------------------

jpeg('./inst/img/multiple_%d.jpg', width = 4, height = 4, units = 'in', res = 80)
dv_vs_ipred(xpdb) %>% multiple_pages(by = 'MEAL')
dev.off()

