#=========================================================================#
# Title : ggxpose benchmark
#=========================================================================#

#library(ggplot2)
library(ggxpose)

# Test bench
xpdb <- xpose_data(dir = 'inst/models/', runno = '037')
dv_vs_ipred(xpdb)

dv_vs_ipred(xpdb, point_color = 'coral2', point_alpha = 0.3, line_color = 'blue', line_linetype = 'dashed', smooth_method = 'lm')

# Example on the use of theme, color options and additional layers --------
## Example 1 use of pipes and xpose_save()
xpose_data(dir = 'inst/models/', runno = '037') %>%
  xpose_theme(gg_theme = theme_bw(),
              xp_theme = c(point_color = 'dodgerblue4',
                           line_color  = 'dodgerblue4')) %>%
  dv_vs_ipred() %>%
  xpose_save()

## Example 2 use of aes
dv_vs_ipred(xpdb, by = 'MEAL', type = 'lps',
            aes(smooth_group = MEAL,
                smooth_fill  = as.factor(MEAL),
                smooth_color = as.factor(MEAL)),
            layers = list(scale_fill_discrete(name = 'STUDY'),
                          scale_color_discrete(name = 'STUDY')))

## Example 3 use of layers
dv_vs_ipred(xpdb,
            line_alpha   = 0.8,
            line_color   = 'grey50',
            point_alpha  = 0.8,
            point_color  = 'grey50',
            smooth_fill  = 'deepskyblue2',
            smooth_color = 'deepskyblue2',
            layers = list(geom_density(aes(x = IPRED, ..scaled..*0.2),
                                       fill  = 'blue',
                                       color = NA,
                                       alpha = 0.2,
                                       inherit.aes = FALSE),
                          geom_rug(alpha = 0.1,
                                   color = 'grey50',
                                   sides = 'b',
                                   size = 0.4)))

## Example 4 multiple faceting
dv_vs_ipred(xpdb, by = c('MEAL','T2DM'), log = TRUE)

dv_vs_ipred(xpdb, by = NULL, log = TRUE)

## Example 5 use of multiple_pages
dv_vs_ipred(xpdb) %>%
  multiple_pages(facets = 'MEAL',
                 ncol   = 2,
                 nrow   = 2)

## Example 7 make DV vs. PRED
dv_vs_ipred(xpdb, title = 'DV vs. PRED') +
  aes(x = PRED)

## Example 8 make CWRES vs. TIME
dv_vs_ipred(xpdb,
            abline_slope = 0,
            title = 'CWRES vs. TIME',
            by = 'T2DM') +
  aes(x = TIME, y = CWRES)

# ggplot2 tests -----------------------------------------------------------
P <- ggplot(xpdb$data, aes(x = IPRED, y = DV)) +
  geom_point() +
  geom_line(aes_string(group = 'ID'))



# Benchmarking ------------------------------------------------------------
library(microbenchmark)
control <- function(xpdb) {
  ggplot(xpdb$data[xpdb$data$MDV == 0, ],
         aes(x = IPRED, y = DV, color = as.factor(T2DM))) +
    geom_abline(slope = 1) +
    geom_point() +
    geom_line(aes(group = ID)) +
    geom_smooth(method = 'loess', color = 'red', fill = 'red') +
    labs(title = bquote(atop(bold(.(title)), scriptstyle(.(subtitle)))))
}

microbenchmark(control(xpdb), dv_vs_ipred(xpdb))

benchplot(control(xpdb))
benchplot(dv_vs_ipred(xpdb, by = 'MEAL'))


# xpose4 ------------------------------------------------------------------
library(xpose4)
system.time({
  xp4 <- xpose.data(dir = 'inst/models/', runno = '037')
  print(dv.vs.ipred(xp4))
})

system.time({
  xp5 <- xpose_data(dir = 'inst/models/', runno = '037')
  print(
    dv_vs_ipred(xp5,
                smooth_se = TRUE,
                type      = 'pls',
                guides    = TRUE,
                gg_theme  = theme_classic(),
                xscale_expand = c(0, 0),
                yscale_expand = c(0, 0)) +
      facet_wrap(facets   = c('MEAL', 'MEAL'),
                 labeller = label_both()) +
      theme(strip.background = element_blank())
  )
})

# The end
