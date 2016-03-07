#=========================================================================#
# Title : ggxpose benchmark
#=========================================================================#

library(ggxpose)

# Test bench
#model  <- parse_nm_model(file = 'inst/models/run001.lst')
xpdb <- xpose_data(dir = 'inst/models/', runno = '001')


# Default tests -----------------------------------------------------------
dv_vs_ipred(xpdb)
dv_vs_pred(xpdb)
cwres_vs_idv(xpdb)
cwres_vs_pred(xpdb)
absval_iwres_vs_pred(xpdb)


# Panels tests ------------------------------------------------------------
dv_vs_ipred(xpdb, by = NULL)
dv_vs_ipred(xpdb, by = 'OCC')
dv_vs_ipred(xpdb, by = c('MED1', 'MED2'), panel_labeller = 'label_both')


# Pipes and xpose_save() --------------------------------------------------
xpose_data(dir = 'inst/models/', runno = '001') %>%
  xpose_theme(gg_theme = theme_bw(),
              xp_theme = xp_theme_xpose4) %>%
  dv_vs_ipred() %>%
  xpose_save()


# Use of aes --------------------------------------------------------------
dv_vs_ipred(xpdb, by = 'CLASS', type = 'lps',
            aes(smooth_group = CLASS,
                smooth_fill  = as.factor(CLASS),
                smooth_color = as.factor(CLASS)),
            layers = list(scale_fill_discrete(name = 'CLASS'),
                          scale_color_discrete(name = 'CLASS')))


# Use of layers -----------------------------------------------------------
dv_vs_ipred(xpdb,
            line_alpha   = 0.8,
            line_color   = 'grey50',
            point_alpha  = 0.8,
            point_color  = 'grey50',
            smooth_fill  = 'deepskyblue2',
            smooth_color = 'deepskyblue2',
            layers = list(geom_rug(alpha = 0.2,
                                   color = 'grey50',
                                   sides = 'l',
                                   size = 0.4),
                          geom_rug(alpha = 0.2,
                                   color = 'grey50',
                                   sides = 'b',
                                   size = 0.4)))


# Multiple_pages ----------------------------------------------------------
dv_vs_ipred(xpdb) %>%
  multiple_pages(by = 'CLASS', ncol = 2, nrow = )


#=========================================================================#
# Title : ggplot2 benchmark
#=========================================================================#

P <- ggplot(xpdb$data, aes(x = IPRED, y = DV)) +
  geom_point() +
  geom_line(aes_string(group = 'ID'))


# Benchmarking ------------------------------------------------------------
library(microbenchmark)
control <- function(xpdb) {
  ggplot(xpdb$data[xpdb$data$EVID == 0, ],
         aes(x = IPRED, y = DV, color = as.factor(SEX))) +
    geom_abline(slope = 1) +
    geom_point() +
    geom_line(aes(group = ID)) +
    geom_smooth(method = 'loess', color = 'blue', fill = 'blue') +
    labs(title = bquote(atop(bold(.('Hello')), scriptstyle(.('World !!!')))))
}

#microbenchmark(control(xpdb), dv_vs_ipred(xpdb))
#benchplot(control(xpdb))
#benchplot(dv_vs_ipred(xpdb, by = 'CLASS'))


#=========================================================================#
# Title : xpose4 benchmark
#=========================================================================#
library(xpose4)
system.time({
  xp4 <- xpose.data(dir = 'inst/models/', runno = '001')
  print(dv.vs.ipred(xp4))
})

system.time({
  xp5 <- xpose_data(dir = 'inst/models/', runno = '001')
  print(dv_vs_ipred(xp5))
})

# The end
