## ---- eval=FALSE---------------------------------------------------------
#  install.packages("plotly")

## ----load packages, results = 'hide', message = FALSE, warning = FALSE----
library(plotly)
library(xpose)

## ----create graph--------------------------------------------------------
gof_plot <- dv_vs_ipred(xpdb_ex_pk, type = 'pl',
                        aes(point_color = SEX, 
                            line_color = SEX)) +
             labs(color = 'Sex', title = 'DV vs IPRED')

## ----render plotly, fig.width = 6, fig.height = 5, out.width = '100%', warning = FALSE, message=FALSE----
plotly::ggplotly(gof_plot)

## ----distribution example, fig.width = 6, fig.height = 5, out.width = '100%', warning = FALSE, message=FALSE----
xpdb_ex_pk %>% 
  eta_distrib(title = 'Eta distribution') %>% 
  plotly::ggplotly()

## ----vpc example, fig.width = 6, fig.height = 5, out.width = '100%', warning = FALSE, message=FALSE----
xpdb_ex_pk %>% 
  vpc_data(stratify = 'OCC') %>% 
  vpc(title = 'Visual predictive checks', type = 'alpr') %>% 
  plotly::ggplotly()

