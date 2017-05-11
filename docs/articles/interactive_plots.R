## ---- eval=FALSE---------------------------------------------------------
#  install.packages("plotly")

## ----load packages, results = 'hide', message = FALSE, warning = FALSE----
library(plotly)
library(xpose)

## ----create graph--------------------------------------------------------
gof_plot <- dv_vs_ipred(xpdb_ex_pk, type = 'pl',
                        aes(point_color = as.factor(SEX), 
                            line_color = as.factor(SEX))) +
             labs(color = 'Gender')

## ----render plotly, out.width = '100%', out.height = '100%', warning = FALSE----
plotly::ggplotly(gof_plot)

