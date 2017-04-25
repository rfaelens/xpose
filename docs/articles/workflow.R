## ---- include = FALSE----------------------------------------------------
library(xpose)

xpdb <- xpdb_ex_pk

knitr::opts_chunk$set(fig.dpi = 96,
                      fig.align = 'center', 
                      fig.height = 4, 
                      fig.width = 4,
                      out.width = '50%')

## ----demo basic gof------------------------------------------------------
# DV vs. IPRED plot
dv_vs_ipred(xpdb)

# CWRES vs. PRED plot
cwres_vs_pred(xpdb)

## ---- eval = FALSE-------------------------------------------------------
#  # Save the last plot
#  xpose_save()
#  
#  # Change file name and extension
#  xpose_save(filename = '@run_@plotfun_[@ofv].jpeg')

## ---- eval = FALSE-------------------------------------------------------
#  xpose_data(runno = '001') %>%
#    dv_vs_ipred() %>%
#    xpose_save()

