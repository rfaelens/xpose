## ---- include = FALSE----------------------------------------------------
library(xpose)

knitr::opts_chunk$set(fig.dpi = 96,
                      fig.align = 'center', 
                      fig.height = 6, 
                      fig.width = 6,
                      out.width = '75%',
                      comment = '',
                      message = FALSE)

## ------------------------------------------------------------------------
dv_vs_idv(xpdb_ex_pk)
ipred_vs_idv(xpdb_ex_pk)
pred_vs_idv(xpdb_ex_pk)
dv_preds_vs_idv(xpdb_ex_pk)

## ------------------------------------------------------------------------
dv_vs_pred(xpdb_ex_pk)
dv_vs_ipred(xpdb_ex_pk)

## ------------------------------------------------------------------------
res_vs_idv(xpdb_ex_pk, res = 'CWRES')
res_vs_pred(xpdb_ex_pk, res = 'CWRES')
absval_res_vs_idv(xpdb_ex_pk, res = 'CWRES')
absval_res_vs_pred(xpdb_ex_pk, res = 'CWRES')

## ------------------------------------------------------------------------
prm_vs_iteration(xpdb_ex_pk)
grd_vs_iteration(xpdb_ex_pk)

## ------------------------------------------------------------------------
ind_plots(xpdb_ex_pk)

## ------------------------------------------------------------------------
prm_distrib(xpdb_ex_pk)
eta_distrib(xpdb_ex_pk)
res_distrib(xpdb_ex_pk)
cov_distrib(xpdb_ex_pk)

## ------------------------------------------------------------------------
prm_qq(xpdb_ex_pk)
eta_qq(xpdb_ex_pk)
res_qq(xpdb_ex_pk)
cov_qq(xpdb_ex_pk)

## ------------------------------------------------------------------------
xpdb_ex_pk %>% 
 vpc_data() %>% 
 vpc()

## ------------------------------------------------------------------------
xpdb_ex_pk %>% 
 vpc_data(vpc_type = 'censored', opt = vpc_opt(lloq = 1)) %>% 
 vpc()

## ---- eval = FALSE-------------------------------------------------------
#  xpdb_ex_pk %>%
#   vpc_data(vpc_type = 'categorical') %>%
#   vpc()

