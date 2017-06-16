context('Check console output function')

print_text <- 'run001.lst overview: \n - Software: nonmem 7.3.0 \n - Attached files: \n   + obs tabs: $prob no.1: catab001.csv, cotab001, patab001, sdtab001 \n   + sim tabs: $prob no.2: simtab001.zip \n   + output files: run001.cor, run001.cov, run001.ext, run001.grd, run001.phi, run001.shk \n   + special: <none> \n - gg_theme: theme_readable \n - xp_theme: theme_xp_default \n - Options: dir = analysis/models/pk/, quiet = TRUE, manual_import = NULL'
summary_text <- '\nSummary for problem no. 0 [Global information] \n - Software                     : nonmem\n - Software version             : 7.3.0\n - Run directory                : analysis/models/pk/\n - Run file                     : run001.lst\n - Reference model              : 000\n - Run description              : NONMEM PK example for xpose\n - Run start time               : Tue Jun 13 14:48:35 CEST 2017\n - Run stop time                : Tue Jun 13 14:48:50 CEST 2017\n\nSummary for problem no. 1 [Parameter estimation] \n - Input data                   : mx19_1.csv\n - Number of individuals        : 74\n - Number of observations       : 1022\n - ADVAN                        : 2\n - Estimation method            : foce-i\n - Termination message          : MINIMIZATION SUCCESSFUL\n - Estimation runtime           : 00:00:06\n - Objective function value     : -656.869\n - Number of significant digits : 3.6\n - Covariance step runtime      : 00:00:04\n - Condition number             : 31.3\n - Eta shrinkage                : 10.2 [1], 48.2 [2], 14.7 [3]\n - Epsilon shrinkage            : 6.7 [1]\n - Run warnings                 : (WARNING 2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.\n\nSummary for problem no. 2 [Model simulations] \n - Input data                   : mx19_1.csv\n - Number of individuals        : 74\n - Number of observations       : 1022\n - Estimation method            : sim\n - Number of simulations        : 20\n - Simulation seed              : 221287\n - Run warnings                 : (WARNING 2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.\n                                  (WARNING 22) WITH $MSFI AND \"SUBPROBS\", \"TRUE=FINAL\" ...'
vars_text <- '\nList of available variables for problem no. 1 \n - Subject identifier (id)               : ID\n - Dependent variable (dv)               : DV\n - Independent variable (idv)            : TIME\n - Occasion flag (occ)                   : OCC\n - Dose amount (amt)                     : AMT\n - Event identifier (evid)               : EVID\n - Model typical predictions (pred)      : PRED\n - Model individual predictions (ipred)  : IPRED\n - Model parameter (param)               : KA, CL, V, ALAG1\n - Eta (eta)                             : ETA1, ETA2, ETA3\n - Residuals (res)                       : CWRES, IWRES, RES, WRES\n - Categorical covariates (catcov)       : SEX, MED1, MED2\n - Continuous covariates (contcov)       : CLCR, AGE, WT\n - Not attributed (na)                   : DOSE, SS, II, TAD, CPRED'

# Tests start here --------------------------------------------------------
test_that('Check print.xpose_data returns a proper message', {
  expect_equal(capture_output(print(xpdb_ex_pk)), print_text)
})

test_that('Check summary.xpose_data returns a proper message', {
  expect_equal(capture_output(summary(xpdb_ex_pk)), summary_text)
})

test_that('Check list_vars returns a proper message', {
  expect_equal(capture_output(list_vars(xpdb_ex_pk, problem = 1)), vars_text)
})
