context('Check VPCs')

# Define files to be tested -----------------------------------------------
psn_cmd_1 <- '/opt/local64/PsN/bin/vpc -seed=221287 -samples=1000 -stratify_on=VAR1,VAR2 -lst=run001.lst run001.mod'
psn_cmd_2 <- '/opt/local64/PsN/bin/vpc -seed=221287 -samples=1000 -idv=TAD -dv=IDV -lst=run001.lst run001.mod'

# ctrl_special <- xpdb_ex_pk %>%
#   vpc_data(opt = vpc_opt(n_bins = 3)) %>%
#   vpc_data(vpc_type = 'cens', opt = vpc_opt(n_bins = 3, lloq = 1))
#  save(ctrl_special, file = 'data/ctrl_special.RData', compress = 'xz')
load(file = file.path('data', 'ctrl_special.RData'))

# Tests start here --------------------------------------------------------
test_that('vpc_opt works properly', {
  expect_equal(vpc_opt(), 
               list(bins = 'jenks', n_bins = 'auto', bin_mid = 'mean', pred_corr = NULL, 
                    pred_corr_lower_bnd = 0, pi = c(0.05, 0.95),  ci = c(0.05, 0.95),  
                    lloq = NULL,  uloq = NULL, rtte = FALSE, rtte_calc_diff = TRUE, 
                    events = NULL, kmmc = NULL, reverse_prob = FALSE, as_percentage = TRUE))
})

test_that('get_psn_vpc_cols works properly', {
  expect_equal(get_psn_vpc_cols(psn_cmd_1),
               list(id = 'ID', idv = 'TIME', dv = 'DV', pred = 'PRED'))
  expect_equal(get_psn_vpc_cols(psn_cmd_2),
               list(id = 'ID', idv = 'TAD', dv = 'IDV', pred = 'PRED'))
})

test_that('get_psn_vpc_strat works properly', {
  expect_equal(get_psn_vpc_strat(psn_cmd_1), c('VAR1', 'VAR2'))
  expect_null(get_psn_vpc_strat(psn_cmd_2))
})


test_that('vpc_data properly check input', {
  expect_error(vpc_data(), regexp = 'argument \"xpdb\" is missing')
  expect_error(vpc_data(xpdb_ex_pk, psn_folder = '.', quiet = TRUE), 
                 regexp = 'No table files could be found')
})

test_that('vpc_data works properly with xpdb tables', {
  xpdb_vpc <- vpc_data(xpdb_ex_pk, opt = vpc_opt(n_bins = 3), quiet = TRUE)
  expect_true(is.xpdb(xpdb_vpc))
  expect_identical(xpdb_vpc$special, ctrl_special$special[2, ])
})

test_that('vpc_type categorical works properly', {
  xpdb_vpc_cens <- vpc_data(xpdb_ex_pk, vpc_type = 'censored', 
                            opt = vpc_opt(n_bins = 3, lloq = 1), quiet = TRUE)
  expect_true(is.xpdb(xpdb_vpc_cens))
  expect_identical(xpdb_vpc_cens$special, ctrl_special$special[1, ])
})

test_that('vpc plot properly check input', {
  expect_error(vpc())
  expect_error(vpc(xpdb = 1, quiet = FALSE), regexp = 'Bad input')
  expect_error(vpc(xpdb_ex_pk, quiet = FALSE), regexp = 'No `special` slot')
  expect_error(vpc(ctrl_special, quiet = FALSE), regexp = 'Several vpc data')
  expect_error(vpc(ctrl_special, vpc_type = 'unknown', quiet = FALSE), 
               regexp = 'should be one of')
})

test_that('vpc plot are properly generated', {
  p_cont  <- vpc(ctrl_special, vpc_type = 'continuous', type = 'alrpt', quiet = FALSE)
  p_cont2 <- vpc(ctrl_special, vpc_type = 'continuous', facets = ~group)
  p_cens  <- vpc(ctrl_special, vpc_type = 'censored', smooth = FALSE, facets = 'group', 
                 type = 'alr', quiet = FALSE)
  expect_true(is.xpose.plot(p_cont))
  expect_true(is.xpose.plot(p_cens))
  expect_equal(class(p_cont$layers[[1]]$geom)[1], 'GeomRibbon')
  expect_equal(class(p_cont$layers[[2]]$geom)[1], 'GeomLine')
  expect_equal(class(p_cont$layers[[3]]$geom)[1], 'GeomPoint')
  expect_equal(class(p_cont$layers[[4]]$geom)[1], 'GeomText')
  expect_equal(class(p_cont$layers[[5]]$geom)[1], 'GeomRug')
  expect_equal(class(p_cont$facet)[1], 'FacetNull')
  expect_equal(class(p_cont2$facet)[1], 'FacetGrid')
  expect_equal(class(p_cens$layers[[1]]$geom)[1], 'GeomRect')
  expect_equal(class(p_cens$facet)[1], 'FacetWrap')
})
