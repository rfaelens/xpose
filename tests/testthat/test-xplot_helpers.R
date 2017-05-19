context('Check xpose_helpers')

# Define objects to be tested -----------------------------------------------

xpdb <- xpdb_ex_pk

# Tests start here --------------------------------------------------------

test_that("Check plot input vars", {
  expect_null(check_vars(NULL,xpdb))
  expect_null(check_vars(c("problem","simtab", "index","data"),xpdb))
})

test_that("Check plot input scales", {
  expect_that(check_scales('x',NULL), matches('continuous'))
  expect_that(check_scales(c('log10', 'continuous'), NULL),matches('continuous'))
  expect_equivalent(check_scales(c('log10', 'continuous'), 'log10'),c('log10','continuous'))
  
})

test_that("Check title check", {
  expect_error(check_title(NULL))
})


test_that("Check parse title", {
  expect_match(parse_title('OFV: @ofv', xpdb), "OFV: -656.869")
  expect_warning(parse_title('OFV: @fake', xpdb),regexp = 'not part of' )
  expect_null(parse_title(NULL, xpdb))
})


