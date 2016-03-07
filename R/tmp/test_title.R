library(ggxpose)
xpdb <- xpose_data(dir = 'inst/models/', runno = '001')
print(xpdb$mod_info)
#paste(unlist(xpdb$mod_info[c('descr','nobs')]), collapse = ' | ')
#string <- paste('My model is :', run, 'with', nobs)

# Method 1
parse_title <- function(string, xpdb) {
  # Add safety, how?
  attach(xpdb$mod_info, warn.conflicts = FALSE)
  return(string)
}

parse_title(paste('My model is:', run, 'with', nobs), xpdb)
parse_title('Hello', xpdb)
parse_title(NULL, xpdb)


# Method 2
parse_t <- function(string, xpdb) {
  #attach(xpdb$mod_info, warn.conflicts = FALSE)
  return(string)
}
parse_t(paste('My model is:', '<run>', 'with', '<nobs>'), xpdb)
parse_t('Hello', xpdb)
parse_t(NULL, xpdb)
