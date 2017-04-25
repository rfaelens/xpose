.onAttach <- function(...) {

  if (!interactive()) return()
  
  text <- c('The xpose is under active development, for updates, checkout: https://guiastrennec.github.io/xpose',
            'Need a new plot? Suggest ideas for the new xpose on github: https://github.com/guiastrennec/xpose',
            'Do you like the new xpose? Give us some feedback on github: https://github.com/guiastrennec/xpose')
  
  packageStartupMessage(sample(text, size = 1))
}

# Remove CRAN note on no visible binding for global variable
utils::globalVariables(c("."))
