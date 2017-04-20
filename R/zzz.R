.onAttach <- function(...) {
  
  if (!interactive()) return()
  
  text <- c('ggxpose is under active development, for updates, checkout: https://guiastrennec.github.io/ggxpose',
            'Need a new plot? Suggest ideas for ggxpose on github: https://github.com/guiastrennec/ggxpose',
            'Do you like ggxpose? Give us some feedback on github: https://github.com/guiastrennec/ggxpose')
  
  packageStartupMessage(sample(text, size = 1))
}
