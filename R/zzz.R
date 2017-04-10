.onAttach <- function(...) {
  
  if (!interactive()) return()
  
  packageStartupMessage('ggxpose is under *active* development, for updates, checkout: https://guiastrennec.github.io/ggxpose')
  
}
