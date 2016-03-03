combine_nmtab <- function(mod_file = NULL,
                          dir = NULL,
                          verbose = FALSE){

  # Check inputs
  if(is.null(mod_file)) {
    stop('Argument \"mod_file\" required.')
  }

  # Extract file name
  tab_file  <- unlist(sapply(strsplit(grep(pattern = '.*FILE\\s*=\\s*',
                                           x = mod_file$CODE[mod_file$ABREV == 'TAB'],
                                           value = TRUE), '.*FILE\\s*=\\s*'), '[', 2))

  # Ensure file exsits
  tab_file  <- tab_file[file.exists(paste0(dir, tab_file))]

  if(is.null(tab_file) | length(tab_file) == 0) {
    stop('No output table available.')
  } else {
    tab_file  <- read_nmtab(file = paste0(dir, tab_file), rm_duplicates = TRUE)
  }
  return(tab_file)

}
