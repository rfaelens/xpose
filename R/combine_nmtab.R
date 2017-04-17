combine_nmtab <- function(mod_file = NULL,
                          dir      = NULL,
                          verbose  = FALSE){
  
  # Check inputs
  if (is.null(mod_file)) {
    stop('Argument \"mod_file\" required.', call. = FALSE)
  }
  
  # Extract file name
  tab_file  <- unlist(sapply(strsplit(grep(pattern = '.*FILE\\s*=\\s*',
                                           x = mod_file$CODE[mod_file$ABREV == 'TAB'],
                                           value = TRUE), '.*FILE\\s*=\\s*'), '[', 2))
  
  # Ensure file exsits
  tab_file  <- tab_file[file.exists(paste0(dir, tab_file))]
  
  if (is.null(tab_file) || length(tab_file) == 0) {
    stop('No output table available.', call. = FALSE)
  } else {
    tab_out  <- read_nm_tab(file = paste0(dir, tab_file),
                            rm_duplicates = TRUE,
                            index = TRUE)
  }
  return(tab_out)
  
}
