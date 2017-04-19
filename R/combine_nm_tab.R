combine_nm_tab <- function(mod_file = NULL,
                           dir      = NULL,
                           verbose  = FALSE) {
  
  if (is.null(mod_file)) {
    stop('Argument `mod_file` required.', call. = FALSE)
  }
  
  # Extract tab names
  tab_file  <- unlist(sapply(strsplit(grep(pattern = '.*FILE\\s*=\\s*',
                                           x = mod_file$CODE[mod_file$ABREV == 'TAB'],
                                           value = TRUE), '.*FILE\\s*=\\s*'), '[', 2))
  
  # Ensure file exsits
  tab_file  <- tab_file[file.exists(file.path(dir, tab_file))]
  
  if (is.null(tab_file) || length(tab_file) == 0) {
    msg('No output table available.', verbose)
    return(NULL)
  } else {
    tab_out  <- read_nm_tab(file = paste0(dir, tab_file),
                            rm_duplicates = TRUE,
                            index = TRUE)
  }
  tab_out
}
