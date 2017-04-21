combine_nm_tab <- function(mod_file = NULL,
                           dir      = NULL,
                           verbose  = TRUE) {
  
  if (is.null(mod_file)) {
    stop('Argument `mod_file` required.', call. = FALSE)
  }
  
  tab_file  <- stringr::str_match(string = mod_file$CODE[mod_file$ABREV == 'TAB'], 
                                  pattern = '\\s+FILE\\s*=\\s*([^\\s]+)')[, 2]
  tab_file  <- tab_file[!is.na(tab_file)]

  if (length(tab_file) == 0) {
    msg('No output table available.', verbose)
  } else {
    read_nm_tab(file = file_path(dir, tab_file),
                rm_duplicates = TRUE,
                index = TRUE)
  }
}
