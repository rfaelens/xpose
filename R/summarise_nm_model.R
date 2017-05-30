summarise_nm_model <- function(file, model, software, rounding) {
  sum <- dplyr::bind_rows(
    sum_software(software),                    # Software name
    sum_version(model, software),              # Software version
    sum_file(file),                            # Model file
    sum_run(file),                             # Model run (model file without extension)
    sum_directory(file),                       # Model directory
    sum_reference(model, software),            # Reference model
    sum_probn(model, software),                # Problem no.
    sum_description(model, software),          # Model description
    sum_input_data(model, software),           # Model input data used
    sum_nobs(model, software),                 # Number of observations
    sum_nind(model, software),                 # Number of individuals
    sum_nsim(model, software),                 # Number of simulations
    sum_simseed(model, software),              # Simulation seed
    sum_subroutine(model, software),           # Des solver
    sum_runtime(model, software),              # Estimation runtime
    sum_covtime(model, software),              # Covariance matrix runtime
    sum_warnings(model, software),             # Run warnings (e.g. boundary)
    sum_errors(model, software),               # Run errors (e.g termination error)
    sum_nsig(model, software),                 # Number of significant digits
    sum_condn(model, software, rounding),      # Condition number
    sum_nesample(model, software),             # Number of esample
    sum_esampleseed(model, software),          # esample seed number
    sum_ofv(model, software),                  # Objective function value
    sum_method(model, software),               # Estimation method or sim
    sum_shk(model, software, 'eps', rounding), # Epsilon shrinkage
    sum_shk(model, software, 'eta', rounding)  # Eta shrinkage
  )
  
  # Complete missing cases for consistency
  tmp <- sum %>% 
    dplyr::filter(.$problem != 0)
  
  if (nrow(tmp) == 0) return(sum)
  
  tmp %>% 
    tidyr::complete_(cols = c(quote(problem), quote(label)), 
                     fill = list(subp = 1, value = 'na')) %>% 
    dplyr::bind_rows(dplyr::filter(sum, sum$problem == 0)) %>%
    dplyr::arrange_(.dots = c('problem', 'label', 'subp')) %>%
    dplyr::mutate(descr = dplyr::case_when(
      .$label == 'software' ~ 'Software',
      .$label == 'version' ~ 'Software version',
      .$label == 'file' ~ 'Run file',
      .$label == 'run' ~ 'Run number',
      .$label == 'dir' ~ 'Run directory',
      .$label == 'ref' ~ 'Reference model',
      .$label == 'probn' ~ 'Problem number',
      .$label == 'descr' ~ 'Run description',
      .$label == 'data' ~ 'Input data',
      .$label == 'nobs' ~ 'Number of observations',
      .$label == 'nind' ~ 'Number of individuals',
      .$label == 'nsim' ~ 'Number of simulations',
      .$label == 'simseed' ~ 'Simulation seed',
      .$label == 'subroutine' ~ 'ADVAN',
      .$label == 'runtime' ~ 'Estimation runtime',
      .$label == 'covtime' ~ 'Covariance matrix runtime',
      .$label == 'warnings' ~ 'Run warnings',
      .$label == 'errors' ~ 'Run errors',
      .$label == 'nsig' ~ 'Number of significant digits',
      .$label == 'condn' ~ 'Condition number',
      .$label == 'nesample' ~ 'Number of ESAMPLE',
      .$label == 'esampleseed' ~ 'ESAMPLE seed number',
      .$label == 'ofv' ~ 'Objective function value',
      .$label == 'method' ~ 'Estimation method',
      .$label == 'epsshk' ~ 'Epsilon shrinkage',
      .$label == 'etashk' ~ 'Eta shrinkage')) %>% 
    dplyr::select(dplyr::one_of('problem', 'subp', 'descr', 'label', 'value'))
}

# Default template for function output
sum_tpl <- function(label, value) {
  dplyr::tibble(problem = 0,
                subp    = 1,
                label   = label,
                value   = value)
}

# Software name
sum_software <- function(software) {
  sum_tpl('software', software)
}

# Software version
sum_version <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$problem == 0) %>%
      dplyr::filter(stringr::str_detect(.$code, 'NONLINEAR MIXED EFFECTS MODEL PROGRAM'))
    
    if (nrow(x) == 0) return(sum_tpl('version', 'na'))
    
    sum_tpl('version', stringr::str_match(x$code, 'VERSION\\s+(.+)$')[, 2])
  }
}

# Model file name
sum_file <- function(file) {
  sum_tpl('file', basename(file))
}

# Model run name
sum_run <- function(file) {
  sum_tpl('run', update_extension(basename(file), ''))
}

# Model file directory
sum_directory <- function(file) {
  sum_tpl('dir', dirname(file))
}

# Reference model
sum_reference <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$problem == 0) %>%
      dplyr::filter(stringr::str_detect(tolower(.$comment), stringr::regex('based on\\s*:', ignore_case = TRUE)))
    
    if (nrow(x) == 0) return(sum_tpl('ref', 'na'))
    
    sum_tpl('ref', stringr::str_match(x$comment, ':\\s*(.+)$')[1, 2]) # Note: only take the first match
  }
}

# Problem no.
sum_probn <- function(model, software) {
  if (software == 'nonmem') {
    x <- unique(model$problem[model$problem != 0]) 
    
    if (length(x) == 0) return(sum_tpl('probn', 'na'))
    
    dplyr::tibble(
      problem = x,
      subp    = 1,
      label   = 'probn',
      value   = as.character(x))
  }
}

# Model description
sum_description <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'pro')
    
    if (nrow(x) == 0) return(sum_tpl('descr', 'na'))
    
    x %>% 
      dplyr::mutate(subp = 1,
                    label = 'descr',
                    value = as.character(.$code)) %>% 
      dplyr::select(dplyr::one_of('problem', 'subp', 'label', 'value'))
  }
}

# Input data
sum_input_data <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'dat')
    
    if (nrow(x) == 0) return(sum_tpl('data', 'na'))
    
    x %>% 
      dplyr::mutate(subp = 1,
                    label = 'data',
                    value = stringr::str_match(.$code, '^\\s*([^\\s]+)\\s+')[, 2]) %>% 
      dplyr::select(dplyr::one_of('problem', 'subp', 'label', 'value'))
  }
}

# Number of observations
sum_nobs <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'lst') %>% 
      dplyr::filter(stringr::str_detect(.$code, stringr::fixed('TOT. NO. OF OBS RECS')))
    
    if (nrow(x) == 0) return(sum_tpl('nobs', 'na'))
    
    x %>% 
      dplyr::mutate(subp = 1,
                    label = 'nobs',
                    value = stringr::str_match(.$code, '\\d+')) %>% 
      dplyr::select(dplyr::one_of('problem', 'subp', 'label', 'value'))
  }
}

# Number of individuals
sum_nind <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'lst') %>% 
      dplyr::filter(stringr::str_detect(.$code, stringr::fixed('TOT. NO. OF INDIVIDUALS')))
    
    if (nrow(x) == 0) return(sum_tpl('nind', 'na'))
    
    x %>% 
      dplyr::mutate(subp = 1,
                    label = 'nind',
                    value = stringr::str_match(.$code, '\\d+')) %>% 
      dplyr::select(dplyr::one_of('problem', 'subp', 'label', 'value'))
  }
}

# Simulation number
sum_nsim <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'sim') %>% 
      dplyr::filter(stringr::str_detect(.$code, stringr::fixed('NSUB')))
    
    if (nrow(x) == 0) return(sum_tpl('nsim', 'na'))
    
    x %>% 
      dplyr::mutate(subp = 1,
                    label = 'nsim',
                    value = stringr::str_match(.$code, 'NSUB.*=\\s*(\\d+)')[, 2]) %>% 
      dplyr::select(dplyr::one_of('problem', 'subp', 'label', 'value'))
  }
}

# Simulation seed
sum_simseed <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'sim') %>% 
      dplyr::filter(stringr::str_detect(.$code, '\\(\\d+\\)'))
    
    if (nrow(x) == 0) return(sum_tpl('simseed', 'na'))
    
    x %>% 
      dplyr::mutate(subp = 1,
                    label = 'simseed',
                    value = stringr::str_match(.$code, '\\((\\d+)\\)')[, 2]) %>% 
      dplyr::select(dplyr::one_of('problem', 'subp', 'label', 'value'))
  }
}

# DES solver
sum_subroutine <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'sub') %>% 
      dplyr::filter(stringr::str_detect(.$code, stringr::fixed('ADVAN')))
    
    if (nrow(x) == 0) return(sum_tpl('subroutine', 'na'))
    
    x %>% 
      dplyr::mutate(subp = 1,
                    label = 'subroutine',
                    value = stringr::str_match(.$code, 'ADVAN(\\d+)')[, 2]) %>% 
      dplyr::select(dplyr::one_of('problem', 'subp', 'label', 'value'))
  }
}

# Estimation runtime
sum_runtime <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'lst') %>% 
      dplyr::filter(stringr::str_detect(.$code, stringr::fixed('Elapsed estimation time')))
    
    if (nrow(x) == 0) return(sum_tpl('runtime', 'na'))
    
    x %>% 
      dplyr::mutate(subp = 1,
                    label = 'runtime',
                    value = as.ctime(stringr::str_match(.$code, '([\\.\\d]+)')[, 2])) %>% 
      dplyr::select(dplyr::one_of('problem', 'subp', 'label', 'value'))
  }
}

# Covariance matrix runtime
sum_covtime <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'lst') %>% 
      dplyr::filter(stringr::str_detect(.$code, stringr::fixed('Elapsed covariance time')))
    
    if (nrow(x) == 0) return(sum_tpl('covtime', 'na'))
    
    x %>% 
      dplyr::mutate(subp = 1,
                    label = 'covtime',
                    value = as.ctime(stringr::str_match(.$code, '([\\.\\d]+)')[, 2])) %>% 
      dplyr::select(dplyr::one_of('problem', 'subp', 'label', 'value'))
  }
}

# Run warnings (e.g. boundary)
sum_warnings <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'oth') %>% 
      dplyr::filter(stringr::str_detect(.$code, 'WARNING'))
    
    if (nrow(x) == 0) return(sum_tpl('warnings', 'na'))
    
    x %>% 
      dplyr::mutate(problem = stringr::str_match(.$code, 'FOR PROBLEM\\s+(\\d+)')[,2]) %>% 
      tidyr::fill_(fill_cols = 'problem') %>% 
      dplyr::mutate(problem = as.numeric(.$problem)) %>% 
      dplyr::filter(!stringr::str_detect(.$code, 'FOR PROBLEM\\s+(\\d+)')) %>% 
      dplyr::mutate(code = stringr::str_trim(.$code)) %>% 
      dplyr::mutate(code = stringr::str_replace(.$code, '\\(.+\\)\\s+', '')) %>% 
      #dplyr::group_by_(.dots = 'problem') %>% 
      #tidyr::nest() %>% 
      #dplyr::mutate(value = purrr::map_chr(.$data, ~stringr::str_c(.$code, collapse = '\n'))) %>% 
      dplyr::mutate(subp = 1,
                    value = as.character(.$code),
                    label = 'warnings') %>% 
      dplyr::select(dplyr::one_of('problem', 'subp', 'label', 'value'))
  }
}

# Run errors (e.g termination error)
sum_errors <- function(model, software) {
  if (software == 'nonmem') {
    sum_tpl('errors', 'na') # To be added
  }
}

# Number of significant digits
sum_nsig <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'lst') %>% 
      dplyr::filter(stringr::str_detect(.$code, stringr::fixed('NO. OF SIG. DIGITS')))
    
    if (nrow(x) == 0) return(sum_tpl('nsig', 'na'))
    
    x %>% 
      dplyr::mutate(subp = 1,
                    label = 'nsig',
                    value = stringr::str_match(.$code, ':\\s+([\\.\\d]+)')[, 2]) %>% 
      dplyr::select(dplyr::one_of('problem', 'subp', 'label', 'value'))
  }
}

# Condition number
sum_condn <- function(model, software, rounding) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'lst') %>% 
      dplyr::slice(which(stringr::str_detect(.$code, stringr::fixed('EIGENVALUES OF COR'))) + 4)
    
    if (nrow(x) == 0) return(sum_tpl('condn', 'na'))
    
    x %>% 
      dplyr::group_by_(.dots = 'problem') %>% 
      tidyr::nest() %>% 
      dplyr::mutate(subp  = 1,
                    label = 'condn',
                    value = purrr::map_chr(.$data, function(x) {
                      stringr::str_trim(x$code, side = 'both') %>%  
                        stringr::str_split(pattern = '\\s+') %>% 
                        purrr::flatten_chr() %>% 
                        as.numeric() %>% 
                        {max(.)/min(.)} %>% 
                        round(digits = rounding) %>% 
                        as.character()})) %>% 
      dplyr::select(dplyr::one_of('problem', 'subp', 'label', 'value'))
  }
}

# Number of ESAMPLE (i.e. NPDE)
sum_nesample <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'tab') %>% 
      dplyr::filter(stringr::str_detect(.$code, stringr::fixed('ESAMPLE')))
    
    if (nrow(x) == 0) return(sum_tpl('nesample', 'na'))
    
    x %>% 
      dplyr::mutate(subp = 1,
                    label = 'nesample',
                    value = stringr::str_match(.$code, 'ESAMPLE\\s*=\\s*(\\d+)')[, 2]) %>% 
      dplyr::select(dplyr::one_of('problem', 'subp', 'label', 'value'))
  }
}

# ESAMPLE seed number
sum_esampleseed <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'tab') %>% 
      dplyr::filter(stringr::str_detect(.$code, stringr::fixed('SEED')))
    
    if (nrow(x) == 0) return(sum_tpl('esampleseed', 'na'))
    
    x %>% 
      dplyr::mutate(subp = 1,
                    label = 'esampleseed',
                    value = stringr::str_match(.$code, 'SEED\\s*=\\s*(\\d+)')[, 2]) %>% 
      dplyr::select(dplyr::one_of('problem', 'subp', 'label', 'value'))
  }
}

# Objective function value
sum_ofv <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'lst') %>% 
      dplyr::filter(stringr::str_detect(.$code, stringr::fixed('#OBJV')))
    
    if (nrow(x) == 0) return(sum_tpl('ofv', 'na'))
    
    x %>% 
      dplyr::mutate(value = stringr::str_match(.$code, '\\*\\s+(.+)\\s+\\*')[, 2]) %>% 
      dplyr::group_by_(.dots =  'problem') %>% 
      dplyr::mutate(subp = 1:n(),
                    label = 'ofv') %>% 
      dplyr::select(dplyr::one_of('problem', 'subp', 'label', 'value')) %>% 
      dplyr::ungroup()
  }
}

# Estimation method or sim
sum_method <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine %in% c('sim', 'est')) %>% 
      dplyr::filter(stringr::str_detect(.$code, 'METHOD|NSUB'))
    
    if (nrow(x) == 0) return(sum_tpl('method', 'na'))
    
    x %>% 
      dplyr::mutate(value = stringr::str_match(.$code, 'METHOD\\s*=\\s*([^\\s]+)')[, 2],
                    inter = stringr::str_detect(.$code, 'INTER')) %>% 
      dplyr::mutate(value = dplyr::if_else(.$subroutine == 'sim', 'sim', .$value)) %>% 
      dplyr::mutate(value = dplyr::case_when(.$value == '0' ~ 'FO',
                                             .$value == '1' ~ 'FOCE',
                                             TRUE ~ .$value)) %>% 
      dplyr::mutate(value = stringr::str_c(stringr::str_to_lower(.$value), dplyr::if_else(.$inter, '-i', ''))) %>% 
      dplyr::group_by_(.dots = 'problem') %>% 
      dplyr::mutate(subp = 1:n(),
                    label = 'method') %>% 
      dplyr::select(dplyr::one_of('problem', 'subp', 'label', 'value')) %>% 
      dplyr::ungroup()
  }
}

# Epsilon/Eta shrinkage
sum_shk <- function(model, software, type, rounding) {
  if (software == 'nonmem') {
    # Get shrinkage from: 1) psn, 2) shr file 3) nonmem lst
    ## Method 3 (worse one)
    x <- model %>% 
      dplyr::filter(.$subroutine == 'lst') %>% 
      dplyr::filter(stringr::str_detect(.$code, stringr::fixed(stringr::str_c(stringr::str_to_upper(type), 
                                                                              'shrink', sep = ''))))
    
    if (nrow(x) == 0) return(sum_tpl(stringr::str_c(type, 'shk'), 'na'))
    
    x %>% 
      dplyr::mutate(code = stringr::str_match(.$code, '\\Q(%):\\E\\s*(.+)')[, 2]) %>% 
      dplyr::mutate(code = stringr::str_split(.$code, '\\s+')) %>% 
      dplyr::mutate(value = purrr::map(.$code, ~round(as.numeric(.), digits = rounding)),
                    grouping = purrr::map(.$code, ~stringr::str_c(' [', 1:length(.), ']', sep = ''))) %>% 
      dplyr::group_by_(.dots = 'problem') %>% 
      dplyr::mutate(subp = 1:n()) %>% 
      dplyr::ungroup() %>% 
      tidyr::unnest_(unnest_cols = c('value', 'grouping')) %>% 
      dplyr::filter(.$value != 100) %>% 
      dplyr::mutate(value = stringr::str_c(.$value, .$grouping)) %>% 
      dplyr::group_by_(.dots = c('problem', 'subp')) %>% 
      tidyr::nest() %>% 
      dplyr::mutate(label = stringr::str_c(type, 'shk'),
                    value = purrr::map_chr(.$data, ~stringr::str_c(.$value, collapse = ', '))) %>% 
      dplyr::select(dplyr::one_of('problem', 'subp', 'label', 'value')) %>% 
      dplyr::ungroup()
  }
}
