#' NONMEM file parser
#'
#' @description Parse NONMEM files in R format
#'
#' @param filename Full file name, with either .ext, .phi. .grd, .cor or .cov extension.
#' @param header The name of the first header column (e.g 'NAME', 'ITERATION')
#' @param ext The file extenstion (e.g '.cov', '.ext')
#'
#' @seealso \code{\link{xpose_data}}, \code{\link{read_nm_tables}}
#' @return A \code{\link[dplyr]{tibble}} tables of containing the columns of each dataset. 
#' If mutiple headers are detected in the file then the last header is used. These columns are used to distinguish between problems: 
#' \itemize{
##'  \item{\strong{PROB}}{: the problem number}
##'  \item{\strong{SUBPROB}}{: The subproblem number.}
##'  \item{\strong{METHOD}}{: a character identifier named after the method used for each problem, i.e FOCE= First order conditional estimation.}
##' }
#' @examples
#' \dontrun{
#' nm_file <- read_nm_file(files = 'inst/extdata/run001.ext')
#' }
#' @export

read_nm_file <- function(filename = NULL,
                      header = NULL,
                      ext= NULL){

  file <- readr::read_lines(file=filename) 
  
  probinf <- prob_info(file)
  tables <- file[stringr::str_detect(file, header)]

  
  header <- unlist(stringr::str_split(tables, pattern = '\\s+')[length(tables)])
  header <- gsub("\\(", ".", header)
  header <- gsub("\\)", "", header)
  header <- gsub("\\,", ".", header)
  
  if(ext==".cov" || ext==".cor"){
    file  <- file[!stringr::str_detect(file, 'TABLE NO.') & 
                    !stringr::str_detect(file, 'NAME')]
  
  }else{
    file  <- file[!stringr::str_detect(file, '[A-Z]{3,}')]
  }
  
  
  # Split on white spaces
  file2 <- stringr::str_split(file, pattern = '\\s+')
  #bind to matrix
  file3 <- do.call('rbind', file2)
  
  # as dataframe and set the header, ensure numeric values
  file3[file3==""] <- NA
  if(ext==".cov" || ext==".cor"){
    file13 <- file3[,1:2]
    file3end <- file3[,3:ncol(file3)]
    file3end <- apply(file3end, 2, as.numeric)
    file3 <- cbind(as.data.frame(file13, stringsAsFactors = F),as.data.frame(file3end))
    
  }else{
    file3 <- apply(file3, 2, as.numeric)
  }
  
  dffile <- data.frame(file3, stringsAsFactors = F)
  
  if (length(header)==ncol(dffile)){
  names(dffile) <- header
  }else{
    dffile <- dffile[,-1]
    names(dffile) <- header
}
    
 
  # Repet each problem according to the number of subjects
  typeOfProb2 <- probinf[rep(seq(nrow(probinf)), probinf$noSubj),]
  # Select columns need
  typeOfProb2$noSubj <- NULL
  # Add problem and subproblem to the phi dataframe
  dffile <- cbind(dffile,typeOfProb2)
  
  dffile <- dffile[, !apply(is.na(dffile), 2, all)]
  dffile$PROB <- as.numeric(dffile$PROB)
  dffile$SUBPROB <- as.numeric(dffile$SUBPROB)
  

  dffile <- dplyr::as.tbl(dffile)
  
  dffile
  
}


prob_info <- function(str){
  df <- find_NoProb(str)
  df$noSubj <- noRows_table(str)
  
  df
}
noRows_table <- function(str){
  df<-data.frame(idx=which(stringr::str_detect(str, 'TABLE NO.')))
  df$N <- 1:nrow(df)
  df$start <- (df$idx - (2*df$N))+2
  df$stop <- dplyr::lead(df$start)
  df$stop[is.na(df$stop)] <- (length(str)-df$N[is.na(df$stop)]*2)+1
  df$noSubj <- as.numeric(df$stop - df$start)
}

find_NoProb <- function(str){ 
  # Extract all problem header
  Prob <- str[stringr::str_detect(str, 'Problem=')]
  
  # Set method of estimation
  typeOfProb <- data.frame(METHOD=find_meth(Prob), org=gsub(".*: ","", Prob),
                           stringsAsFactors = F)
  
  # extract the number of problems
  typeOfProb$org <- stringr::str_replace(typeOfProb$org," Superproblem.*","")
  typeOfProb$PROB <- stringr::str_replace(typeOfProb$org,"Problem=","")
  typeOfProb$PROB <- stringr::str_replace(typeOfProb$PROB," Subproblem=.*","")
  
  # Extract the subproblem number 
  typeOfProb$SUBPROB <- stringr::str_replace(typeOfProb$org, ".* ","")
  typeOfProb$SUBPROB <- stringr::str_replace(typeOfProb$SUBPROB,"Subproblem=","")
  
  typeOfProb$org <- NULL
  typeOfProb
}


find_meth <- function(str){
  meth <- dplyr::case_when(stringr::str_detect(str, 'First Order Conditional') ~ 'FOCE',
                           stringr::str_detect(str, 'Laplace Conditional') ~ 'LCE', 
                           stringr::str_detect(str, 'Iterative Two Stage') ~ 'ITS',
                           stringr::str_detect(str, 'Importance Sampling') ~ 'IMP',
                           stringr::str_detect(str, 'Stochastic Approximation') ~ 'SAEM',
                           TRUE ~ 'BAYES')
}

