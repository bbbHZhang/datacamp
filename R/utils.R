#' Documentation path
doc_url <- "http://docs.datacamp.com/teach"

#' Is the file name a rmd file?
#' 
#' @param filename The queried filename
is_rmd = function(filename) {
  ex = substr(filename, nchar(filename)-3, nchar(filename))
  is_rmd = (ex == ".Rmd") || (ex == ".RMD") || (ex == ".rmd") || (ex == ".RMd")  
  return(is_rmd)
}

#' split string into separate lines (copied from knitr)
#' @param x string or file to split in lines
split_lines <- function(x) {
  if (length(grep("\n", x)) == 0L) 
    return(x)
  con = textConnection(x)
  on.exit(close(con))
  readLines(con)
}

#' Utility function to convert the code words _tbt_ and _tast_ to triple backticks and triple asterisks respectively
#' This is to escape from the backtick inception in R markdown files (R markdown containing R markdown...)
#' 
#' @param code The code to convert
fix_specials <- function(code) {
  code <- gsub("_tbt_","```",code)
  code <- gsub("_tast_","***",code)
  return(code)
}