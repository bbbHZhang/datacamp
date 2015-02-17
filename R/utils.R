#' Documentation path
doc_url = function() { return("http://teach.datacamp.com") }

#' Is the file name a rmd file?
#' 
#' @param filename The queried filename
is_rmd = function(filename) {
  ex = substr(filename, nchar(filename)-3, nchar(filename))
  is_rmd = (ex == ".Rmd") || (ex == ".RMD") || (ex == ".rmd") || (ex == ".RMd")  
  return(is_rmd)
}

#' delete the .md and .html files
#' 
#' @param input_file The chapter fie for which the cleaning is needed
clean_leftovers = function(input_file) {
  file_name = substr(input_file,1, nchar(input_file)-3)
  unlink(paste0(file_name,"md"))
  unlink(paste0(file_name,"html"))
  unlink("libraries",recursive = TRUE) # delete the libraries folder if it exists
}
