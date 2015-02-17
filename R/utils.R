# Documentation path
doc_url = function() { return("http://teach.datacamp.com") }

# rmd checker
is_rmd = function(text) {
  ex = substr(text, nchar(text)-3, nchar(text))
  is_rmd = (ex == ".Rmd") || (ex == ".RMD") || (ex == ".rmd") || (ex == ".RMd")  
  return(is_rmd)
}

# delete the .md and .html files
clean_leftovers = function(input_file) {
  file_name = substr(input_file,1, nchar(input_file)-3)
  unlink(paste0(file_name,"md"))
  unlink(paste0(file_name,"html"))
  unlink("libraries",recursive = TRUE) # delete the libraries folder if it exists
}

# Extract clean code from html
extract_code = function(html) {
  if (!is.null(html)) {
    if (nchar(html)!=0) {
      r = regexpr("<code class=\"r\">(.*?)</code>",html)
      code = regmatches(html,r)
      code = gsub("<code class=\"r\">|</code>","",code)
      code = html2txt(code)
      
      # solve bug: when quotes are within quotes, we need different type of quotes! e.g. "c('f','t','w')"
      code = gsub("[\\]\"","'",as.character(code))
      
      return(code)
    }
  }
} 

# Extract the R Markdown code files
extract_markdown = function(html, default_name) {
  if (!is.null(html) && nchar(html)!=0) {
    r = gregexpr("<code class=\"r\">(.*?)</code>",html)
    code = unlist(regmatches(html,r))
    code = gsub("<code class=\"r\">|</code>","",code)
    code = sapply(code, html2txt)
    
    titles_pos = gregexpr("\\{\\{\\{(.*?)\\}\\}\\}", code)
    titles = Map(regmatches, code, titles_pos)
    titles = lapply(titles, function(title) {
      if(length(title) == 0)
        return(default_name)
      else
        title = gsub("\\{\\{\\{|\\}\\}\\}","",title)
      return(title)
    })
    code = gsub("\\{\\{\\{(.*?)\\}\\}\\}\n*","",code)
    names(code) = unlist(titles)
    
    # fix triple backticks and asterisks
    code = sapply(code, fix_specials)
    
    return(toJSON(code))
  } else {
    code = ""
    names(code) = default_name
    return(toJSON(code))
  }
}

fix_specials <- function(code) {
  code = gsub("_tbt_","```",code)
  code = gsub("_tast_","***",code)
  return(code)
}

# Convenience function to convert html codes:
html2txt <- function(str) {
  str = paste0("<code>",str,"</code>")
  xpathApply(htmlParse(str, asText=TRUE),"//body//text()", xmlValue)[[1]]
}

# Function to create an array with the multiple choice options: 
make_multiple_choice_vector = function(instructions) {
  pattern = "<li>(.*?)</li>"
  instruction_lines =  strsplit(instructions,"\n")[[1]]
  r = regexec(pattern, instruction_lines)
  matches = regmatches(instruction_lines, r)
  extracted_matches = sapply(matches, function(x) x[2])
  multiple_choice_vector = extracted_matches[!is.na(extracted_matches)]
  return(multiple_choice_vector)
}
