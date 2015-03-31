#' Extract clean code from html
#' 
#' @param html The html content to 'codify'
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

#' Extract the R Markdown code files
#' 
#' @param html The html content to 'rmarkdown codify'
#' @param default_name the default file name of an r markdown code chunk, if no title is specified in three curly braces.
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
    
    if(length(names(code)) != length(unique(names(code)))) {
      stop("You need to specify unique file names in sample_code or solution_code in markdown exercises.")
    }
    
    # fix triple backticks and asterisks
    code = sapply(code, fix_specials)
    
    return(toJSON(code))
  } else {
    code = ""
    names(code) = default_name
    return(toJSON(code))
  }
}

#' Helper function to convert the code words _tbt_ and _tast_ to triple backticks and triple asterisks respectively
#' This is to escape from the backtick inception in R markdown files (R markdown containing R markdown...)
#' 
#' @param code The code to convert
fix_specials <- function(code) {
  code = gsub("_tbt_","```",code)
  code = gsub("_tast_","***",code)
  return(code)
}

#' Convenience function to convert html codes:
#' 
#' @param str html string to convert back to more human readable characters
#' @importFrom XML xpathApply xmlValue htmlParse
html2txt <- function(str) {
  str = paste0("<code>",str,"</code>")
  xpathApply(htmlParse(str, asText=TRUE),"//body//text()", xmlValue)[[1]]
}

#' Function to create a vector with the multiple choice options: 
#'
#' @param instructions html code of an unordered list
make_multiple_choice_vector = function(instructions) {
  pattern = "<li>(.*?)</li>"
  instruction_lines =  strsplit(instructions,"\n")[[1]]
  r = regexec(pattern, instruction_lines)
  matches = regmatches(instruction_lines, r)
  extracted_matches = sapply(matches, function(x) x[2])
  multiple_choice_vector = extracted_matches[!is.na(extracted_matches)]
  return(multiple_choice_vector)
}

#' Convenience function for challenges
#' 
#' @param instructions unordered list of instructions
#' @importFrom XML xpathSApply xmlValue htmlParse toString.XMLNode
convert_to_named_list = function(instructions) {
  html = htmlParse(instructions, asText=TRUE)
  titles <- xpathSApply(html, "//h2", xmlValue) # do not keep formatting
  instructions <- xpathSApply(html, "//p", toString.XMLNode) # keep formatting
  mapply(function(x,y) { list(title = x, content = y) }, titles, instructions, USE.NAMES = FALSE, SIMPLIFY = FALSE)
}
