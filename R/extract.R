#' Extract the title
#' 
#' @param x The main content containing a second-level header title
#' 
#' @importFrom markdown markdownToHTML
#' @importFrom stringr str_trim
extract_title <- function(x) {
  lines <- split_lines(x)
  pattern <- "^##\\s+(.*?)"
  hits <- grepl(pattern, lines)
  if(sum(hits) == 0) {
    stop("No title found.")
  }
  if(sum(hits) > 1) {
    stop("More than one title found.")
  }
  title_line <- lines[hits]
  title <- str_trim(gsub("##","",title_line))
  if(nchar(title) == 0) {
    stop("Make sure to specify a title.")
  }
  return(title)
}

#' Extract html from a chunk of text
#' 
#' If a second-level header is in the content, this is removed.
#' 
#' @param x Raw text to be converted to html
#' @importFrom markdown markdownToHTML
extract_html <- function(x) {
  if(is.null(x) || nchar(x) == 0) {
    return(NULL)
  }
  html <- markdownToHTML(text = x, fragment.only = TRUE)
  # remove title, if any
  content <- gsub("<h2>.*</h2>\n*","",html)
  # Convert curly quotes to normal quotes
  content <- gsub("&ldquo;", "&quot;",content)
  content <- gsub("&rdquo;", "&quot;",content)
  return(content)
}

#' Extract R code chunks from raw text
#' 
#' @param x Raw text containing R code chunk
extract_code <- function(x) {
  if(is.null(x))
    return(x)
  lines <- split_lines(x)
  chunk_begin = "^\\s*```+\\s*\\{[.]?r(.*)\\}\\s*$"
  chunk_end = "^\\s*```+\\s*$"
  begin <- grepl(chunk_begin, lines)
  end <- grepl(chunk_end, lines)
  if(!any(begin) || !any(end)) {
    stop(sprintf("No code chunk found!"))
  }
  begin_lines <- which(begin)
  end_lines <- which(end)
  if(any(begin_lines > end_lines)) {
    stop("Something wrong with the code chunks.")
  }
  code <- mapply(function(b,e) if(e-b == 1) "" else paste(lines[(b+1):(e-1)], collapse = "\n"), begin_lines, end_lines)
  #print(diagnose_code(code))
  return(code)
}

#' Function to create a vector with multiple choice options
#'
#' @param x raw text that should contain an unordered list.
#' @importFrom stringr str_extract
extract_mc = function(x) {
  if(is.null(x) || nchar(x) == 0) {
    return(c())
  }
  html <- markdownToHTML(text = x, fragment.only = TRUE)
  lines <- split_lines(html)
  pattern <- "<li>(.*?)</li>"
  list_elements <- lines[grepl(pattern, lines)]
  mc <- gsub("<li>|</li>","",str_extract(list_elements, "<li>.*</li>"))
  if(length(mc) == 0 || length(mc) == 1) {
    stop("No or only one choice could be extracted for the MCE.")
  }
  return(mc)
}

#' Extract R Markdown
#' 
#' @param x The html content to 'rmarkdown codify'
#' @param default_name the default file name of an r markdown code chunk, if no title is specified in three curly braces.
extract_markdown = function(x, default_name) {
  if (!is.null(x) && nchar(x)!=0) {
    code <- extract_code(x)
    
    titles_pos <- gregexpr("\\{\\{\\{(.*?)\\}\\}\\}", code)
    titles <- Map(regmatches, code, titles_pos)
    titles <- lapply(titles, function(title) {
      if(length(title) == 0)
        return(default_name)
      else
        title <- gsub("\\{\\{\\{|\\}\\}\\}","",title)
      return(title)
    })
    code <- gsub("\\{\\{\\{(.*?)\\}\\}\\}\n*","",code)
    names(code) <- unlist(titles)
    
    if(length(names(code)) != length(unique(names(code)))) {
      stop("You need to specify unique file names in sample_code or solution_code in markdown exercises.")
    }
    
    # fix triple backticks and asterisks
    code <- sapply(code, fix_specials)
    
    return(toJSON(code))
  } else {
    code <- ""
    names(code) <- default_name
    return(toJSON(code))
  }
}

#' Convenience function for challenges
#' 
#' Returns embedded list. Titles are second-level headers, the content beneath it the content.
#' 
#' @param x input
#' @importFrom XML xpathSApply xmlValue htmlParse toString.XMLNode
extract_named_list = function(x) {
  if(is.null(x) || nchar(x) == 0) {
    return(NULL)
  }
  html <- markdownToHTML(text = x, fragment.only = TRUE)
  lines <- split_lines(html)
  pattern <- "<h2>(.*?)</h2>"
  title_lines <- grepl(pattern, lines)
  blocks <- cumsum(title_lines)
  titles <- gsub("<h2>|</h2>","",str_extract(lines[title_lines], "<h2>.*</h2>"))
  lst <- list()
  for(i in 1:max(blocks)) {
    lst[[i]] <- list(title = titles[i], content = paste(lines[blocks == i & !title_lines], collapse = "\n"))
  }
  return(lst)
}


#' Extract skills from an exercise
#' 
#' @param x content of the skills part (should be comma separated skill ids)
extract_skills = function(x) {
  if(is.null(x)) {
    return(NULL)
  }
  ids <- strsplit(gsub(" ", "", x), split = ",")[[1]]
  return(as.list(ids))
}