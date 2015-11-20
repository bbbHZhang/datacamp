#' @importFrom stringr str_trim
extract_title <- function(x) {
  lines <- str_split(x, pattern = "\n")[[1]]
  pattern <- "^##\\s+(.*?)"
  hits <- grepl(pattern, lines)
  if(sum(hits) == 0) {
    stop("No title found.")
  }
  if(sum(hits) > 1) {
    stop("More than one title found.")
  }
  title_line <- lines[hits]
  title <- stringr::str_trim(gsub("##","",title_line))
  if(nchar(title) == 0) {
    stop("Make sure to specify a title.")
  }
  return(title)
}

# Extract html from a chunk of text
# If a second-level header is in the content, this is removed.
#' @importFrom markdown markdownToHTML
extract_html <- function(x, htmlify) {
  if(is.null(x) || nchar(x) == 0) {
    return("")
  }
  
  if(htmlify) {
    html <- markdownToHTML(text = x, fragment.only = TRUE)
    # remove title, if any
    content <- gsub("<h2>.*</h2>\n*","",html)
    # Convert curly quotes to normal quotes
    content <- gsub("&ldquo;", "&quot;",content)
    content <- gsub("&rdquo;", "&quot;",content)  
  } else {
    content <- gsub("##\\s*.*?\n+","",x)
  }
  return(content)
}

# Extract code chunks from raw text
extract_code <- function(x) {
  if(is.null(x)) return("")
  lines <- str_split(x, "\n")[[1]]
  lang_part <- "r|py"
  chunk_begin <- sprintf("^\\s*```+\\s*\\{[.]?%s(.*)\\}\\s*$", lang_part)
  chunk_end <- "^\\s*```+\\s*$"
  begin <- grepl(chunk_begin, lines)
  end <- grepl(chunk_end, lines)
  if(!any(begin) || !any(end)) {
    stop(sprintf("No code chunk found! Make sure you specify the correct language."))
  }
  begin_lines <- which(begin)
  end_lines <- which(end)
  if(any(begin_lines > end_lines)) {
    stop("Something wrong with the code chunks.")
  }
  code <- mapply(function(b,e) if(e-b == 1) "" else paste(lines[(b+1):(e-1)], collapse = "\n"), begin_lines, end_lines)
  return(code)
}

#' @importFrom markdown markdownToHTML
#' @importFrom xml2 read_html xml_find_all
extract_as_vec <- function(x) {
  if(is.null(x) || nchar(x) == 0) {
    return(c("empty"))
  }
  html <- markdownToHTML(text = x, fragment.only = TRUE)
  vec <- vapply(xml_find_all(xml2::read_html(html), "./body/ul/li"), as.character, character(1))
  instructions <- gsub("^\\s*<li>(.*)</li>\\s*$", "\\1", vec)
  if(length(instructions) == 0) {
    stop("No instructions could be extracted. Make sure to use a markdown list under \"*** =instructions.\"")
  }
  return(instructions)
}

extract_as_list <- function(x) {
  as.list(extract_as_vec(x))
}

# Extract R Markdown
# default_name is the default name of r markdown code chunk
#' @importFrom RJSONIO toJSON
extract_markdown <- function(x, default_name) {
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
    
    return(RJSONIO::toJSON(code))
  } else {
    code <- ""
    names(code) <- default_name
    return(RJSONIO::toJSON(code))
  }
}

# Extract challenge exercise data
extract_named_list <- function(x) {
  if(is.null(x) || nchar(x) == 0) {
    return(NULL)
  }
  html <- markdownToHTML(text = x, fragment.only = TRUE)
  lines <- str_split(html, "\n")[[1]]
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


# Extract skills from an exercise
# Both in *** =skills part as well as in --- type... header (preferred) is supported
extract_skills <- function(x) {
  if(is.null(x)) {
    return(NULL)
  }
  # support for old format
  if(is.list(x)) {
    x <- x$content
  }
  ids <- strsplit(gsub(" ", "", x), split = ",")[[1]]
  return(as.list(ids))
}

# Extract language
extract_lang <- function(x) {
  if(isTRUE(grepl("py", tolower(x)))) {
    lang <- "python"
  } else {
    # the default is R
    lang <- "r"
  }
  lang
}

# Extract video link
# Both with and without (preferred) code chunks is supported.
#' @importFrom stringr str_split
extract_video_link <- function(x) {
  if(is.null(x)) return(NULL)
  lines <- str_split(x, pattern = "\n")[[1]]
  if(any(grepl("^\\s*```+\\s*$", lines))) {
    return(extract_code(x))
  } else {
    return(gsub("\n", "", x))
  }
}
