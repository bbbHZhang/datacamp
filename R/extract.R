#' @importFrom stringr str_trim
extract_title <- function(x) {
  lines <- str_split(x, pattern = "\n")[[1]]
  pattern <- "^##\\s+(.*?)"
  hits <- grepl(pattern, lines)
  if (sum(hits) == 0) {
    stop("No title found.")
  }
  if (sum(hits) > 1) {
    stop("More than one title found.")
  }
  title_line <- lines[hits]
  title <- stringr::str_trim(gsub("##","",title_line))
  if (nchar(title) == 0) {
    stop("Make sure to specify a title.")
  }
  return(title)
}

# Extract html from a chunk of text
# If a second-level header is in the content, this is removed.
#' @importFrom markdown markdownToHTML
extract_html <- function(x, htmlify) {
  if (is.null(x) || nchar(x) == 0) {
    return("")
  }
  
  if (htmlify) {
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
  if (is.null(x)) return("")
  lines <- str_split(x, "\n")[[1]]
  lang_part <- "r|py"
  chunk_begin <- sprintf("^\\s*```+\\s*\\{[.]?%s(.*)\\}\\s*$", lang_part)
  chunk_end <- "^\\s*```+\\s*$"
  begin <- grep(chunk_begin, lines)
  end <- grep(chunk_end, lines)
  if (length(begin) != 1 || length(end) != 1 || begin > end) {
    stop("No code chunk, too many code chunks, or no valid code chunk found. Make sure you specify the correct language.")
  }
  
  if (end - begin == 1) {
    return("")
  } else {
    return(paste(lines[(begin + 1):(end - 1)], collapse = "\n"))
  }
}

#' @importFrom markdown markdownToHTML
#' @importFrom xml2 read_html xml_find_all
extract_as_vec <- function(x) {
  if (is.null(x) || nchar(x) == 0) {
    return(c("empty"))
  }
  html <- markdownToHTML(text = x, fragment.only = TRUE)
  vec <- vapply(xml_find_all(xml2::read_html(html), "./body/ul/li"), as.character, character(1))
  instructions <- gsub("^\\s*<li>(.*)</li>\\s*$", "\\1", vec)
  if (length(instructions) == 0) {
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
  if (!is.null(x) && nchar(x) != 0) {
    lines <- strsplit(x, split = "\n")[[1]]
    titles_pos <- grep("\\{\\{\\{(.*?)\\}\\}\\}", lines)
    if (length(titles_pos) == 0) {
      stop("Make sure to use {{{my_title.ext}}} to specify the file name.")
    } else {
      titles <- gsub("\\{\\{\\{|\\}\\}\\}","", lines[titles_pos])
      if (length(titles) != length(unique(titles))) {
        stop("You need to specify unique file names in sample_code or solution_code of markdown exercises.")  
      }
      starts <- titles_pos + 1
      ends <- c(titles_pos[-1] - 1, length(lines))
      extract <- function(lines, s, e) {
        file <- paste(lines[s:e], collapse = "\n")
        trimmed <- gsub("(^\n*)|(\n*$)", "", file)
        fix_specials(trimmed)
      }
      files <- mapply(extract, s = starts, e = ends, MoreArgs = list(lines = lines))
    }
  } else {
    stop("Make sure to specify sample_code and solution_code for markdown exercises.")
  }
  names(files) <- titles
  return(RJSONIO::toJSON(files))
}

# Extract skills from an exercise
# Both in *** =skills part as well as in --- type... header (preferred) is supported
extract_skills <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  # support for old format
  if (is.list(x)) {
    x <- x$content
  }
  ids <- strsplit(gsub(" ", "", x), split = ",")[[1]]
  if (any(as.integer(ids) > 8) || any(as.integer(ids) < 1)) {
    stop("Invalid skills ids: choose ids 1 to 7.")
  }
  return(as.list(ids))
}

# Extract language
extract_lang <- function(x) {
  if (isTRUE(grepl("py", tolower(x)))) {
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
extract_link <- function(x) {
  if (is.null(x)) return(NULL)
  lines <- str_split(x, pattern = "\n")[[1]]
  if (any(grepl("^\\s*```+\\s*$", lines))) {
    return(extract_code(x))
  } else {
    return(gsub("\n", "", x))
  }
}

#' @importFrom yaml yaml.load_file
extract_attachments <- function(x) {
  tryCatch(attachments <- yaml.load(x), error = function(e) {
    stop(paste("The YAML specifying the attachents not valid:\n", x, 
               "This was the error:", e$message, sep = "\n"))
  })
}
