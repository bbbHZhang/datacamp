# files and paths
course_file <- "course.yml"
chapter_pattern <- "chapter%s.%smd"
chapter_search_pattern <- "chapter\\d+\\.R?md"
index_yaml <- "index.yaml"
doc_url <- "http://docs.datacamp.com/teach"

# Information message
have_a_look <- sprintf("Have a look at the documentation on %s.", doc_url)
no_response <- paste("Something went wrong. We didn't get a valid response from the datacamp server.",
                     "Try again or contact info@datacamp.com in case you keep experiencing this problem.")
specify_lang <- "Make sure to define 'lang', the programming language of your course. Set this to \"r\" or \"python\"."
specify_simplified <- "Make sure to define 'simplified', whether or not to generate simplified templates. Set this TRUE or FALSE."

# split string into separate lines (copied from knitr)
split_lines <- function(x) {
  if (length(grep("\n", x)) == 0L) 
    return(x)
  con = textConnection(x)
  on.exit(close(con))
  readLines(con)
}

parse_elements <- function(raw_part) {
  # element_details <- parse_meta(raw_part)
  splitup <- split_meta(raw_part)
  element <- parse_meta(splitup[1])
  element$content <- splitup[2]
  element
}

#' @importFrom stringr str_split_fixed
split_meta <- function(blocks){
  split_block <- function(block){
    if (grepl("^\\s*\\{", block)){
      block <- str_split_fixed(block, "}\n", 2)
      block[1] <- paste(block[1], "}")
    } else {
      block <- str_split_fixed(block, "\n", 2)
    }
    return(block)
  }
  t(sapply(blocks, split_block, USE.NAMES = F))
}

#' @importFrom stringr str_split_fixed
parse_meta <- function(meta){
  x <- strsplit(meta, ' ')[[1]]
  x <- sub('^=', 'name:', x)
  y <- str_split_fixed(x[grep(":", x)], ":", 2)
  y1 = y[,1]; y2 = y[,2]
  meta  = as.list(y2[y1 != 'class'])
  names(meta) = y1[y1 != 'class']
  meta
  #  filter_blank(meta)
}


# Utility function to convert the code words _tbt_ and _tast_ to triple backticks and triple asterisks respectively
# This is to escape from the backtick inception in R markdown files (R markdown containing R markdown...)
fix_specials <- function(code) {
  code <- gsub("_tbt_","```",code)
  code <- gsub("_tast_","***",code)
  return(code)
}

datacamp_accessors <- function() {
  dc_data <- list()
  
  get = function(name) {
    if(missing(name)) {
      dc_data
    } else {
      dc_data[[name]]
    }
  }
  
  set = function(...) {
    dots = list(...)
    dc_data <<- merge(dots)
    invisible(NULL)
  }
  
  clear = function() {
    dc_data <<- list()
    invisible(NULL)
  }
  
  merge = function(values) merge_list(dc_data, values)
  
  list(get = get, set = set, clear = clear)
}

merge_list = function(x, y) {
  x[names(y)] = y
  x
}

datacamp <- datacamp_accessors()

