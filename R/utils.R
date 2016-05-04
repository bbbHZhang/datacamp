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

datacamp_accessors <- function() {
  dc_data <- list()
  
  get = function(name) {
    if (missing(name)) {
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

