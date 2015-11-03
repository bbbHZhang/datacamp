#' Parse chapter file into R list
#'  
#' @param chapter_file path to the chapter file you want to parse
#' 
#' @importFrom stringr str_split_fixed str_split
#' @importFrom yaml yaml.load
#' @export
parse_chapter <- function(chapter_file) {
  splitup <- str_split_fixed(paste(readLines(chapter_file, warn = FALSE), collapse = '\n'), "\n---", 2)
  chapter_meta <- try(yaml.load(gsub("^---\n+", '', splitup[1])))
  if(inherits(chapter_meta, "try-error")) {
    stop(paste0("Something went wrong when parsing the yaml header of ", chapter_file, 
                ". Make sure to wrap your title and description in quotes if there are colons in there."))
  }
  
  if(splitup[2] == "") stop(sprintf("Add at least one exercise to %s before you try to upload it.", chapter_file))
  raw_exercises <- str_split(splitup[2], pattern = '\n\n---')[[1]]
  
  exercises = list()
  message("Rendering all exercises...")
  for(i in 1:length(raw_exercises)) {
    
    message(sprintf("  - %s. ", i), appendLF = FALSE)
    exercise <- parse_exercise(raw_exercises[[i]], i)
    message(exercise$title)
    
    # check exercise for consistency
    check_exercise(exercise)
    
    exercises[[i]] <- exercise
  }

  # Check for duplicate titles
  if (any(duplicated(sapply(exercises, function(x) x$title)))) {
    stop(sprintf(paste("You have duplicate titles in %s.",
                       "The titles are used as unique identifiers inside chapters;",
                       "make sure they are unique."), chapter_file))
  }
    
  check_chapter(exercises)
  message("Rendering all exercises done.")

  return(c(chapter_meta, list(exercises = exercises)))
}

