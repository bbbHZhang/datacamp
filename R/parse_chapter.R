#' Parse chapter file into R list
#'  
#' @param chapter_file path to the chapter file you want to parse
#' @param htmlify convert the text elements to HTML? TRUE by default
#' @param check check the code against datacamp guidelines? TRUE by default
#' 
#' @importFrom stringr str_split_fixed str_split
#' @importFrom yaml yaml.load
#' @export
parse_chapter <- function(chapter_file, htmlify = TRUE, check = TRUE) {
  # split yaml header from exercises
  splitup <- str_split_fixed(paste(readLines(chapter_file, warn = FALSE), collapse = '\n'), "\n---", 2)
  
  # get yaml info
  chapter_meta <- try(yaml.load(gsub("^---\n+", '', splitup[1])))
  if(inherits(chapter_meta, "try-error")) {
    stop(paste0("Something went wrong when parsing the yaml header of ", chapter_file, 
                ". Make sure to wrap your title and description in quotes if there are colons in there."))
  }
  
  # parse exercises
  if(splitup[2] == "") stop(sprintf("Add at least one exercise to %s before you try to upload it.", chapter_file))
  raw_exercises <- str_split(splitup[2], pattern = '\n\n---')[[1]]
  
  exercises = list()
  message("Rendering all exercises...")
  for(i in 1:length(raw_exercises)) {
    
    message(sprintf("  - %s. ", i), appendLF = FALSE)
    exercise <- parse_exercise(raw_exercises[[i]], i, htmlify)
    message(exercise$title)
    
    # check exercise for consistency
    if(htmlify || check) {
      check_exercise(exercise)  
    }
    exercises[[i]] <- exercise
  }

  # Check for duplicate titles
  if (any(duplicated(sapply(exercises, function(x) x$title)))) {
    stop(sprintf(paste("You have duplicate titles in %s.",
                       "The titles are used as unique identifiers inside chapters;",
                       "make sure they are unique."), chapter_file))
  }
    
  if(check) {
    check_chapter(exercises)  
  }
  
  if(!is.null(chapter_meta$capstone)) {
    ids <- sapply(exercises, `[[`, "id")
    numbers <- sapply(exercises, `[[`, "number")
    names(numbers) <- ids
    lut <- numbers
    
    for(i in seq_along(exercises)) {
      if(exercises[[i]]$type == "CapstoneVideoExercise" || exercises[[i]]$type == "CapstoneNormalExercise") {
        y <- lut[exercises[[i]]$nxt]
        names(y) <- NULL
        exercises[[i]]$next_exercise_number <- ifelse(is.na(y), 0, y)
      } else if(exercises[[i]]$type == "CapstoneMultipleChoiceExercise") {
        ids <- lut[gsub(":", "", gsub("id=", "", str_extract(exercises[[i]]$instructions, "id=.*?:")))]
        options <- gsub("id=.*?:\\s+?", "", exercises[[i]]$instructions)
        exercises[[i]]$instructions <- mapply(function(x, y) list(list(option = x, next_exercise_number = y)), options, ids, USE.NAMES = FALSE)
      } else {
        stop(sprintf("%s is not supported in a capstone chapter", exercises[[i]]$type))
      }
      # clean up
      exercises[[i]]$id <- NULL
      exercises[[i]]$nxt <- NULL
    }
  }
  
  message("Rendering all exercises done.")

  return(c(chapter_meta, list(exercises = exercises)))
}



