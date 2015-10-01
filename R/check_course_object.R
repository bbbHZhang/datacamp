#' Check the course object
#' 
#' @param course The course object to be checked
check_course_object <- function(course) {
  # Is any of the required fields missing?
  required_fields <- c("title", "author_field", "description")
  absent_fields <- !(required_fields %in% names(course))
  if (any(absent_fields)) {
    stop(sprintf("Looks like your course.yml file is missing the following field(s):\n%s\n%s", 
                 paste0("- ", required_fields[absent_fields], collapse="\n"), have_a_look))
  }
  
  # Is any of the required fields empty?
  required_fields_content <- course[required_fields]
  empty_fields <- sapply(required_fields_content, is_empty_field)
  if (any(empty_fields)) {
    stop(sprintf("Looks like your course.yml file is missing information for the field(s):\n%s\n%s", 
                 paste0("- ", required_fields[empty_fields], collapse="\n"), have_a_look))
  }
  
  # Are the chapter details listed as they should be?
  chapters <- course$chapters
  if(is.null(chapters)) {
    return(course)
  }
  
  chapter_error <- "Something is wrong with the chapters section in your course.yml file."
  chapter_names <- names(chapters)
  
  # Do the chapter ids exist?
  empty_chapters <- sapply(chapters, is_empty_field)
  if(any(empty_chapters)) {
    stop(sprintf("%s\nYour chapter id can't be empty.\n%s", chapter_error, have_a_look))
  }
  
  # Are the chapter ids unique?
  if(length(chapters) > length(unique(chapters))) {
    stop(sprintf("%s\nYour chapter ids should be unique.\n%s", chapter_error, have_a_look))
  }
  
  # Do the .Rmd files exist?
  chapter_files_existing <- file.exists(chapter_names)
  if (!all(chapter_files_existing)) {
    stop(sprintf("%s\nThe following files are listed in the course.yml but do not exist in your working directory:\n%s\n%s",
                 chapter_error, paste0("- ", names(chapters)[!chapter_files_existing], collapse = "\n"), have_a_look))
  }
  
  # Are the .Rmd filenames unique?
  if(length(chapter_names) > length(unique(chapter_names))) {
    stop(sprintf("%s\nYour chapter file names should be unique.\n%s", chapter_error, have_a_look))
  }
}

is_empty_field <- function(x) {
  return(is.null(x) || (x == "") || (x == " ") || is.na(x))
}