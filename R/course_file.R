#' @importFrom yaml yaml.load_file
load_course_file = function() { 
  # Step 1: Load the yaml file such that we have a list with all course information:
  if (!file.exists(course_file)) {
    stop(sprintf("%s is not found in your current working directory.", course_file))
  }
  
  # If there is old yaml syntax in there, replace it
  yml_content <- readLines(course_file)
  if(any(grepl(pattern = "^- ", yml_content))) {
    write(gsub("^- ", "  ", yml_content), file = course_file)
  }
  
  # Parse the yaml file
  course <- try(suppressWarnings(yaml.load_file(course_file)), silent = TRUE)
  if (inherits(course,"try-error")) {
    stop(sprintf("An error occurred when reading your course file.\n%s",have_a_look))
  }
  
  # Check the course yaml object on inconsistencies.
  check_course_object(course)
  
  return(course)
}

#' @importFrom yaml as.yaml
add_id_to_course_file = function(course_id) {
  course <- load_course_file()
  if (is.null(course$id)) {
    # Add id to the front of the list.
    new_course <- c(list(id = as.integer(course_id)), course)
    yaml_output <- as.yaml(new_course, line.sep="\n")
    write(yaml_output, file = course_file)
    message("The id was added to the course file.")
  } else if (course$id != course_id) {
    stop(sprintf(paste("Something strange happened.",
                       "Your course.yml file has course id %s whereas the server just returned %s.",
                       "Please compare your course.yml file with the web interface."), course$id, course_id))
  } else {
    return(NULL)
  }
}

get_chapter_index <- function(chapter_file_name) {
  course <- load_course_file()
  return(which(names(course$chapter) == chapter_file_name))
}

#' @importFrom yaml as.yaml
add_chapter_to_course_file <- function(chapter_file_name, chapter_id) {
  course <- load_course_file()
  chapter_index = get_chapter_index(chapter_file_name)
  if (length(chapter_index) == 0) {
    new_course <- course
    new_chapter <- structure(list(as.integer(chapter_id)), names = chapter_file_name)
    new_course$chapters <- c(new_course$chapters, new_chapter)
    yaml_output <- as.yaml(new_course, line.sep="\n")
    write(yaml_output, file = course_file)
    message("The chapter was added to your course file.")
  }
}

# See if course object contains all necessary information 
# Check if this information is valid
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