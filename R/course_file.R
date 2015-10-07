#' Load course file and check if all necessary info is there and is valid
#' 
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
  course <- try(suppressWarnings(yaml.load_file(course_file)))
  if (inherits(course,"try-error")) {
    stop(sprintf("An error occurred when reading your course file.%s",have_a_look))
  }
  
  # Check the course yaml object on inconsistencies.
  check_course_object(course)
  
  return(course)
}

#' Add a course_id to the course.yml file
#' 
#' @param course_id id to be added
#' 
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

#' Get the chapter id from the course.yml for a particular chapter file.
#' 
#' @param chapter_file_name the chapter file name to get the id for
get_chapter_index <- function(chapter_file_name) {
  course <- load_course_file()
  return(which(names(course$chapter) == chapter_file_name))
}

#' Add a chapter to the list of chapters in the course.yml file
#' 
#' @param chapter_file_name the chapter file name to add
#' @param chapter_id the id to assign to the chapter inside the course.yml file
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