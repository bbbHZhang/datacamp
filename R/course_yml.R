#' Load course.yml file and check if all necessary info is there and is valid
#' 
#' @importFrom yaml yaml.load_file
load_course_yml = function() { 
  # Step 1: Load the yaml file such that we have a list with all course information:
  if (!file.exists("course.yml")) { 
    stop("Seems like there is no course.yml file in the current directory.") 
  }
  course = try(suppressWarnings(yaml.load_file("course.yml")))
  if (inherits(course,"try-error")) {
    stop(sprintf("There's a problem loading your course.yml file. Please check the documentation at %s.",doc_url()))
  }
  
  # Step 2: Check the course yaml object on inconsistencies. 
  # This is necessary since the rest of the code assumes a certain structure. 
  # Everything should be clean before it's uploaded to the back-end or further processed in R.
  check_course_object(course)
  
  return(course)
}

#' Add a course_id to the course.yml file
#' 
#' @param course_id id to be added
#' @importFrom yaml as.yaml
add_id_to_course_yml = function(course_id) {
  yaml_list = load_course_yml()
  if (is.null(yaml_list$id)) {
    # Add id to the front of the list. As.integer because could be num:
    yaml_list = append(yaml_list, list(id = as.integer(course_id)), after = 0)
    
    yaml_output = as.yaml(yaml_list,line.sep="\n")
    write(yaml_output, file="course.yml")
    message("The id was added to your course.yml file.")
  } else if (yaml_list$id != course_id) {
    stop(paste0("Something strange happened. Your course.yml file has course id ", yaml_list$id, ", whereas the server just returned ", course_id, " . Please compare your course.yml file with the web interface."))
  }
}

#' Get the chapter id from the course.yml for a particular chapter file.
#' 
#' @param file_name the chapter file name to get the id for
get_chapter_id = function(file_name) {
  course = load_course_yml()
  chapter_index = which(sapply(course$chapters, function(x) {names(x)}) == file_name)
  return(as.integer(chapter_index))
}

#' Add a chapter to the list of chapters in the course.yml file
#' 
#' @param chapter_file_name the chapter file name to add
#' @param chapter_id the id to assign to the chapter inside the course.yml file
add_chapter_to_course_yml = function(chapter_file_name, chapter_id) {
  chapter_index = get_chapter_id(chapter_file_name)
  if (length(chapter_index) == 0) {
    yaml_list = load_course_yml()
    
    n = length(yaml_list$chapters)
    yaml_list$chapters[[n+1]] = structure(list(chapter_id), names = chapter_file_name)
    
    yaml_output = as.yaml(yaml_list,line.sep="\n")
    write(yaml_output, file="course.yml")
    message("The chapter was added to your course.yml file.")
  }
}