add_id_to_course_yml = function(course_id) {
  yaml_list = load_course_yaml()
  if (is.null(yaml_list$id)) {
    # Add id to the front of the list. As.integer because could be num:
    yaml_list = append(yaml_list, list(id = as.integer(course_id)), after = 0)
    
    yaml_output = as.yaml(yaml_list,line.sep="\n")
    write(yaml_output, file="course.yml")
    message("The id was added to your course.yml file.")
  } else if (yaml_list$id != course_id) {
    stop(paste0("Something strange happened. Your course.yml file has course id ", course$id, ", whereas the server just returned ", course_id, " . Please compare your course.yml file with the web interface."))
  }
}

get_chapter_id = function(file_name) {
  course = load_course_yaml()
  chapter_index = which(sapply(course$chapters, function(x) {names(x)}) == file_name)
  return(as.integer(chapter_index))
}

add_chapter_to_course_yml = function(chapter_file_name, chapter_id) {
  chapter_index = get_chapter_id(chapter_file_name)
  if (length(chapter_index) == 0) {
    yaml_list = load_course_yaml()
    
    n = length(yaml_list$chapters)
    yaml_list$chapters[[n+1]] = structure(list(chapter_id), names = chapter_file_name)
    
    yaml_output = as.yaml(yaml_list,line.sep="\n")
    write(yaml_output, file="course.yml")
    message("The chapter was added to your course.yml file.")
  }
}

# Load yaml:
load_course_yaml = function() { 
  # Step 1: Load the yaml file such that we have a list with all course information:
  if (!file.exists("course.yml")) { 
    stop("Seems like there is no course.yml file in the current directory.") 
  }
  course = try(suppressWarnings(yaml.load_file("course.yml")))
  if (inherits(course,"try-error")) {
    stop(paste("There's a problem loading your course.yml file. Please check the documentation to find out what the course.yml file should contain. Go to:",doc_url()))
  }
  
  # Step 2: Check the course yaml object on inconsistencies. 
  # This is necessary since the rest of the code assumes a certain structure. 
  # Everything should be clean before it's uploaded to the back-end or further processed in R.
  check_course_object(course)
  
  return(course)
}