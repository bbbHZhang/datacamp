#' Create a JSON from the slidified chapter
#' @param file_name name of the chapter file
#' @param payload structure built by slidify package, containing all data in the chapter r markdown file.
#' @param force whether or not to impose the local structure onto the remote datacamp course.
#' @param skip_validation whether or not to skip validity of uploaded exercises on DataCamp's servers.
#' 
#' @importFrom RJSONIO toJSON
render_chapter_json_for_datacamp = function(file_name, payload, force, skip_validation) {
  # Extract basic course info:
  course = load_course_yml()
  if (is.null(course$id)) {
    stop("Error: course.yml does not contain a course id. Please upload your course before uploading chapters.")
  }
  output_list = list(force = force,
                     skip_validation = skip_validation,
                     course = course$id,
                     email = .DATACAMP_ENV$email,
                     chapter = payload$meta)
  
  # Extract chapter id and index from course.yml. If found, add to outputList
  course = load_course_yml()
  chapter_index = get_chapter_id(file_name)
  if (length(chapter_index) != 0) { # existing chapter, add info to output_list
    output_list$chapter$id = as.integer(course$chapters[[chapter_index]])
    output_list$chapter$number = chapter_index
  }
  
  # Extract for each exercise the relevant information:
  exercises = payload$exercises
  exerciseList = list()
  for(i in 1:length(exercises)) {
    message(paste0("Rendering exercise ", i, "..."))
    ex = exercises[[i]]
    class(ex) <- ex$type
    rendered_exercise <- render_exercise(ex)
    rendered_exercise$number <- i
    exerciseList[[i]] <- rendered_exercise
  }
  
  # Add list of rendered exercises to the output_list
  output_list$chapter$exercises = exerciseList 
  
  # Convert entire list to JSON
  RJSONIO::toJSON(output_list)
}