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
  output_list = list( force = force,
                      skip_validation = skip_validation,
                      course = course$id,
                      email  = .DATACAMP_ENV$email,
                      chapter=list(
                        title_meta=payload$title_meta,
                        title=payload$title,
                        description=payload$description,
                        free_preview=payload$free_preview,
                        attachments=list(
                          slides_link=payload$attachments$slides_link
                        )
                      )
  )
  
  # Extract chapter id and index from course.yml. If found, add to outputList
  course = load_course_yml()
  chapter_index = get_chapter_id(file_name)
  if (length(chapter_index) != 0) { # existing chapter, add info to output_list
    output_list$chapter$id = as.integer(course$chapters[[chapter_index]])
    output_list$chapter$number = chapter_index
  }
  
  # Extract for each exercise the relevant information:
  slides = payload$slides 
  exerciseList = list() 
  for(i in 1:length(slides)) {
    slide = slides[[i]]
    if( !is.null(slide$type) && slide$type == "VideoExercise") {
      exerciseList[[i]] = list(  title         = html2txt(slide$title),
                                 assignment    = slide$content, 
                                 number        = slide$num,
                                 aspect_ratio  = slide$aspect_ratio,
                                 video_link    = gsub("[\r\n]", "", extract_code(slide$video_link$content)) ,
                                 type          = "VideoExercise")
    } else if( !is.null(slide$type) && slide$type == "MultipleChoiceExercise" ) {
      exerciseList[[i]] = list(  title         = html2txt(slide$title),
                                 assignment    = slide$content, 
                                 number        = slide$num,
                                 instructions  = make_multiple_choice_vector(slide$instructions$content), 
                                 hint          = slide$hint$content,
                                 sample_code   = extract_code( slide$sample_code$content ),
                                 solution      = extract_code( slide$solution$content ),
                                 sct           = extract_code( slide$sct$content ),
                                 pre_exercise_code = extract_code( slide$pre_exercise_code$content),
                                 type          = "MultipleChoiceExercise")
    } else if( !is.null(slide$type) && slide$type == "MarkdownExercise") {
      exerciseList[[i]] = list(  title         = html2txt(slide$title),
                                 assignment    = slide$content, 
                                 number        = slide$num,
                                 instructions  = slide$instructions$content, 
                                 hint          = slide$hint$content,
                                 sample_code   = extract_markdown( slide$sample_code$content, "my_document.Rmd"),
                                 solution      = extract_markdown( slide$solution$content, "my_solution.Rmd"),
                                 sct           = extract_code( slide$sct$content ),
                                 pre_exercise_code = extract_code( slide$pre_exercise_code$content),
                                 type          = "MarkdownExercise")
    } else if( is.null(slide$type) || slide$type == "NormalExercise") {
      exerciseList[[i]] = list(  title         = html2txt(slide$title),
                                 assignment    = slide$content, 
                                 number        = slide$num,
                                 instructions  = slide$instructions$content, 
                                 hint          = slide$hint$content,
                                 sample_code   = extract_code( slide$sample_code$content),
                                 solution      = extract_code( slide$solution$content),
                                 sct           = extract_code( slide$sct$content ),
                                 pre_exercise_code = extract_code( slide$pre_exercise_code$content),
                                 type          = "NormalExercise")
    } else {
      stop(sprintf("Exercise %i is of an unknown exercise type", i))
    }
  }
  
  # Join everything: 
  output_list$chapter$exercises = exerciseList 
  
  # Make JSON: 
  RJSONIO::toJSON(output_list)
}