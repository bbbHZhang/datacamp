#' Add exercise to chapter file
#' 
#' @param chapter_file path to the chapter you want to append a template to
#' @param type type of the exercise you want to add a template for
#' @param simplified whether or not to add a simplified template (only TRUE is supported for the moment)
#' @export
add_exercise <- function(chapter_file,
                         type = c("NormalExercise", 
                                  "MultipleChoiceExercise", 
                                  "VideoExercise"), 
                         simplified = TRUE) {
  type <- match.arg(type)
  stopifnot(file.exists(chapter_file))
  
  template_file <- file.path(system.file("templates", package = "datacamp"), paste0(type, ".Rmd"))
  template <- paste(readLines(template_file), collapse = "\n")
  write(template, file = chapter_file, append = TRUE)
}