#' Generate course and chapter scaffold
#'
#' The \code{author_course} function will:
#' \enumerate{
#'  \item create and open a `course.yml` file, a scaffold to provide the necessary course information.
#'  \item create and open a `chapter1.md` file, a scaffold for creating your first chapter.
#' }
#' 
#' In the `course.yml` file a course title, course author and course description should be provided. Next, it also contains the unique 
#' course ID, and a list of the chapters within the course. The `chapter1.md` file provides the structure and building blocks of the 
#' first chapter.
#' 
#' @param language The programming language of the course you want to build.
#' @param simplified Whether or not to build a simplified course scaffold.
#' @param open Whether or not to open the file in a text editor after creating it.
#' @return No return values.
#' @examples
#' \dontrun{ 
#' author_course()
#' }
#' 
#' @export
author_course = function(language = c("r", "python"), simplified = FALSE, open = TRUE) {
  language <- match.arg(language)
  
  message("Creating course.yml template in current directory ...", appendLF = FALSE)
  write(course_yml_template, file = course_file)
  message("Done.")
  message("Creating chapter template in current directory ...", appendLF = FALSE)
  chapter_file <- ifelse(language == "r", "chapter1.Rmd", "chapter1.md")
  write(chapter_yml_template, file = chapter_file)
  add_exercise(chapter_file = chapter_file, language = language, type = "NormalExercise", simplified = simplified)
  add_exercise(chapter_file = chapter_file, language = language, type = "MultipleChoiceExercise", simplified = simplified)
  add_exercise(chapter_file = chapter_file, language = language, type = "VideoExercise", simplified = simplified)
  message("Done.")
  
  if(open) {
    message("Opening course and chapter file...", appendLF = FALSE)
    file.edit(course_file)
    file.edit(chapter_file)
    message("Done.")
  }
  message("You can start editing your course.")
  message(sprintf("After changing the details in %s, run upload_course()", course_file))
  message(sprintf("Next, you can start working in %s. To upload this chapter, run upload_chapter(%s)", chapter_file, chapter_file))
}


#' Add exercise to chapter file
#' 
#' @param chapter_file path to the chapter you want to append a template to
#' @param type type of the exercise you want to add a template for
#' @param simplified whether or not to add a simplified template (only TRUE is supported for the moment)
#' @export
add_exercise <- function(chapter_file,
                         language = c("r", "python"),
                         type = c("NormalExercise", 
                                  "MultipleChoiceExercise", 
                                  "VideoExercise"), 
                         simplified = FALSE) {
  language <- match.arg(language)
  type <- match.arg(type)
  stopifnot(file.exists(chapter_file))
  
  template <- subset(templates, subset = lang_col == language & type_col == type & simple_col == simplified)$template
  write(template, file = chapter_file, append = TRUE)
}