#' Generate course and chapter template
#'
#' The \code{author_course} function will:
#' \enumerate{
#'  \item create and open a `course.yml` file, with course information
#'  \item create and open a `chapter1.md` file, containing some exercise templates
#' }
#' 
#' @param lang The programming language of the exercises in the template exercises (default is "r")
#' @param simplified Whether or not to build a simplified course scaffold (default is FALSE)
#' @param open Whether or not to open the files in a text editor after creating it.
#' 
#' @examples
#' \dontrun{ 
#' author_course(lang = "python", simplified = TRUE)
#' author_course(lang = "python", simplified = FALSE)
#' author_course(lang = "r", simplified = TRUE)
#' author_course(lang = "r", simplified = FALSE)
#' }
#' 
#' @export
author_course <- function(lang = c("r", "python"), simplified = FALSE, open = TRUE) {
  if(file.exists(course_file)) {
    stop(sprintf("A file named %s already exists in your current working directory.", course_file))
  }
  lang <- match.arg(lang)
  
  message(sprintf("Creating course template %s in current directory ...", course_file))
  write(course_yml_template, file = course_file)
  
  if(open) file.edit(course_file)
  message(sprintf("After changing the details in %s, run 'upload_course()'", course_file))
  author_chapter(lang = lang, simplified = simplified, open = open)
}

#' Generate course and chapter template
#'
#' The \code{author_course} function will:
#' \enumerate{
#'  \item create and open a `course.yml` file, with course information
#'  \item create and open a `chapter1.md` file, containing some exercise templates
#' }
#' 
#' @inheritParams author_course
#' 
#' @examples
#' \dontrun{ 
#' author_chapter(lang = "python", simplified = TRUE)
#' author_chapter(lang = "python", simplified = FALSE)
#' author_chapter(lang = "r", simplified = TRUE)
#' author_chapter(lang = "r", simplified = FALSE)
#' }
#' 
#' @export
#'@importFrom stringr str_extract
author_chapter <- function(lang = c("r", "python"), simplified = FALSE, open = TRUE) {
  lang <- match.arg(lang)
  chapter_file <- ifelse(lang == "r", "chapter1.Rmd", "chapter1.md")
  while(file.exists(chapter_file)) {
    num <- as.numeric(str_extract(chapter_file, "\\d+")) + 1
    chapter_file <- gsub("\\d+", as.character(num), chapter_file)
  }
  message(sprintf("Creating chapter template %s in current directory ...", chapter_file))
  write(chapter_yml_template, file = chapter_file)
  ex_to_add <- c("NormalExercise", "MultipleChoiceExercise", "VideoExercise")
  lapply(ex_to_add, add_exercise, chapter_file = chapter_file, lang = lang, simplified = simplified)
  
  if(open) file.edit(chapter_file)
  message(sprintf("To upload this chapter, run 'upload_chapter(%s)'", chapter_file, chapter_file))
  return(chapter_file)
}

#' Add exercise to chapter file
#' 
#' @param chapter_file path to the chapter you want to append a template to
#' @param type type of the exercise you want to add a template for (default is "NormalExercise")
#' @inheritParams author_course
#' 
#' @examples
#' \dontrun{ 
#' add_exercise("chapter1.Rmd", lang = "r", type = "NormalExercise")
#' add_exercise("chapter1.Rmd", lang = "r", type = "MultipleChoiceExercise")
#' add_exercise("chapter1.Rmd", lang = "r", type = "VideoExercise")
#' add_exercise("chapter1.md", lang = "python", type = "NormalExercise")
#' add_exercise("chapter1.md", lang = "python", type = "MultipleChoiceExercise")
#' add_exercise("chapter1.md", lang = "python", type = "VideoExercise")
#' }
#' 
#' @export
add_exercise <- function(chapter_file,
                         lang = c("r", "python"),
                         type = c("NormalExercise", 
                                  "MultipleChoiceExercise", 
                                  "VideoExercise"), 
                         simplified = FALSE) {
  lang <- match.arg(lang)
  type <- match.arg(type)
  stopifnot(file.exists(chapter_file))
  message(sprintf("Adding template for %s ... ", type), appendLF = FALSE)
  template <- subset(templates, subset = lang_col == lang & type_col == type & simple_col == simplified)$template
  write(template, file = chapter_file, append = TRUE)
  message("Done.")
}