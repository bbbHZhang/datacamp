#' Generate course and chapter scaffold
#'
#' The \code{author_course} function will:
#' \enumerate{
#'  \item create a folder in you current working directory with the name "course_name".
#'  \item create and open a `course.yml` file, a scaffold to provide the necessary course information.
#'  \item create and open a `chapter1.Rmd` file, a scaffold for creating your first chapter.
#' }
#' 
#' In the `course.yml` file a course title, course author and course description should be provided. Next, it also contains the unique 
#' course ID, and a list of the chapters within the course. The `chapter1.Rmd` file provides the structure and building blocks of the 
#' first chapter.    
#' 
#' @usage author_course(course_name, ...)
#' @param course_name String indicating the course name (and thus the name of the folder that will be created in your current working directory).
#' @param ... Extra arguments you'd like to pass to the function. Under the hood, the \code{author} function from the \code{slidify} package is called.
#' @return No return values.
#' @examples
#' \dontrun{ 
#' # This will create the new directory ../myNewTutorialName in your current working directory 
#' author_course("myNewTutorialName")
#' }
#' @export
author_course = function(course_name, ...) {
  require("slidify")
  message(paste0("Creating course directory ",course_name))
  message("Done.")
  message("Switching to course directory...")
  suppressMessages(author(deckdir = course_name,  use_git = FALSE, scaffold = system.file('skeleton', package = 'datacamp'), open_rmd = FALSE, ...))  
  message("Created course.yml and first chapter file.")
  file.edit("chapter1.Rmd")
  message("Now open these files and start editing your course.")
}

#' Create a new chapter 
#' 
#' Creates an R Markdown file for a new course chapter in the current working directory.
#' The R markdown file already contains a template which is opened for editing.
#' 
#' @param chapter_name Character with the name of the chapter you'd like to create. 
#' 
#' @examples
#' \dontrun{
#' author_chapter("chapter2")
#' }
#'@export
author_chapter = function(chapter_name=NULL) {
  if (is.null(chapter_name)) { 
    stop("Please provide a chapter name.") 
  } 
  if (!is_rmd(chapter_name)) {
    to_file_path = paste0(chapter_name, ".Rmd")
  } else { to_file_path = chapter_name }
  
  from_file_path = paste0(system.file('skeleton', package = 'datacamp'), "/chapter1.Rmd")
  file.copy(from_file_path, to_file_path)
  message(paste("Creating chapter: ", to_file_path, "..."))
  file.edit(to_file_path)
  message(paste0("Done. You can start editing ", to_file_path))
}