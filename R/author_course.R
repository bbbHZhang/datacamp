#' Generate course and chapter scaffold
#'
#' The \code{author_course} function will:
#' \enumerate{
#'  \item create a folder in you current working directory with the name "course_name".
#'  \item create and open a `course.yml` file, a scaffold to provide the necessary course information.
#'  \item create and open a `chapter1.md` file, a scaffold for creating your first chapter.
#' }
#' 
#' In the `course.yml` file a course title, course author and course description should be provided. Next, it also contains the unique 
#' course ID, and a list of the chapters within the course. The `chapter1.md` file provides the structure and building blocks of the 
#' first chapter.    
#' 
#' @param course_name String indicating the course name (and thus the name of the folder that will be created in your current working directory).
#' @param open Whether or not to open the file in a text editor after creating it.
#' @return No return values.
#' @examples
#' \dontrun{ 
#' # This will create the new directory my_course in your current working directory 
#' author_course("my_course")
#' }
#' 
#' @export
author_course = function(course_name, open = TRUE) {
  if(!file.exists(course_name)) {
    message(paste0("Creating course directory ",course_name, "..."))
    dir.create(course_name)
  }
  message("Creating scaffold (chapter and course file)...")
  scaffold_dir <- system.file('skeleton', package = 'datacamp')
  file.copy(list.files(scaffold_dir, full.names = TRUE), course_name, recursive = TRUE)
  message("Switching to course directory...")
  setwd(course_name)
  if(open) 
    message("Opening the scaffolded chapter file...")
    file.edit("chapter1.md")
  message("You can start editing your course!")
}