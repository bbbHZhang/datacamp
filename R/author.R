#' Generate course and chapter template
#'
#' The \code{author_course} function will:
#' \enumerate{
#'  \item create and open a `course.yml` file, with course information
#'  \item create and open a `chapter1.md` file, containing some exercise templates
#' }
#' 
#' @param lang The programming language of the exercises in the template
#' @param simplified Whether or not to build simplified templates.
#' 
#' @examples
#' \dontrun{ 
#' author_course(lang = "python")
#' author_course(lang = "r")
#' }
#' 
#' @export
author_course <- function(lang, simplified) {
  if(missing(lang)) stop(specify_lang)
  if(missing(simplified)) stop(specify_simplified)
  generate_course_template()
  author_chapter(lang = lang, simplified = simplified)
}

generate_course_template <- function() {
  if(file.exists(course_file)) {
    stop(sprintf("A file named %s already exists in your current working directory.", course_file))
  }
  message(sprintf("Creating %s in current directory ...", course_file), appendLF = FALSE)
  write(course_yaml_template, file = course_file)
  message("Done.")
}


#' Generate chapter file
#' 
#' Generate a chapter file with 3 exercise templates so you can start right away.
#' 
#' @param lang Programming language of the chapter
#' @param title Title of the the chapter (optional)
#' @param description Description of the chapter (optional)
#' @param internal Do not touch this parameter (for internal use)
#' @inheritParams author_course
#' 
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
author_chapter <- function(lang, simplified, title = NULL, description = NULL, internal = FALSE) {
  if(missing(lang)) stop(specify_lang)
  if(missing(simplified)) stop(specify_simplified)
  num <- 1
  chapter_file <- sprintf(chapter_pattern, num, ifelse(lang == "r", "R", ""))
  while(file.exists(chapter_file)) {
    num <- as.numeric(str_extract(chapter_file, "\\d+")) + 1
    chapter_file <- gsub("\\d+", as.character(num), chapter_file)
  }
  
  message(sprintf("Creating %s in current directory ... ", chapter_file), appendLF = FALSE)
  if(is.null(title)) title <- "Insert the chapter title here"
  if(is.null(description)) description <- "Add chapter description here."
  yaml_header <- sprintf(chapter_yaml_template, num, title, description)
  write(yaml_header, file = chapter_file)
  
  if(!isTRUE(internal)) {
    types <- c("VideoExercise", "MultipleChoiceExercise", "NormalExercise")
    lapply(types, add_exercise, chapter_file = chapter_file, lang = lang, simplified = simplified)
  }
  
  message("Done.")
  return(chapter_file)
}

#' Add exercise to chapter file
#' 
#' @param chapter_file path to the chapter you want to append a template to
#' @param type type of the exercise you want to add a template for (default is "NormalExercise")
#' @param title Title of the exercise
#' @param content Short summary of the exercise (will be filled in in assignment part)
#' @inheritParams author_course
#' 
#' @export
add_exercise <- function(chapter_file,
                         lang,
                         type = c("NormalExercise", 
                                  "MultipleChoiceExercise", 
                                  "VideoExercise"),
                         simplified,
                         title = NULL,
                         content = NULL) {
  if(missing(lang)) stop(specify_lang)
  if(missing(simplified)) stop(specify_simplified)
  type <- match.arg(type)
  stopifnot(file.exists(chapter_file))
  
  xp <- switch(type, NormalExercise = 100, MultipleChoiceExercise = 50, VideoExercise = 50)
  skills_id <- switch(lang, r = 1, python = 2, 0)
  ex_header <- sprintf("--- type:%s lang:%s xp:%s skills:%s", type, lang, xp, skills_id)
  if(is.null(title)) title <- paste("My", type)
  ex_title <- paste0("## ", title, "\n")
  if(is.null(content)) content <- "Assignment comes here. Use Markdown for text formatting."

  body <- switch(type,
                 NormalExercise = {
                   sample <- ifelse(simplified, "", sample)
                   sct <- ifelse(simplified, "", sct)
                   body <- sprintf(normal_body, sample, sct)
                   gsub("prog_lang", lang, body)
                 },
                 MultipleChoiceExercise = {
                   gsub("prog_lang", lang, mce_body)
                 },
                 VideoExercise = {
                   gsub("prog_lang", lang, video_body)
                 })
  template <- paste(ex_header, ex_title, content, body, sep = "\n")
  write(template, file = chapter_file, append = TRUE)
}


#' Build a scaffold for a YAML outlining the entire course
#' 
#' @param index_file The path of the index file, by default this is index.yaml in your current working directory.
#' @inheritParams author_course
#' 
#' @export
#' @importFrom yaml yaml.load_file
build_scaffold <- function(index_file = index_yaml, lang, simplified) {
  if(missing(lang)) stop(specify_lang)
    
  if(missing(simplified)) {
    stop("Make sure to define 'simplified', whether or not to scaffold simplified exercises.")
  }
    
  chapter_files <- dir(pattern = chapter_search_pattern)
  if(length(chapter_files) > 0) {
    dirname <- paste0("archived_",gsub("-|\\s|:", "_", Sys.time()))
    dir.create(dirname)
    file.rename(chapter_files, file.path(dirname, chapter_files))
    message(sprintf("Chapter files were found and moved to %s before creating new chapter files.", dirname))
  }

  if(!file.exists(index_file)) {
    generate_course_template()
  }
  
  outline <- yaml::yaml.load_file(index_file)
  for(chapter in outline) {
    chapter_file <- author_chapter(title = chapter$chapter_title, 
                                   description = chapter$chapter_description,
                                   lang = lang,
                                   internal = TRUE)
    for(ex in chapter$exercises) {
      add_exercise(chapter_file = chapter_file, lang = lang, 
                   type = ex$type, simplified = simplified,
                   title = ex$title, content = ex$content)
    }
  }
  message("To upload this entire course and all chapters, run 'upload_course(upload_chapters = TRUE)'")
}
