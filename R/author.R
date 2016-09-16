#' Generate course and chapter template
#'
#' The \code{author_course} function will:
#' \enumerate{
#'  \item create and open a `course.yml` file, with course information
#'  \item create and open a `chapter1.md` file, containing some exercise templates
#' }
#'
#' @param lang The programming language of the exercises in the template
#'
#' @examples
#' \dontrun{
#' author_course(lang = "python")
#' author_course(lang = "r")
#' }
#'
#' @export
author_course <- function(lang) {
  if(missing(lang)) stop(specify_lang)
  generate_course_template()
  author_chapter(lang = lang)
  return(invisible(course_file))
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
#' @examples
#' \dontrun{
#' author_chapter(lang = "python")
#' author_chapter(lang = "r")
#' author_chapter(lang = "r",
#'                title = "My Chapter",
#'                description = "This is my chapter!")
#' }
#'
#' @export
#'@importFrom stringr str_extract
author_chapter <- function(lang, title = NULL, description = NULL, internal = FALSE) {
  if(missing(lang)) stop(specify_lang)
  num <- 1
  chapter_file <- sprintf(chapter_pattern, num, ifelse(lang == "r", "R", ""))
  while(file.exists(chapter_file)) {
    num <- as.numeric(str_extract(chapter_file, "\\d+")) + 1
    chapter_file <- gsub("\\d+", as.character(num), chapter_file)
  }

  message(sprintf("Creating %s in current directory ... ", chapter_file), appendLF = FALSE)
  if(is.null(title)) title <- "Insert the chapter title here"
  if(is.null(description)) description <- "Insert the chapter description here"
  yaml_header <- sprintf(chapter_yaml_template, title, description)
  write(yaml_header, file = chapter_file)

  if(!isTRUE(internal)) {
    types <- c("VideoExercise", "MultipleChoiceExercise", "NormalExercise")
    lapply(types, add_exercise, chapter_file = chapter_file, lang = lang)
  }

  message("Done.")
  return(invisible(chapter_file))
}

#' Add exercise to chapter file
#'
#' @param chapter_file path to the chapter you want to append a template to
#' @param type type of the exercise you want to add a template for (default is "NormalExercise")
#' @param title Title of the exercise
#' @param content Short summary of the exercise (will be filled in in assignment part)
#' @inheritParams author_course
#'
#' @examples
#' \dontrun{
#' add_exercise("chapter1.Rmd", lang = "r", type = "NormalExercise")
#' add_exercise("chapter1.Rmd", lang = "r", type = "MultipleChoiceExercise")
#' add_exercise("chapter1.Rmd", lang = "r", type = "VideoExercise")
#' add_exercise("chapter1.Rmd", lang = "r", type = "NormalExercise",
#'              title = "My Normal Exercise", content = "This is a normal exercise.")
#' add_exercise("chapter1.Rmd", lang = "r", type = "MultipleChoiceExercise")
#'              title = "My Multiple Choice Exercise", content = "This is an MCE.")
#' add_exercise("chapter1.Rmd", lang = "r", type = "VideoExercise",
#'              title = "My Video", content = "This video discusses interesting things.")
#'
#' add_exercise("chapter1.md", lang = "python", type = "NormalExercise")
#' add_exercise("chapter1.md", lang = "python", type = "MultipleChoiceExercise")
#' add_exercise("chapter1.md", lang = "python", type = "VideoExercise")
#' add_exercise("chapter1.md", lang = "python", type = "NormalExercise",
#'              title = "My Normal Exercise", content = "This is a normal exercise.")
#' add_exercise("chapter1.md", lang = "python", type = "MultipleChoiceExercise")
#'              title = "My Multiple Choice Exercise", content = "This is an MCE.")
#' add_exercise("chapter1.md", lang = "python", type = "VideoExercise",
#'              title = "My Video", content = "This video discusses interesting things.")
#' }
#'
#' @export
add_exercise <- function(chapter_file,
                         lang,
                         type = c("NormalExercise",
                                  "MultipleChoiceExercise",
                                  "VideoExercise",
                                  "RStudioMultipleChoiceExercise",
                                  "ShinyNormalExercise"),
                         title = NULL,
                         content = NULL) {
  if (missing(lang)) stop(specify_lang)
  type <- match.arg(type)
  stopifnot(file.exists(chapter_file))

  xp <- switch(type, NormalExercise = 100, MultipleChoiceExercise = 50, 
               VideoExercise = 50, RStudioMultipleChoiceExercise = 50,
               ShinyNormalExercise = 100)
  skills_id <- switch(lang, r = 1, python = 2, 0)
  ex_header <- sprintf("--- type:%s lang:%s xp:%s skills:%s", type, lang, xp, skills_id)
  if (is.null(title)) title <- paste("My", type)
  ex_title <- paste0("## ", title, "\n")
  if (is.null(content) && type != "VideoExercise") content <- "Assignment comes here. Use Markdown for text formatting."

  body <- switch(type,
                 NormalExercise = sprintf(normal_body, lang),
                 MultipleChoiceExercise = paste(sprintf(mce_body, lang), sct_mce_body[[lang]], sep= ""),
                 VideoExercise = sprintf(video_body),
                 RStudioMultipleChoiceExercise = sprintf(rstudio_mce_body, lang),
                 ShinyNormalExercise = sprintf(shiny_body, lang))

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
build_scaffold <- function(index_file = index_yaml, lang) {
  if(missing(lang)) stop(specify_lang)

  chapter_files <- dir(pattern = chapter_search_pattern)
  if(length(chapter_files) > 0) {
    dirname <- paste0("archived_",gsub("-|\\s|:", "_", Sys.time()))
    dir.create(dirname)
    file.rename(chapter_files, file.path(dirname, chapter_files))
    message(sprintf("Chapter files were found and moved to %s before creating new chapter files.", dirname))
  }

  if(!file.exists(course_file)) {
    generate_course_template()
  }

  outline <- yaml::yaml.load_file(index_file)
  for(chapter in outline) {
    chapter_file <- author_chapter(lang = lang,
                                   title = chapter$chapter_title,
                                   description = chapter$chapter_description,
                                   internal = TRUE)
    for(ex in chapter$exercises) {
      add_exercise(chapter_file = chapter_file, lang = lang,
                   type = ex$type, title = ex$title, content = ex$content)
    }
  }
  return(invisible())
}
