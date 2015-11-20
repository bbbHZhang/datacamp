#' Based on the structured list representing a chapter file, create the chapter file again.
#' 
#' @param chapter_list structured list resulting from \link{parse_chapter}
#' @param chapter_file Path of the file to which you want to write the list representing the chapter.
#' 
#' @examples 
#' \dontrun{
#' chapter_list <- parse_chapter("path_to_file", htmlify = FALSE, check = FALSE)
#' deparse_chapter(chapter_list, "chapterX.Rmd")
#' }
#'
#' @export
deparse_chapter <- function(chapter_list, chapter_file) {
  chapter_meta <- chapter_list[-which(names(chapter_list) == "exercises")]
  write(as.yaml(chapter_meta), file = chapter_file)
  exercise_strings <- sapply(chapter_list$exercises, function(ex) {
    class(ex) <- c(ex$type, class(ex))
    paste0(deparse_exercise(ex), "\n")
  })
  write(exercise_strings, file = chapter_file, append = TRUE)
}

build_commons <- function(ex) {
  sprintf("--- type:%s lang:%s skills:%s xp:%s\n## %s\n\n%s\n",
          ex$type, ex$lang, paste(ex$skills, collapse = ","), ex$xp,
          ex$title, ex$assignment)
}

deparse_exercise <- function(ex) UseMethod("deparse_exercise")

deparse_exercise.default <- function(ex) {
  stop("Unknown Exercise type when deparsing exercises")
}

deparse_exercise.NormalExercise <- function(ex) {
  paste0(build_commons(ex), 
         build_text(ex[c("instructions", "hint")]),
         build_code(ex[c("pre_exercise_code", "sample_code", "solution", "pre_exercise_code", "sct")], ex$lang), 
         collapse = "\n")
}

deparse_exercise.MultipleChoiceExercise <- function(ex) {
  paste0(build_commons(ex),
         paste("*** =instructions\n", paste0("- ", ex$instructions, collapse = "\n"), collapse = "\n"),
         build_text(ex["hint"]),
         build_code(ex[c("pre_exercise_code", "sct")], ex$lang),
         collapse = "\n")
}

deparse_exercise.VideoExercise <- function(ex) {
  paste0(build_commons(ex),
         build_text(ex[c("video_link", "video_stream", "video_hls")]),
         collapse = "\n\n")
}


deparse_exercise.ChallengeExercise <- function(ex) {
  stop("No support for deparsing Challenge Exercises")
}

deparse_exercise.MarkdownExercise <- function(ex) {
  stop("No support for deparsing Markdown Exercises")
}

build_text <- function(lst) {
  string_vecs <- mapply(function(x, y) {
    sprintf("*** =%s\n%s", x, y)
  }, names(lst), lst)
  paste0(paste0(string_vecs, collapse = "\n"),"\n")
}

build_code <- function(lst, lang) {
  string_vecs <- mapply(function(x, y) {
    sprintf("*** =%s\n```{%s}\n%s\n```\n", x, lang, y)
  }, names(lst), lst)
  paste0(paste0(string_vecs, collapse = "\n"), "\n")
}

