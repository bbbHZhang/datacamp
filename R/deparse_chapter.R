#' @export
deparse_chapter <- function(chapter_list, chapter_file) {
  chapter_meta <- chapter_list[-which(names(chapter_list) == "exercises")]
  write(as.yaml(chapter_meta), file = chapter_file)
  exercise_strings <- lapply(chapter_list$exercises, function(ex) {
    class(ex) <- c(ex$type, class(ex))
    deparse_exercise(ex)
  })
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
  x <- paste(build_commons(ex), 
             build_text(ex[c("instructions", "hint")]),
             build_code(ex[c("pre_exercise_code", "sample_code", "solution", "pre_exercise_code", "sct")], ex$lang), 
             collapse = "\n")
  x
}

build_text <- function(lst) {
  string_vecs <- mapply(function(x, y) {
    sprintf("*** =%s\n%s", x, y)
  }, names(lst), lst)
  paste(string_vecs, collapse = "\n")
}

build_code <- function(lst, lang) {
  string_vecs <- mapply(function(x, y) {
    sprintf("*** =%s\n```{%s}\n%s\n```\n", x, lang, y)
  }, names(lst), lst)
  paste(string_vecs, collapse = "\n")
}

deparse_exercise.MultipleChoiceExercise <- function(ex) {
  ex
  #   paste(build_commons(ex), 
  #         
  #         , collapse = "\n")
}

deparse_exercise.VideoExercise <- function(ex) {
  ex
  #   paste(build_commons(ex), 
  #         
  #         , collapse = "\n")
}

deparse_exercise.ChallengeExercise <- function(ex) {
  stop("No support for deparsing Challenge Exercises")
}

deparse_exercise.MarkdownExercise <- function(ex) {
  stop("No support for deparsing Markdown Exercises")
}