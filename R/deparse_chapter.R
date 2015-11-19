deparse_chapter <- function(chapter_list, chapter_file) {
  chapter_meta <- chapter_list[-which(names(chapter_list) == "exercises")]
  write(as.yaml(chapter_meta), file = chapter_file)
  jumbo_string <- lapply(chapter_list$exercises, function(ex) {
    
  })
}

deparse_exercise <- function(ex) UseMethod("deparse_exercise")

build_commons <- function(ex) {
  sprintf("--- type:%s lang:%s skills:%s xp:%s\n## %s\n\n%s",
          ex$type, ex$lang, paste(ex$skills, collapse = ","), ex$xp,
          ex$title, ex$assignment)
}
deparse_exercise.default <- function(ex) {
  stop("Unknown Exercise type when deparsing exercises")
}

deparse_exercise.NormalExercise <- function(ex) {
  
}