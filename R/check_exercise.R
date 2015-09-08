check_exercise <- function(exercise) {
  check_code_blocks(exercise)
  check_assignment(exercise)
  check_instructions(exercise)
  check_hint(exercise)
}

check_code_blocks <- function(exercise) {
  if(exercise$type == "MarkdownExercise") {
    return(NULL)
  }
  chunk_names <- c("sample_code", "solution")
  selection <- exercise[chunk_names]
  chunks_to_check <- selection[!sapply(selection, is.null)]
  for(i in seq_along(chunks_to_check)) {
    diagnose_code(chunks_to_check[[i]], names(chunks_to_check)[i])
  }
}

#' @importFrom lintr lint
diagnose_code <- function(code, type) {
  file <- "code.R"
  write(code, file = file)
  lints <- lintr::lint(file)
  
  for(lint in lints) {
    if(lint$linter %in% linters_to_ignore) {
      cat("")
    } else {
      message(sprintf("\t> %s\n\t  Code: %s\n\t  Line: %s\n\t  Column: %s\n\t  Message: %s\n", 
                      type, lint$line, lint$line_number, lint$column, lint$message))
    }
  }
  unlink(file)
}

check_assignment <- function(exercise) {
  if(exercise$type == "VideoExercise") {
    return(NULL)
  }
  
  if(is.null(exercise$assignment)) {
    message("\t> assignment\n\t  You have not specified the assignment!")
  } else {
    num_ass_char <- nchar(exercise$assignment)
    if(num_ass_char > max_num_ass_char) {
      message(sprintf("\t> assignment\n\t  Counts %s characters. Try to limit yourself to %s.\n", 
                      num_ass_char, max_num_ass_char))
    }
  }
}

check_instructions <- function(exercise) {
  ex_types_instructions_required <- c("NormalExercise", "MultipleChoiceExercise", "MarkdownExercise")
  if(!(exercise$type %in% ex_types_instructions_required)) {
    return(NULL)
  }
  
  if(is.null(exercise$instructions)) {
    message("\t> instructions:\n\t  You have not specified instructions!")
  } else {
    if(exercise$type == "MultipleChoiceExercise") {
      num_instr <- length(exercise$instructions)
    } else {
      num_instr <- stringr::str_count(exercise$instructions, "<li>")
    }
    
    if(num_instr > max_number_instructions) {
      message(sprintf("\t> instructions\n\t  You have %s instructions. Try to limit yourself to %s.\n", 
                      num_instr, max_number_instructions))
    }
  }
}

check_hint <- function(exercise) {
  if(exercise$type %in% c("ChallengeExercise","VideoExercise")) {
    return(NULL)
  }
  
  if(is.null(exercise$hint)) {
    message("\t> hint:\n\t  You have not specified a hint!")
  }
}