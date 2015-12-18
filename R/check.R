# Maximum number of instructions (or options in MCE)
max_number_instructions <- 5

# Maximum number of characters in exercise assignment
max_num_ass_char <- 800

# linters that should be ignored
linters_to_ignore <- c("trailing_blank_lines_linter", "trailing_whitespace_linter", "line_length_linter", "camel_case_linter")

# how long each exercise takes to complete, in minutes
timing_lut <- c(VideoExercise = 5,
                NormalExercise = 3,
                InteractiveExercise = 3,
                MultipleChoiceExercise = 2,
                ChallengeExercise = 4,
                MarkdownExercise = 3,
                SwirlExercise = 10,
                CapstoneVideoExercise = 1,
                CapstoneMultipleChoiceExercise = 1,
                CapstoneNormalExercise = 2)

max_time <- 60
min_time <- 30

check_chapter <- function(exercises) {
  ex_types <- sapply(exercises, function(x) x$type)
  timings <- timing_lut[ex_types]
  total_time <- sum(timings)
  message(sprintf("  -- Estimated time to take chapter: %s minutes.", total_time))
  if(total_time > max_time || total_time < min_time) {
    message(sprintf("     The ideal DataCamp chapter is between %s and %s minutes long.", min_time, max_time))
  }
}

check_exercise <- function(exercise) {
  check_code_blocks(exercise)
  check_assignment(exercise)
  check_instructions(exercise)
  check_hint(exercise)
}

check_code_blocks <- function(exercise) {
  if(exercise$type == "MarkdownExercise" || exercise$lang == "python") {
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
  file <- tempfile(fileext = ".R")
  write(code, file = file)
  
  lints <- try(lintr::lint(file), silent = TRUE)
  if(inherits(lints, "try-error")) {
    message(sprintf("\t> %s\n\t  lintr package encountered an error.\n", type))
  } else {
    for(lint in lints) {
      if(lint$linter %in% linters_to_ignore) {
        cat("")
      } else {
        message(sprintf("\t> %s\n\t  Code: %s\n\t  Line: %s\n\t  Column: %s\n\t  Message: %s\n", 
                        type, lint$line, lint$line_number, lint$column, lint$message))
      }
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
    num_ass_char <- nchar(gsub("(<(.*?)>|</(.*?)>|\\\n|\\\\)", "", exercise$assignment))
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
  
  if(is.null(exercise$instructions) || !nzchar(gsub("\n|\\s", "", exercise$instructions))) {
    message("\t> instructions:\n\t  You have not specified instructions!")
  } else {
    if(exercise$type == "MultipleChoiceExercise") {
      num_instr <- length(exercise$instructions)
    } else {
      num_instr <- length(vapply(xml_find_all(xml2::read_html(exercise$instructions), "./body/ul/li"), as.character, character(1)))  
    }
    
    if(num_instr > max_number_instructions) {
      message(sprintf("\t> instructions\n\t  You have %s top-level bullets. Try to limit yourself to %s.\n", 
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