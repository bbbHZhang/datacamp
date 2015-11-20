parse_exercise <- function(raw_ex, index, htmlify) {
  raw_parts <- str_split(raw_ex, "\n\\*{3}")[[1]]
  
  parts <- lapply(raw_parts, parse_elements)
  if (length(parts) > 1){
    main  <- parts[[1]]
    others <- parts[-1]
    names(others) <- lapply(others, `[[`, "name")
    exercise <- c(main, others)
  } else {
    stop("Something went wrong during parsing")
  }
  
  class(exercise) <- c(exercise$type, class(exercise))
  parsed_exercise <- render_exercise(exercise, index, htmlify)
  
  return(parsed_exercise)
}

parse_elements <- function(raw_part) {
  # element_details <- parse_meta(raw_part)
  splitup <- str_split_fixed(raw_part, "\n", 2)
  element <- parse_header(splitup[1])
  element$content <- splitup[2]
  return(element)
}

#' @importFrom stringr str_split_fixed
parse_header <- function(meta){
  x <- strsplit(meta, ' ')[[1]]
  
  # change =sample_code to name:sample_code
  x <- sub('^=', 'name:', x)
  
  y <- str_split_fixed(x[grep(":", x)], ":", 2)
  meta  = as.list(y[,2])
  names(meta) = y[,1]
  return(meta)
}


get_commons <- function(ex, num, htmlify) {
  list(title = extract_title(ex$content),
       lang = extract_lang(ex$lang),
       xp = ex$xp,
       skills = extract_skills(ex$skills),
       assignment = extract_html(ex$content, htmlify),
       number = num)
}

render_exercise <- function(ex, num, htmlify, capstone) UseMethod("render_exercise")

render_exercise.default <- function(ex, num, htmlify) {
  stop("Unknown Exercise type. Make sure to specify this, e.g. --- type:NormalExercise")
}

render_exercise.NormalExercise <- function(ex, num, htmlify) {
  c(get_commons(ex, num, htmlify),
    list(type = "NormalExercise",
         instructions = extract_html(ex$instructions$content, htmlify),
         hint = extract_html(ex$hint$content, htmlify),
         pre_exercise_code = extract_code(ex$pre_exercise_code$content),
         sample_code = extract_code(ex$sample_code$content),
         solution = extract_code(ex$solution$content),
         sct = extract_code(ex$sct$content)))
}

render_exercise.InteractiveExercise <- function(ex, num, htmlify) {
  insts <- extract_as_list(ex$instructions$content)
  hints <- extract_as_list(ex$hint$content)
  if(length(insts) != length(hints)) {
    stop("The number of instructions does not match the number of hints.")
  }
  c(get_commons(ex, num, htmlify),
    list(type = "InteractiveExercise",
         instructions = insts,
         hint = hints,
         pre_exercise_code = extract_code(ex$pre_exercise_code$content),
         sample_code = extract_code(ex$sample_code$content),
         solution = extract_code(ex$solution$content),
         sct = extract_code(ex$sct$content)))
} 

render_exercise.MultipleChoiceExercise <- function(ex, num, htmlify) {
  c(get_commons(ex, num, htmlify),
    list(type = "MultipleChoiceExercise",
         instructions = extract_as_vec(ex$instructions$content),
         hint = extract_html(ex$hint$content, htmlify),
         pre_exercise_code = extract_code(ex$pre_exercise_code$content),
         sct = extract_code(ex$sct$content)))
}

render_exercise.VideoExercise <- function(ex, num, htmlify) {
  c(get_commons(ex, num, htmlify),
    list(type = "VideoExercise",
         aspect_ratio = ex$aspect_ratio,
         video_link = extract_video_link(ex$video_link$content),
         video_stream = extract_video_link(ex$video_stream$content),
         video_hls = extract_video_link(ex$video_hls$content)))
}

render_exercise.MarkdownExercise <- function(ex, num, htmlify) {
  c(get_commons(ex, num, htmlify),
    list(type = "MarkdownExercise",
         instructions = extract_html(ex$instructions$content, htmlify),
         hint = extract_html(ex$hint$content, htmlify),
         pre_exercise_code = extract_code(ex$pre_exercise_code$content),
         sample_code = extract_markdown(ex$sample_code$content, "my_document.Rmd"),
         solution = extract_markdown(ex$solution$content, "solution.Rmd"),
         sct = extract_code(ex$sct$content)))
}

render_exercise.SwirlExercise <- function(ex, num, htmlify) {
  c(get_commons(ex, num, htmlify),
    list(type = "SwirlExercise",
         swirl_course = extract_code(ex$swirl_course$content),
         swirl_lesson = extract_code(ex$swirl_lesson$content)))
}

render_exercise.ChallengeExercise <- function(ex, num, htmlify) {
  c(get_commons(ex, num, htmlify),
    list(type = "ChallengeExercise",
         challenge_steps = extract_named_list(ex$challenge_steps$content),
         challenge_goal = extract_named_list(ex$challenge_goal$content),
         solution = extract_code(ex$solution$content),
         sct = extract_code(ex$sct$content),
         pre_exercise_code = extract_code(ex$pre_exercise_code$content)))
} 

render_exercise.CapstoneVideoExercise <- function(ex, num, htmlify) {
  c(get_commons(ex, num, htmlify),
    list(id = ex$id, nxt = ex$nxt),
    list(type = "CapstoneVideoExercise",
         aspect_ratio = ex$aspect_ratio,
         video_link = extract_video_link(ex$video_link$content),
         video_stream = extract_video_link(ex$video_stream$content),
         video_hls = extract_video_link(ex$video_hls$content)))
}

render_exercise.CapstoneMultipleChoiceExercise <- function(ex, num, htmlify) {
  c(get_commons(ex, num, htmlify),
    list(id = ex$id),
    list(type = "CapstoneMultipleChoiceExercise",
         instructions = extract_as_vec(ex$instructions$content),
         hint = extract_html(ex$hint$content, htmlify),
         pre_exercise_code = extract_code(ex$pre_exercise_code$content),
         sct = extract_code(ex$sct$content)))
}
