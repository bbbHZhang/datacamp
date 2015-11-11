parse_exercise <- function(raw_ex, index) {
  raw_parts <- str_split(raw_ex, "\n\\*{3}")[[1]]
  parts <- lapply(raw_parts, parse_elements)
  if (length(parts) > 1){
    main  <- parts[[1]]
    named <- Filter(function(z) !is.null(z$name), parts[-1])
    names(named) <- lapply(named, '[[', "name")
    exercise <- c(main, named)
  } else {
    exercise <- parts[[1]]
  }
  
  class(exercise) <- exercise$type
  return(render_exercise(exercise, index))
}

render_exercise <- function(ex, num) UseMethod("render_exercise")

render_exercise.default <- function(ex, num) {
  stop("Unknown Exercise type. Make sure to specify this, e.g. --- type:NormalExercise")
}

get_commons <- function(ex, num) {
  list(title = extract_title(ex$content),
       language = extract_lang(ex$lang),
       xp = ex$xp,
       skills = extract_skills(ex$skills),
       assignment = extract_html(ex$content),
       number = num)
}

render_exercise.NormalExercise <- function(ex, num) {
  c(get_commons(ex, num),
    list(type = "NormalExercise",
         instructions = extract_html(ex$instructions$content),
         hint = extract_html(ex$hint$content),
         pre_exercise_code = extract_code(ex$pre_exercise_code$content),
         sample_code = extract_code(ex$sample_code$content),
         solution = extract_code(ex$solution$content),
         sct = extract_code(ex$sct$content)))
}

render_exercise.InteractiveExercise <- function(ex, num) {
  insts <- extract_as_list(ex$instructions$content)
  hints <- extract_as_list(ex$hint$content)
  if(length(insts) != length(hints)) {
    stop("The number of instructions does not match the number of hints.")
  }
  c(get_commons(ex, num),
    list(type = "InteractiveExercise",
         instructions = insts,
         hint = hints,
         pre_exercise_code = extract_code(ex$pre_exercise_code$content),
         sample_code = extract_code(ex$sample_code$content),
         solution = extract_code(ex$solution$content),
         sct = extract_code(ex$sct$content)))
} 

render_exercise.MultipleChoiceExercise <- function(ex, num) {
  c(get_commons(ex, num),
    list(type = "MultipleChoiceExercise",
         instructions = extract_as_vec(ex$instructions$content),
         hint = extract_html(ex$hint$content),
         pre_exercise_code = extract_code(ex$pre_exercise_code$content),
         sct = extract_code(ex$sct$content)))
}

render_exercise.VideoExercise <- function(ex, num) {
  c(get_commons(ex, num),
    list(type = "VideoExercise",
         aspect_ratio = ex$aspect_ratio,
         video_link = extract_video_link(ex$video_link$content),
         video_stream = extract_video_link(ex$video_stream$content),
         video_hls = extract_video_link(ex$video_hls$content)))
}

render_exercise.MarkdownExercise <- function(ex, num) {
  c(get_commons(ex, num),
    list(type = "MarkdownExercise",
         instructions = extract_html(ex$instructions$content),
         hint = extract_html(ex$hint$content),
         pre_exercise_code = extract_code(ex$pre_exercise_code$content),
         sample_code = extract_markdown(ex$sample_code$content, "my_document.Rmd"),
         solution = extract_markdown(ex$solution$content, "solution.Rmd"),
         sct = extract_code(ex$sct$content)))
}

render_exercise.SwirlExercise <- function(ex, num) {
  c(get_commons(ex, num),
    list(type = "SwirlExercise",
         swirl_course = extract_code(ex$swirl_course$content),
         swirl_lesson = extract_code(ex$swirl_lesson$content)))
}

render_exercise.ChallengeExercise <- function(ex, num) {
  c(get_commons(ex, num),
    list(type = "ChallengeExercise",
         challenge_steps = extract_named_list(ex$challenge_steps$content),
         challenge_goal = extract_named_list(ex$challenge_goal$content),
         solution = extract_code(ex$solution$content),
         sct = extract_code(ex$sct$content),
         pre_exercise_code = extract_code(ex$pre_exercise_code$content)))
} 

