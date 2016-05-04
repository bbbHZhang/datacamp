parse_exercise <- function(raw_ex, index, htmlify) {
  raw_parts <- str_split(raw_ex, "\n\\*{3}")[[1]]
  
  parts <- lapply(raw_parts, parse_elements)
  if (length(parts) > 1){
    main  <- parts[[1]]
    names(main)[which(names(main) == "content")] <- "body"
    
    others <- parts[-1]
    nms <- lapply(others, `[[`, "name")
    contents <- lapply(others, `[[`, "content")
    names(contents) <- nms
    exercise <- c(main, contents)
  } else {
    stop("Something went wrong during parsing; there's not enough content for some exercises.")
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
  
  # change e.g. =sample_code to name:sample_code
  x <- sub('^=', 'name:', x)
  
  y <- str_split_fixed(x[grep(":", x)], ":", 2)
  meta  = as.list(y[,2])
  names(meta) = y[,1]
  return(meta)
}


get_commons <- function(ex, num, htmlify) {
  list(title = extract_title(ex$body),
       lang = extract_lang(ex$lang),
       xp = ex$xp,
       skills = extract_skills(ex$skills),
       assignment = extract_html(ex$body, htmlify),
       number = num)
}

render_exercise <- function(ex, num, htmlify, capstone) UseMethod("render_exercise")

render_exercise.default <- function(ex, num, htmlify) {
  stop("Unknown Exercise type. Make sure to specify this, e.g. --- type:NormalExercise")
}

render_exercise.NormalExercise <- function(ex, num, htmlify) {
  c(get_commons(ex, num, htmlify),
    list(type = "NormalExercise",
         instructions = extract_html(ex$instructions, htmlify),
         hint = extract_html(ex$hint, htmlify),
         pre_exercise_code = extract_code(ex$pre_exercise_code),
         sample_code = extract_code(ex$sample_code),
         solution = extract_code(ex$solution),
         sct = extract_code(ex$sct)))
}

render_exercise.InteractiveExercise <- function(ex, num, htmlify) {
  insts <- extract_as_list(ex$instructions)
  hints <- extract_as_list(ex$hint)
  if(length(insts) != length(hints)) {
    stop("The number of instructions does not match the number of hints.")
  }
  c(get_commons(ex, num, htmlify),
    list(type = "InteractiveExercise",
         instructions = insts,
         hint = hints,
         pre_exercise_code = extract_code(ex$pre_exercise_code),
         sample_code = extract_code(ex$sample_code),
         solution = extract_code(ex$solution),
         sct = extract_code(ex$sct)))
} 

render_exercise.MultipleChoiceExercise <- function(ex, num, htmlify) {
  c(get_commons(ex, num, htmlify),
    list(type = "MultipleChoiceExercise",
         instructions = extract_as_vec(ex$instructions),
         hint = extract_html(ex$hint, htmlify),
         pre_exercise_code = extract_code(ex$pre_exercise_code),
         sct = extract_code(ex$sct)))
}

render_exercise.VideoExercise <- function(ex, num, htmlify) {
  c(get_commons(ex, num, htmlify),
    list(type = "VideoExercise",
         aspect_ratio = ex$aspect_ratio,
         video_link = extract_link(ex$video_link),
         video_stream = extract_link(ex$video_stream),
         video_hls = extract_link(ex$video_hls)))
}

render_exercise.MarkdownExercise <- function(ex, num, htmlify) {
  c(get_commons(ex, num, htmlify),
    list(type = "MarkdownExercise",
         instructions = extract_html(ex$instructions, htmlify),
         hint = extract_html(ex$hint, htmlify),
         pre_exercise_code = extract_code(ex$pre_exercise_code),
         sample_code = extract_markdown(ex$sample_code),
         solution = extract_markdown(ex$solution),
         sct = extract_code(ex$sct)))
}

render_exercise.SwirlExercise <- function(ex, num, htmlify) {
  c(get_commons(ex, num, htmlify),
    list(type = "SwirlExercise",
         swirl_course = extract_link(ex$swirl_course),
         swirl_lesson = extract_link(ex$swirl_lesson)))
}

render_exercise.RStudioMultipleChoiceExercise <- function(ex, num, htmlify) {
  c(get_commons(ex, num, htmlify),
    list(type = "RStudioMultipleChoiceExercise",
         attachments = extract_attachments(ex$attachments)))
}

render_exercise.CapstoneVideoExercise <- function(ex, num, htmlify) {
  x <- c(render_exercise.VideoExercise(ex, num, htmlify),
         list(id = ex$id, nxt = ex$nxt, optimal = as.logical(ex$optimal)))
  x$type <- "CapstoneVideoExercise"
  return(x)
}

render_exercise.CapstoneMultipleChoiceExercise <- function(ex, num, htmlify) {
  x <- c(render_exercise.MultipleChoiceExercise(ex, num, htmlify),
         list(video_link = extract_link(ex$video_link),
              video_stream = extract_link(ex$video_stream),
              video_hls = extract_link(ex$video_hls)),
         list(id = ex$id, optimal = as.logical(ex$optimal)))
  x$type <- "CapstoneMultipleChoiceExercise"
  return(x)
}

render_exercise.CapstoneNormalExercise <- function(ex, num, htmlify) {
  x <- c(render_exercise.NormalExercise(ex, num, htmlify),
         list(id = ex$id, nxt = ex$nxt, optimal = as.logical(ex$optimal)))
  x$type <- "CapstoneNormalExercise"
  return(x)
}
