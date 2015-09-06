render_exercise <- function(ex, num) UseMethod("render_exercise")

render_exercise.default <- function(ex, num) {
  if(!is.null(ex$type) && !ex$type == "NormalExercise") {
    stop("Unknown Exercise Type!")
  }
  list(title = extract_title(ex$content),
       xp         = ex$xp,
       assignment = extract_html(ex$content), 
       instructions = extract_html(ex$instructions$content),
       hint = extract_html(ex$hint$content),
       sample_code   = extract_code(ex$sample_code$content),
       solution      = extract_code(ex$solution$content),
       sct           = extract_code(ex$sct$content),
       pre_exercise_code = extract_code(ex$pre_exercise_code$content),
       skills = extract_skills(ex$skills$content),
       type          = "NormalExercise",
       number = num)
}

render_exercise.MultipleChoiceExercise <- function(ex, num) {
  list(title         = extract_title(ex$content),
       xp            = ex$xp,
       assignment    = extract_html(ex$content),
       instructions  = extract_mc(ex$instructions$content), 
       hint          = extract_html(ex$hint$content),
       sct           = extract_code(ex$sct$content),
       pre_exercise_code = extract_code(ex$pre_exercise_code$content),
       skills = extract_skills(ex$skills$content),
       type          = "MultipleChoiceExercise",
       number = num) 
}

render_exercise.VideoExercise <- function(ex, num) {
  list(title         = extract_title(ex$content),
       xp            = ex$xp,
       assignment    = extract_html(ex$content),
       aspect_ratio  = ex$aspect_ratio,
       video_link    = extract_code(ex$video_link$content),
       video_stream  = extract_code(ex$video_stream$content),
       video_hls     = extract_code(ex$video_hls$content),
       skills = extract_skills(ex$skills$content),
       type          = "VideoExercise",
       number = num)
}

render_exercise.MarkdownExercise <- function(ex, num) {
  list(title         = extract_title(ex$content),
       xp            = ex$xp,
       assignment    = extract_html(ex$content),
       instructions  = extract_html(ex$instructions$content),
       hint          = extract_html(ex$hint$content),
       sample_code   = extract_markdown(ex$sample_code$content, "my_document.Rmd"),
       solution      = extract_markdown(ex$solution$content, "my_solution.Rmd"),
       sct           = extract_code(ex$sct$content),
       pre_exercise_code = extract_code(ex$pre_exercise_code$content),
       skills = extract_skills(ex$skills$content),
       type          = "MarkdownExercise",
       number = num)
}

render_exercise.SwirlExercise <- function(ex, num) {
  list(title         = extract_title(ex$content),
       xp            = ex$xp,
       assignment    = extract_html(ex$content),
       swirl_course  = extract_code(ex$swirl_course$content),
       swirl_lesson  = extract_code(ex$swirl_lesson$content),
       skills = extract_skills(ex$skills$content),
       type          = "SwirlExercise",
       number = num)
  
}
render_exercise.ChallengeExercise <- function(ex, num) {
  list(title          = extract_title(ex$content),
       xp = ex$xp,
       assignment = extract_html(ex$content), 
       challenge_steps = extract_named_list(ex$challenge_steps$content),
       challenge_goal = extract_named_list(ex$challenge_goal$content),
       solution = extract_code(ex$solution$content),
       sct = extract_code(ex$sct$content),
       pre_exercise_code = extract_code(ex$pre_exercise_code$content),
       skills = extract_skills(ex$skills$content),
       type = "ChallengeExercise",
       number = num)
} 