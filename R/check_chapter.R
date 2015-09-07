check_chapter <- function(exercises) {
  ex_types <- sapply(exercises, function(x) x$type)
  timings <- timing_lut[ex_types]
  total_time <- sum(timings)
  message(sprintf("  -- Estimated time to take chapter: %s minutes.", total_time))
  if(total_time > max_time) {
    message(sprintf("     Try limiting the content to stay under %s minutes.", max_time))
  }
}

# how long each exercise takes to complete, in minutes
timing_lut <- c(VideoExercise = 5, 
                NormalExercise = 3, 
                MultipleChoiceExercise = 2, 
                ChallengeExercise = 4, 
                MarkdownExercise = 3)
max_time <- 60
