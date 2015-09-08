check_chapter <- function(exercises) {
  ex_types <- sapply(exercises, function(x) x$type)
  timings <- timing_lut[ex_types]
  total_time <- sum(timings)
  message(sprintf("  -- Estimated time to take chapter: %s minutes.", total_time))
  if(total_time > max_time) {
    message(sprintf("     Try limiting the content to stay under %s minutes.", max_time))
  }
}
