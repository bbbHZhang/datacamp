# Maximum number of instructions (or options in MCE)
max_number_instructions <- 5

# Maximum number of characters in exercise assignment
max_num_ass_char <- 1200

# linters that should be ignored
linters_to_ignore <- c("trailing_blank_lines_linter", "trailing_whitespace_linter", "line_length_linter", "camel_case_linter")

# how long each exercise takes to complete, in minutes
timing_lut <- c(VideoExercise = 5, 
                NormalExercise = 3, 
                MultipleChoiceExercise = 2, 
                ChallengeExercise = 4, 
                MarkdownExercise = 3)
max_time <- 60