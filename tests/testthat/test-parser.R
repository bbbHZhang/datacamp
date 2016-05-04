context("parser")

test_that("parse_chapter, parse_exercise and render_exercise work as expected", {
  chapter_file <- author_chapter(lang = "r", internal = TRUE)
  write(paste0("--- type:NormalExercise xp:100 skills:1,3 lang:r\n## Test\nAssignment\n",
               "*** =instructions\n- instruction 1\n- instruction 2\n\n",
               "*** =hint\njust a hint\n\n",
               "*** =pre_exercise_code\n```{r}\nx <- 5\n```\n",
               "*** =sample_code\n```{r}\ny <- 3\n```\n",
               "*** =solution\n```{r}\ny <- 6\n```\n",
               "*** =sct\n```{r}\nsuccess_msg(\"OK\")\n```\n")
        , chapter_file, append = TRUE)
  write(paste0("--- type:MultipleChoiceExercise xp:50 skills:1\n## MCE\nAssignment\n",
               "*** =instructions\n- option 1\n- option 2\n\n",
               "*** =hint\njust a hint\n\n",
               "*** =pre_exercise_code\n```{r}\nx <- 5\n```\n",
               "*** =sct\n```{r}\ntest_mc(1)\n```\n")
        , chapter_file, append = TRUE)
  write(paste0("--- type:VideoExercise xp:50 skills:2 lang:python\n## Video\nAssignment\n",
               "*** =video_link\nthis_is_the_link\n\n",
               "*** =video_stream\n```{python}\nthis_is_another_link\n```\n")
        , chapter_file, append = TRUE)
  write(paste0("--- type:MarkdownExercise xp:100 skills:1 lang:r\n## Markdown\nAssignment\n",
               "*** =instructions\n- instruction 1\n- instruction 2\n\n",
               "*** =hint\njust a hint\n\n",
               "*** =pre_exercise_code\n```{r}\nx <- 5\n```\n",
               "*** =sample_code\n{{{my_doc.Rmd}}}\nsome_code_here\n",
               "*** =solution\n{{{solution.Rmd}}}\nsome_code_here\n",
               "*** =sct\n```{r}\nsuccess_msg(\"OK\")\n```\n")
        , chapter_file, append = TRUE)
  
  
  out <- parse_chapter(chapter_file)
  unlink(chapter_file)
  
  expect_equal(out$title, "Insert the chapter title here")
  expect_equal(out$description, "Insert the chapter description here")
  expect_equal(length(out$exercises), 4)
  
  # first exercise
  ex1 <- out$exercises[[1]]
  expect_equal(ex1$type, "NormalExercise")
  expect_equal(ex1$xp, "100")
  expect_equal(ex1$skills, list("1", "3"))
  expect_equal(ex1$lang, "r")
  expect_equal(ex1$title, "Test")
  expect_equal(ex1$number, 1)
  expect_equal(ex1$assignment, "<p>Assignment</p>\n")
  expect_equal(ex1$instructions, "<ul>\n<li>instruction 1</li>\n<li>instruction 2</li>\n</ul>\n")
  expect_equal(ex1$hint, "<p>just a hint</p>\n")
  expect_equal(ex1$pre_exercise_code, "x <- 5")
  expect_equal(ex1$sample_code, "y <- 3")
  expect_equal(ex1$solution, "y <- 6")
  expect_equal(ex1$sct, "success_msg(\"OK\")")
  
  # second exercise
  ex2 <- out$exercises[[2]]
  expect_equal(ex2$type, "MultipleChoiceExercise")
  expect_equal(ex2$xp, "50")
  expect_equal(ex2$skills, list("1"))
  expect_equal(ex2$lang, "r")
  expect_equal(ex2$title, "MCE")
  expect_equal(ex2$number, 2)
  expect_equal(ex2$assignment, "<p>Assignment</p>\n")
  expect_equal(ex2$instructions, c("option 1", "option 2"))
  expect_equal(ex2$hint, "<p>just a hint</p>\n")
  expect_equal(ex2$pre_exercise_code, "x <- 5")
  expect_equal(ex2$sct, "test_mc(1)")
  
  # third exercise
  ex3 <- out$exercises[[3]]
  expect_equal(ex3$type, "VideoExercise")
  expect_equal(ex3$xp, "50")
  expect_equal(ex3$skills, list("2"))
  expect_equal(ex3$lang, "python")
  expect_equal(ex3$title, "Video")
  expect_equal(ex3$number, 3)
  expect_equal(ex3$assignment, "<p>Assignment</p>\n")
  expect_equal(ex3$video_link, "this_is_the_link")
  expect_equal(ex3$video_stream, "this_is_another_link")
  
  # fifth exercise
  ex4 <- out$exercises[[4]]
  expect_equal(ex4$type, "MarkdownExercise")
  expect_equal(ex4$xp, "100")
  expect_equal(ex4$skills, list("1"))
  expect_equal(ex4$lang, "r")
  expect_equal(ex4$title, "Markdown")
  expect_equal(ex4$number, 4)
  expect_equal(ex4$assignment, "<p>Assignment</p>\n")
  expect_equal(ex4$instructions, "<ul>\n<li>instruction 1</li>\n<li>instruction 2</li>\n</ul>\n")
  expect_equal(ex4$hint, "<p>just a hint</p>\n")
  expect_equal(ex4$pre_exercise_code, "x <- 5")
  expect_equal(ex4$sample_code, RJSONIO::toJSON(c(my_doc.Rmd = "some_code_here")))
  expect_equal(ex4$solution, RJSONIO::toJSON(c(solution.Rmd = "some_code_here")))
  expect_equal(ex4$sct, "success_msg(\"OK\")")
})
                            