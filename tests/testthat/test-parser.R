context("parser")

test_that("parse_meta works as expected", {
  expect_that(length(parse_meta("")), equals(0))
  expect_that(parse_meta(" =instructions"), equals(list(name = "instructions")))
  expect_that(parse_meta(" =hint"), equals(list(name = "hint")))
  expect_that(parse_meta(" type:VideoExercise"), equals(list(type = "VideoExercise")))
  expect_that(parse_meta(" type:VideoExercise aspect_ratio:62.5"), equals(list(type = "VideoExercise", aspect_ratio = "62.5")))
})

test_that("split_meta works as expected", {
  x <- split_meta(" type:VideoExercise aspect_ratio:62.5\n## Video Exercise Title\n")
  expect_that(x[1], equals(" type:VideoExercise aspect_ratio:62.5"))
  expect_that(x[2], equals("## Video Exercise Title\n"))
  y <- split_meta(" =pre_exercise_code\n```{r,eval=FALSE}\nurl <- \"http://assets.datacamp.com/blog_assets/chol.txt\"\n```\n")
  expect_that(y[1], equals(" =pre_exercise_code"))
  expect_that(y[2], equals("```{r,eval=FALSE}\nurl <- \"http://assets.datacamp.com/blog_assets/chol.txt\"\n```\n"))
})

test_that("parse_elements works as expected", {
  x <- parse_elements(" type:VideoExercise aspect_ratio:62.5\n## Video Exercise Title\n")
  expect_that(x, equals(list(type = "VideoExercise", aspect_ratio = "62.5", content = "## Video Exercise Title\n")))
  y <- parse_elements(" =pre_exercise_code\n```{r eval=FALSE}\n```\n")
  expect_that(y, equals(list(name = "pre_exercise_code", content = "```{r eval=FALSE}\n```\n")))
})


test_that("parse_chapter works as expected", {
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
  write(paste0("--- type:ChallengeExercise xp:200 skills:1,3,5 lang:r\n## Challenge\nAssignment\n",
               "*** =challenge_steps\n## Step 1\nthis is step 1\n## Step 2\nthis is step 2\n",
               "*** =challenge_goal\n## Goal\nthis is the goal\n",
               "*** =pre_exercise_code\n```{r}\nx <- 5\n```\n",
               "*** =solution\n```{r}\ny <- 6\n```\n",
               "*** =sct\n```{r}\nsuccess_msg(\"OK\")\n```\n")
        , chapter_file, append = TRUE)
  write(paste0("--- type:MarkdownExercise xp:100 skills:1 lang:r\n## Markdown\nAssignment\n",
               "*** =instructions\n- instruction 1\n- instruction 2\n\n",
               "*** =hint\njust a hint\n\n",
               "*** =pre_exercise_code\n```{r}\nx <- 5\n```\n",
               "*** =sample_code\n```{r}\n{{{my_doc.Rmd}}}\nsome_code_here\n```\n",
               "*** =solution\n```{r}\nsome_code_here\n```\n",
               "*** =sct\n```{r}\nsuccess_msg(\"OK\")\n```\n")
        , chapter_file, append = TRUE)
  
  
  out <- parse_chapter(chapter_file)
  unlink(chapter_file)
  
  expect_equal(out$title_meta, "Chapter 1")
  expect_equal(out$title, "Insert the chapter title here")
  expect_equal(out$description, "Insert the chapter description here")
  expect_equal(length(out$exercises), 5)
  
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
  
  # fourth exercise
  ex4 <- out$exercises[[4]]
  expect_equal(ex4$type, "ChallengeExercise")
  expect_equal(ex4$xp, "200")
  expect_equal(ex4$skills, list("1", "3", "5"))
  expect_equal(ex4$lang, "r")
  expect_equal(ex4$title, "Challenge")
  expect_equal(ex4$number, 4)
  expect_equal(ex4$assignment, "<p>Assignment</p>\n")
  expect_equal(ex4$challenge_steps, list(list(title = "Step 1", content = "\n<p>this is step 1</p>\n"),
                                         list(title = "Step 2", content = "\n<p>this is step 2</p>\n")))
  expect_equal(ex4$challenge_goal, list(list(title = "Goal", content = "\n<p>this is the goal</p>\n")))
  expect_equal(ex4$pre_exercise_code, "x <- 5")
  expect_equal(ex4$solution, "y <- 6")
  expect_equal(ex4$sct, "success_msg(\"OK\")")
  
  # fifth exercise
  ex5 <- out$exercises[[5]]
  expect_equal(ex5$type, "MarkdownExercise")
  expect_equal(ex5$xp, "100")
  expect_equal(ex5$skills, list("1"))
  expect_equal(ex5$lang, "r")
  expect_equal(ex5$title, "Markdown")
  expect_equal(ex5$number, 5)
  expect_equal(ex5$assignment, "<p>Assignment</p>\n")
  expect_equal(ex5$instructions, "<ul>\n<li>instruction 1</li>\n<li>instruction 2</li>\n</ul>\n")
  expect_equal(ex5$hint, "<p>just a hint</p>\n")
  expect_equal(ex5$pre_exercise_code, "x <- 5")
  expect_equal(ex5$sample_code, RJSONIO::toJSON(c(my_doc.Rmd = "some_code_here")))
  expect_equal(ex5$solution, RJSONIO::toJSON(c(solution.Rmd = "some_code_here")))
  expect_equal(ex5$sct, "success_msg(\"OK\")")
})
                            