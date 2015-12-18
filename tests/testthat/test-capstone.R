context("capstone")

test_that("parse_chapter with capstone exercises works as expected", {
  chapter_file = "capstone.Rmd"
  write(paste0("---\ntitle_meta: Chapter 1\ntitle: Insert the chapter title here\n",
               "description: Insert the chapter description here\ncapstone : TRUE"), file = chapter_file)
  write(paste0("--- type:CapstoneNormalExercise xp:100 skills:1,3 lang:r id:intro nxt:multiple optimal:TRUE\n## Test\nAssignment\n",
               "*** =instructions\n- instruction 1\n- instruction 2\n\n",
               "*** =hint\njust a hint\n\n",
               "*** =pre_exercise_code\n```{r}\nx <- 5\n```\n",
               "*** =sample_code\n```{r}\ny <- 3\n```\n",
               "*** =solution\n```{r}\ny <- 6\n```\n",
               "*** =sct\n```{r}\nsuccess_msg(\"OK\")\n```\n")
        , chapter_file, append = TRUE)
  write(paste0("--- type:CapstoneMultipleChoiceExercise xp:50 skills:1 id:multiple optimal:FALSE\n## MCE\nAssignment\n",
               "*** =instructions\n- id=video: option1\n- id=video: option2\n\n",
               "*** =hint\njust a hint\n\n",
               "*** =video_link\nthis_is_the_link\n\n",
               "*** =pre_exercise_code\n```{r}\nx <- 5\n```\n",
               "*** =sct\n```{r}\ntest_mc(1)\n```\n")
        , chapter_file, append = TRUE)
  write(paste0("--- type:CapstoneVideoExercise xp:50 skills:2 lang:python id:video nxt:void optimal:FALSE\n## Video\nAssignment\n",
               "*** =video_link\nthis_is_the_link\n\n",
               "*** =video_stream\n```{python}\nthis_is_another_link\n```\n")
        , chapter_file, append = TRUE)
  
  out <- parse_chapter(chapter_file)
  unlink("capstone.Rmd")
  
  expect_equal(out$title_meta, "Chapter 1")
  expect_equal(out$title, "Insert the chapter title here")
  expect_equal(out$description, "Insert the chapter description here")
  expect_equal(length(out$exercises), 3)
  
  # first exercise
  ex1 <- out$exercises[[1]]
  expect_equal(ex1$type, "CapstoneNormalExercise")
  expect_equal(ex1$next_exercise_number, 2)
  expect_equal(ex1$optimal, TRUE)
  
  # second exercise
  ex2 <- out$exercises[[2]]
  expect_equal(ex2$type, "CapstoneMultipleChoiceExercise")
  expect_equal(ex2$instructions[[1]]$next_exercise_number, 3)
  expect_equal(ex2$instructions[[2]]$next_exercise_number, 3)
  expect_equal(ex2$video_link, "this_is_the_link")
  expect_equal(ex2$optimal, FALSE)
  
  # third exercise
  ex3 <- out$exercises[[3]]
  expect_equal(ex3$type, "CapstoneVideoExercise")
  expect_equal(ex3$next_exercise_number, 0)
  expect_equal(ex3$optimal, FALSE)
})
