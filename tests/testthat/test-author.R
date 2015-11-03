context("author")

test_that("author_course works as expected", {
  expect_that(author_course(), throws_error())
  expect_that(author_course(lang = "r"), throws_error())
  
  crs_file <- author_course(lang = "r", simplified = TRUE)
  
  expect_true(file.exists(crs_file))
  crs <- load_course_file()
  unlink(crs_file)
  expect_equal(crs$title, "insert course title here")
  expect_equal(crs$author_field, "insert author name here")
  expect_equal(crs$description, "insert course description here")
  
  chapter_file <- "chapter1.Rmd"
  expect_true(file.exists(chapter_file))
  out <- parse_chapter(chapter_file)
  unlink(chapter_file)
  expect_equal(length(out$exercises), 3)
})

test_that("author_chapter works as expected", {
  expect_that(author_chapter(), throws_error())
  expect_that(author_chapter(lang = "r"), throws_error())
  chapter_file <- author_chapter(lang = "r", simplified = FALSE)
  expect_true(file.exists(chapter_file))
  out <- parse_chapter(chapter_file)
  unlink(chapter_file)
  expect_equal(length(out$exercises), 3)
  expect_equal(out$title_meta, "Chapter 1")
  expect_equal(out$title, "Insert the chapter title here")
  expect_equal(out$description, "Insert the chapter description here")
  
  
  chapter_file <- author_chapter(lang = "r", simplified = FALSE, title = "test", description = "description")
  expect_true(file.exists(chapter_file))
  out <- parse_chapter(chapter_file)
  unlink(chapter_file)
  expect_equal(length(out$exercises), 3)
  expect_equal(out$title_meta, "Chapter 1")
  expect_equal(out$title, "test")
  expect_equal(out$description, "description")
})
  
test_that("add_exercise works as expected", {
  chapter_file <- author_chapter(lang = "r", internal = TRUE)
  add_exercise(chapter_file, lang = "r", simplified = FALSE, type = "NormalExercise", title = "Test", content = "Assignment")
  add_exercise(chapter_file, lang = "python", simplified = FALSE, type = "NormalExercise", title = "Test 2", content = "Assignment")
  add_exercise(chapter_file, lang = "r", simplified = TRUE, type = "NormalExercise", title = "Test 3", content = "Assignment")
  add_exercise(chapter_file, lang = "python", simplified = TRUE, type = "NormalExercise", title = "Test 4", content = "Assignment")
  out <- parse_chapter(chapter_file)
  unlink(chapter_file)
  expect_equal(length(out$exercises), 4)
  expect_equal(out$exercises[[1]]$title, "Test")
  expect_equal(out$exercises[[1]]$type, "NormalExercise")
  expect_equal(out$exercises[[1]]$lang, "r")
  expect_equal(out$exercises[[1]]$assignment, "<p>Assignment</p>\n")
  expect_equal(out$exercises[[1]]$sample_code, "# sample code comes here")
  
  expect_equal(out$exercises[[2]]$title, "Test 2")
  expect_equal(out$exercises[[2]]$type, "NormalExercise")
  expect_equal(out$exercises[[2]]$lang, "python")
  expect_equal(out$exercises[[2]]$assignment, "<p>Assignment</p>\n")
  expect_equal(out$exercises[[2]]$sample_code, "# sample code comes here")
  
  expect_equal(out$exercises[[3]]$title, "Test 3")
  expect_equal(out$exercises[[3]]$type, "NormalExercise")
  expect_equal(out$exercises[[3]]$lang, "r")
  expect_equal(out$exercises[[3]]$assignment, "<p>Assignment</p>\n")
  expect_equal(out$exercises[[3]]$sample_code, NULL)
  
  expect_equal(out$exercises[[4]]$title, "Test 4")
  expect_equal(out$exercises[[4]]$type, "NormalExercise")
  expect_equal(out$exercises[[4]]$lang, "python")
  expect_equal(out$exercises[[4]]$assignment, "<p>Assignment</p>\n")
  expect_equal(out$exercises[[4]]$sample_code, NULL)
  
  chapter_file <- author_chapter(lang = "r", internal = TRUE)
  add_exercise(chapter_file, lang = "r", simplified = FALSE, type = "MultipleChoiceExercise", title = "Test", content = "Assignment")
  add_exercise(chapter_file, lang = "python", simplified = FALSE, type = "MultipleChoiceExercise", title = "Test 2", content = "Assignment")
  add_exercise(chapter_file, lang = "r", simplified = FALSE, type = "VideoExercise", title = "Test 3", content = "Assignment")
  add_exercise(chapter_file, lang = "python", simplified = FALSE, type = "VideoExercise", title = "Test 4", content = "Assignment")
  
  out <- parse_chapter(chapter_file)
  unlink(chapter_file)
  expect_equal(length(out$exercises), 4)
  expect_equal(out$exercises[[1]]$title, "Test")
  expect_equal(out$exercises[[1]]$type, "MultipleChoiceExercise")
  expect_equal(out$exercises[[1]]$lang, "r")
  expect_equal(out$exercises[[1]]$assignment, "<p>Assignment</p>\n")
  expect_equal(out$exercises[[1]]$instructions, c("option 1", "option 2", "option 3"))
  
  expect_equal(out$exercises[[2]]$title, "Test 2")
  expect_equal(out$exercises[[2]]$type, "MultipleChoiceExercise")
  expect_equal(out$exercises[[2]]$lang, "python")
  expect_equal(out$exercises[[2]]$assignment, "<p>Assignment</p>\n")
  expect_equal(out$exercises[[1]]$instructions, c("option 1", "option 2", "option 3"))
  
  expect_equal(out$exercises[[3]]$title, "Test 3")
  expect_equal(out$exercises[[3]]$type, "VideoExercise")
  expect_equal(out$exercises[[3]]$lang, "r")
  expect_equal(out$exercises[[3]]$video_link, "//player.vimeo.com/video/108225030")
  
  expect_equal(out$exercises[[4]]$title, "Test 4")
  expect_equal(out$exercises[[4]]$type, "VideoExercise")
  expect_equal(out$exercises[[4]]$lang, "python")
  expect_equal(out$exercises[[4]]$video_link, "//player.vimeo.com/video/108225030")
})

test_that("build_scaffold works as expected", {
  # TO DO
})