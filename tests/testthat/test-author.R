context("author")

test_that("author_course works as expected", {
  expect_error(author_course())
  
  crs_file <- author_course(lang = "r")
  
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
  
  crs_file <- author_course(lang = "r")
  expect_error(author_course(lang = "r"), "A file named course.yml already exists in your current working directory.")
  unlink(crs_file)
  unlink("chapter1.Rmd")
})

test_that("author_chapter works as expected", {
  expect_error(author_chapter())
  chapter_file <- author_chapter(lang = "r")
  expect_true(file.exists(chapter_file))
  out <- parse_chapter(chapter_file)
  unlink(chapter_file)
  expect_equal(length(out$exercises), 3)
  expect_equal(out$title, "Insert the chapter title here")
  expect_equal(out$description, "Insert the chapter description here")
  
  chapter_file <- author_chapter(lang = "r", title = "test", description = "description")
  expect_true(file.exists(chapter_file))
  out <- parse_chapter(chapter_file)
  unlink(chapter_file)
  expect_equal(length(out$exercises), 3)
  expect_equal(out$title, "test")
  expect_equal(out$description, "description")
})
  
test_that("add_exercise works as expected", {
  
  # NormalExercise
  chapter_file <- author_chapter(lang = "r", internal = TRUE)
  add_exercise(chapter_file, lang = "r", type = "NormalExercise", title = "Test", content = "Assignment")
  add_exercise(chapter_file, lang = "python", type = "NormalExercise", title = "Test 2", content = "Assignment")
  
  out <- parse_chapter(chapter_file)
  unlink(chapter_file)
  expect_equal(length(out$exercises), 2)
  expect_equal(out$exercises[[1]]$title, "Test")
  expect_equal(out$exercises[[1]]$type, "NormalExercise")
  expect_equal(out$exercises[[1]]$lang, "r")
  expect_equal(out$exercises[[1]]$assignment, "<p>Assignment</p>\n")
  expect_equal(out$exercises[[1]]$sample_code, "# sample code")
  expect_equal(out$exercises[[1]]$solution, "# solution code")
  
  expect_equal(out$exercises[[2]]$title, "Test 2")
  expect_equal(out$exercises[[2]]$type, "NormalExercise")
  expect_equal(out$exercises[[2]]$lang, "python")
  expect_equal(out$exercises[[2]]$assignment, "<p>Assignment</p>\n")
  expect_equal(out$exercises[[2]]$sample_code, "# sample code")
  expect_equal(out$exercises[[2]]$solution, "# solution code")
  
  # MultipleChoiceExercise
  chapter_file <- author_chapter(lang = "r", internal = TRUE)
  add_exercise(chapter_file, lang = "r", type = "MultipleChoiceExercise", title = "Test", content = "Assignment")
  add_exercise(chapter_file, lang = "python", type = "MultipleChoiceExercise", title = "Test 2", content = "Assignment")
  
  out <- parse_chapter(chapter_file)
  unlink(chapter_file)
  expect_equal(length(out$exercises), 2)
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
  
  # VideoExercise
  chapter_file <- author_chapter(lang = "r", internal = TRUE)
  add_exercise(chapter_file, lang = "r", type = "VideoExercise", title = "Test", content = "Assignment")
  add_exercise(chapter_file, lang = "python", type = "VideoExercise", title = "Test 2", content = "Assignment")
  
  out <- parse_chapter(chapter_file)
  unlink(chapter_file)
  expect_equal(length(out$exercises), 2)
  expect_equal(out$exercises[[1]]$title, "Test")
  expect_equal(out$exercises[[1]]$type, "VideoExercise")
  expect_equal(out$exercises[[1]]$lang, "r")
  expect_equal(out$exercises[[1]]$video_link, "//player.vimeo.com/video/108225030")
  
  expect_equal(out$exercises[[2]]$title, "Test 2")
  expect_equal(out$exercises[[2]]$type, "VideoExercise")
  expect_equal(out$exercises[[2]]$lang, "python")
  expect_equal(out$exercises[[2]]$video_link, "//player.vimeo.com/video/108225030")
})

test_that("build_scaffold works as expected", {
  write(paste0("- chapter_title: Chapter 1\n",
               "  chapter_description: This is my chapter\n",
               "  exercises:\n",
               "    - type: NormalExercise\n",
               "      title: normal\n",
               "      content: normal content\n",
               "    - type: MultipleChoiceExercise\n",
               "      title: mce\n",
               "      content: mce content\n",
               "    - type: VideoExercise\n",
               "      title: video\n",
               "      content: video content\n",
               "- chapter_title: Chapter 2\n",
               "  chapter_description: This is another chapter\n",
               "  exercises:\n",
               "    - type: NormalExercise\n",
               "      title: normal 2\n",
               "      content: normal content 2\n",
               "    - type: MultipleChoiceExercise\n",
               "      title: mce 2\n",
               "      content: mce content 2\n",
               "    - type: VideoExercise\n",
               "      title: video 2\n",
               "      content: video content 2\n"),
        file = index_yaml)
  build_scaffold(index_yaml, lang = "r")
  crs <- load_course_file()
  expect_equal(crs$title, "insert course title here")
  out1 <- parse_chapter("chapter1.Rmd")
  unlink("chapter1.Rmd")
  expect_equal(length(out1$exercises), 3)
  expect_equal(out1$exercises[[1]]$type, "NormalExercise")
  expect_equal(out1$exercises[[1]]$title, "normal")
  expect_equal(out1$exercises[[1]]$assignment, "<p>normal content</p>\n")
  expect_equal(out1$exercises[[2]]$type, "MultipleChoiceExercise")
  expect_equal(out1$exercises[[2]]$title, "mce")
  expect_equal(out1$exercises[[2]]$assignment, "<p>mce content</p>\n")
  expect_equal(out1$exercises[[3]]$type, "VideoExercise")
  expect_equal(out1$exercises[[3]]$title, "video")
  expect_equal(out1$exercises[[3]]$assignment, "<p>video content</p>\n")
  
  out2 <- parse_chapter("chapter2.Rmd")
  unlink("chapter2.Rmd")
  expect_equal(out2$exercises[[1]]$type, "NormalExercise")
  expect_equal(out2$exercises[[1]]$title, "normal 2")
  expect_equal(out2$exercises[[1]]$assignment, "<p>normal content 2</p>\n")
  expect_equal(out2$exercises[[2]]$type, "MultipleChoiceExercise")
  expect_equal(out2$exercises[[2]]$title, "mce 2")
  expect_equal(out2$exercises[[2]]$assignment, "<p>mce content 2</p>\n")
  expect_equal(out2$exercises[[3]]$type, "VideoExercise")
  expect_equal(out2$exercises[[3]]$title, "video 2")
  expect_equal(out2$exercises[[3]]$assignment, "<p>video content 2</p>\n")
  
  build_scaffold(index_yaml, lang = "r")
  build_scaffold(index_yaml, lang = "r")
  archived <- grepl("archived_*", dir())
  expect_true(any(archived))
  unlink(dir()[archived], recursive = TRUE)
  unlink("chapter1.Rmd")
  unlink("chpater2.Rmd")
  
  unlink(index_yaml)
})