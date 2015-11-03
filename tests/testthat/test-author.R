context("author")

test_that("author functions work as expected", {
  expect_that(author_course(), throws_error())
  expect_that(author_course(lang = "r"), throws_error())
  crs_file <- author_course(lang = "r", simplified = TRUE)
  expect_true(file.exists(crs_file))
  chapter_file <- "chapter1.Rmd"
  expect_true(file.exists(chapter_file))
  out <- parse_chapter(chapter_file)
  expect_equal(length(out$exercises), 3)
  unlink(crs_file)
  unlink(chapter_file)
  
  expect_that(author_chapter(), throws_error())
  expect_that(author_chapter(lang = "r"), throws_error())
  chapter_file <- author_chapter(lang = "r", simplified = FALSE)
  expect_true(file.exists(chapter_file))
  out <- parse_chapter(chapter_file)
  expect_equal(length(out$exercises), 3)
  expect_equal(out$title_meta, "Chapter 1")
  expect_equal(out$title, "Insert the chapter title here")
  expect_equal(out$description, "Insert the chapter description here")
  unlink(chapter_file)
  
  chapter_file <- author_chapter(lang = "r", simplified = FALSE, title = "test", description = "description")
  expect_true(file.exists(chapter_file))
  out <- parse_chapter(chapter_file)
  expect_equal(length(out$exercises), 3)
  expect_equal(out$title_meta, "Chapter 1")
  expect_equal(out$title, "test")
  expect_equal(out$description, "description")
  unlink(chapter_file)
  
  # TO DO ADD TESTS FOR ADD EXERCISE AND BUILD EXERCISE TEMPLATE
})