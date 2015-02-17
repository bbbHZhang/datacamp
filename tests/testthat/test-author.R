context("author")

test_that("author_course works as expected", {
  expect_that(author_course(), throws_error())
  expect_that(author_course("test_course", open = FALSE), shows_message())
  expect_that(tail(strsplit(getwd(), "/")[[1]],1), equals("test_course"))
  expect_that(any(dir() == "course.yml"), is_true())
  expect_that(any(dir() == "chapter1.Rmd"), is_true())
  setwd("..")
  unlink("test_course", recursive = TRUE)
})

test_that("author_chapter works as expected", {
  expect_that(author_chapter(), throws_error())
  expect_that(author_chapter("test_chapter.Rmd", open = FALSE), shows_message())
  expect_that(readLines("test_chapter.Rmd")[1], equals("---"))
  write("testtesttest",file="test_chapter.Rmd")
  expect_that(author_chapter("test_chapter.Rmd", open = FALSE), shows_message())
  expect_that(readLines("test_chapter.Rmd"), equals("testtesttest"))
  expect_that(author_chapter("test_chapter1.Rmd", open = FALSE), shows_message())
  expect_that(readLines("test_chapter1.Rmd")[1], equals("---"))
  
  unlink("test_chapter.Rmd")
  unlink("test_chapter1.Rmd")
})