context("author")

test_that("author_course works as expected", {
  expect_that(author_course(), throws_error())
  expect_that(author_course("test_course", open = FALSE), shows_message())
  expect_that(tail(strsplit(getwd(), "/")[[1]],1), equals("test_course"))
  expect_that(any(dir() == "course.yml"), is_true())
  expect_that(any(dir() == "chapter1.md"), is_true())
  setwd("..")
  unlink("test_course", recursive = TRUE)
})