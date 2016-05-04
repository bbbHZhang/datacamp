context("utils")

test_that("parse_elements works as expected", {
  x <- parse_elements(" type:VideoExercise aspect_ratio:62.5\n## Video Exercise Title\n")
  expect_equal(x, list(type = "VideoExercise", aspect_ratio = "62.5", content = "## Video Exercise Title\n"))
  y <- parse_elements(" =pre_exercise_code\n```{r eval=FALSE}\n```\n")
  expect_equal(y, list(name = "pre_exercise_code", content = "```{r eval=FALSE}\n```\n"))
})

test_that("datacamp object works as expected", {
  datacamp$set(test = 1)
  expect_equal(datacamp$get("test"), 1)
  datacamp$set(test2 = 2)
  expect_equal(datacamp$get("test2"), 2)
  expect_equal(length(datacamp$get()), 2)
  datacamp$clear()
  expect_equal(datacamp$get(), list())
})