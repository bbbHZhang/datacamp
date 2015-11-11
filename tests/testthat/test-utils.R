context("utils")

test_that("split_lines behaves as expected", {
  s1 <- "a\nb\nc\nd\n"
  expect_equal(length(split_lines(s1)), 5)
  s2 <- "\n\n\n\n\n"
  expect_equal(length(split_lines(s2)), 6)
  s3 <- "nonewlinesinhere"
  expect_equal(split_lines(s3), s3)
})

test_that("parse_meta works as expected", {
  expect_equal(length(parse_meta("")), 0)
  expect_equal(parse_meta(" =instructions"), list(name = "instructions"))
  expect_equal(parse_meta(" =hint"), list(name = "hint"))
  expect_equal(parse_meta(" type:VideoExercise"), list(type = "VideoExercise"))
  expect_equal(parse_meta(" type:VideoExercise aspect_ratio:62.5"), list(type = "VideoExercise", aspect_ratio = "62.5"))
})

test_that("split_meta works as expected", {
  x <- split_meta(" type:VideoExercise aspect_ratio:62.5\n## Video Exercise Title\n")
  expect_equal(x[1], " type:VideoExercise aspect_ratio:62.5")
  expect_equal(x[2], "## Video Exercise Title\n")
  y <- split_meta(" =pre_exercise_code\n```{r,eval=FALSE}\nurl <- \"http://assets.datacamp.com/blog_assets/chol.txt\"\n```\n")
  expect_equal(y[1], " =pre_exercise_code")
  expect_equal(y[2], "```{r,eval=FALSE}\nurl <- \"http://assets.datacamp.com/blog_assets/chol.txt\"\n```\n")
})

test_that("parse_elements works as expected", {
  x <- parse_elements(" type:VideoExercise aspect_ratio:62.5\n## Video Exercise Title\n")
  expect_equal(x, list(type = "VideoExercise", aspect_ratio = "62.5", content = "## Video Exercise Title\n"))
  y <- parse_elements(" =pre_exercise_code\n```{r eval=FALSE}\n```\n")
  expect_equal(y, list(name = "pre_exercise_code", content = "```{r eval=FALSE}\n```\n"))
})

test_that("fix_specials is working as expected", {
  s1 <- "_tbt_"
  s2 <- "_tast_"
  s3 <- "_tbt_\n_tast_"
  s4 <- "nothinginhere"
  expect_equal(fix_specials(s1), "```")
  expect_equal(fix_specials(s2), "***")
  expect_equal(fix_specials(s3), "```\n***")
  expect_equal(fix_specials(s4), s4)
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