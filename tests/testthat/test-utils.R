context("utils")

test_that("split_lines behaves as expected", {
  s1 <- "a\nb\nc\nd\n"
  expect_that(length(split_lines(s1)), equals(5))
  s2 <- "\n\n\n\n\n"
  expect_that(length(split_lines(s2)), equals(6))
  s3 <- "nonewlinesinhere"
  expect_that(split_lines(s3), equals(s3))
})

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

test_that("fix_specials is working as expected", {
  s1 <- "_tbt_"
  s2 <- "_tast_"
  s3 <- "_tbt_\n_tast_"
  s4 <- "nothinginhere"
  expect_that(fix_specials(s1), equals("```"))
  expect_that(fix_specials(s2), equals("***"))
  expect_that(fix_specials(s3), equals("```\n***"))
  expect_that(fix_specials(s4), equals(s4))
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