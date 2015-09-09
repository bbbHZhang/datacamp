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

# test_that("parse_exercise works as expected", {
#   x <- paste0(" type:SwirlExercise\n## Basic Building Blocks\n\n*** =swirl_course\n",
#               "```{r,eval=FALSE}\nR Programming\n```\n\n*** =swirl_lesson\n```",
#               "{r,eval=FALSE}\nBasic Building Blocks\n```")
#   xout <- parse_exercise(x)
#   expect_that(xout$type, equals("SwirlExercise"))
#   expect_that(xout$content, equals("## Basic Building Blocks\n"))
#   expect_that(xout$swirl_course$content, equals("```{r,eval=FALSE}\nR Programming\n```\n"))
#   expect_that(xout$swirl_lesson$content, equals("```{r,eval=FALSE}\nBasic Building Blocks\n```"))
#   # TODO add more tests
# })

test_that("parse_chapter works as expected", {
  path <- file.path(system.file(package = "datacamp"),"skeleton","chapter1.Rmd")
  out <- parse_chapter(path)
  expect_true(is.list(out))
  expect_true(is.list(out$exercises))
  # TODO add more tests
})
