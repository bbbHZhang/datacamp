context("utils")

test_that("doc_url works as expected", {
  expect_that(doc_url, equals("http://teach.datacamp.com"))
})

test_that("is_rmd works as expected", {
  expect_that(is_rmd("test.rmd"), is_true())
  expect_that(is_rmd("test.Rmd"), is_true())
  expect_that(is_rmd("test.RMd"), is_true())
  expect_that(is_rmd("test.RMD"), is_true())
  expect_that(is_rmd("no.yaml"), is_false())
})

test_that("split_lines behaves as expected", {
  s1 <- "a\nb\nc\nd\n"
  expect_that(length(split_lines(s1)), equals(5))
  s2 <- "\n\n\n\n\n"
  expect_that(length(split_lines(s2)), equals(6))
  s3 <- "nonewlinesinhere"
  expect_that(split_lines(s3), equals(s3))
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