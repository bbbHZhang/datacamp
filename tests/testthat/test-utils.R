context("utils")

test_that("doc_url works as expected", {
  expect_that(doc_url(), equals("http://teach.datacamp.com"))
})

test_that("is_rmd works as expected", {
  expect_that(is_rmd("test.rmd"), is_true())
  expect_that(is_rmd("test.Rmd"), is_true())
  expect_that(is_rmd("test.RMd"), is_true())
  expect_that(is_rmd("test.RMD"), is_true())
  expect_that(is_rmd("no.yaml"), is_false())
})

test_that("clean_leftovers works as expected", {
  write("testmd", file = "test.md")
  write("testhtml", file = "test.html")
  dir.create("libraries")
  write("testfile", file = "libraries/testfile.txt")
  expect_that(clean_leftovers("test.rmd"), equals(0))
  expect_that(any(dir() == "test.md"), is_false())
  expect_that(any(dir() == "test.html"), is_false())
  expect_that(any(dir() == "libraries"), is_false())
})



