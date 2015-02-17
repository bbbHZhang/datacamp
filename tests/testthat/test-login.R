context("login")

test_that("login related functions work as expected", {
  suppressWarnings(rm(.DATACAMP_ENV, envir = globalenv()))
  expect_that(datacamp_login("datacamp_package@datacamp.com","wrongpass",""), throws_error())
  expect_that(datacamp_logged_in(), is_false())
  expect_that(datacamp_login("datacamp_package@datacamp.com","datacamp",""), shows_message())
  expect_that(datacamp_logged_in(), is_true())
  suppressWarnings(rm(.DATACAMP_ENV, envir = globalenv()))
})

