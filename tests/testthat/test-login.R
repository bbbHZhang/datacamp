context("login")

test_that("login related functions work as expected", {
  suppressWarnings(rm(.DATACAMP_ENV, envir = globalenv()))
  expect_error(datacamp_login("datacamp_package@datacamp.com","wrongpass",""))
  expect_false(datacamp_logged_in())
  expect_message(datacamp_login("datacamp_package@datacamp.com","datacamp",""))
  expect_true(datacamp_logged_in())
  datacamp$clear()
})

