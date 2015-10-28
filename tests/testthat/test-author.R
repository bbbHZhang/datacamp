# context("author")
# 
# test_that("author functions work as expected", {
#   expectation_wrapper <- function(...) {
#     chapter_file <- author_chapter(...)
#     parsed <- parse_chapter(chapter_file)
#     expect_true(is.list(parsed))
#     unlink(chapter_file)  
#   }
#   
#   expectation_wrapper(lang = "r", simplified = FALSE, open = FALSE)
#   expectation_wrapper(lang = "r", simplified = TRUE, open = FALSE)
#   expectation_wrapper(lang = "python", simplified = FALSE, open = FALSE)
#   expectation_wrapper(lang = "python", simplified = TRUE, open = FALSE)
# })