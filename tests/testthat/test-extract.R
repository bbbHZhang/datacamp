context("extract")

test_that("extract_title works as expected", {
  s1in = "## My Title\n"
  s1out = "My Title"
  s2in = "\n## My Title\n"
  s2out = "My Title"
  s3in = "\n## My Title\nSomeMoreText"
  s3out = "My Title"
  expect_equal(extract_title(s1in), s1out)
  expect_equal(extract_title(s2in), s2out)
  expect_equal(extract_title(s3in), s3out)
  expect_error(extract_title("Nothing"))
  expect_error(extract_title("##WrongFormat"))
  expect_error(extract_title("## T1\n## T2"))
})

test_that("extract_html works as expected", {
  s1in = "Just some text"
  s1out = "<p>Just some text</p>\n"
  s2in = "\n## Title\nAnd some text"
  s2out = "<p>And some text</p>\n"
  s3in = "\n## Title\n\n\nLonely Text\n\n\n"
  s3out = "<p>Lonely Text</p>\n"
  expect_equal(extract_html(s1in), s1out)
  expect_equal(extract_html(s2in), s2out)
  expect_equal(extract_html(s3in), s3out)
})

test_that("extract_code works as expected", {
  s1in = "```{r}\nsome_code_here\n```\n"
  s1out = "some_code_here"
  s2in = "```{r}\n```\n"
  s2out = ""
  s3in = "```{r}\nsome_code_here\n```\n\n```{r}\nmore_code_here\n```\n"
  s3out = c("some_code_here","more_code_here")
  s4in <- "```{r}\nsome_string<-\"abc\"\n```\n"
  s4out <- "some_string<-\"abc\""
  expect_equal(extract_code(s1in), s1out)
  expect_equal(extract_code(s2in), s2out)
  expect_equal(extract_code(s3in), s3out)
  expect_equal(extract_code(s4in), s4out)
  
  s1in <- "```{python}\nsome_code_here\n```\n"
  s1out <- "some_code_here"
  s2in <- "```{py}\n```\n"
  s2out <- ""
  s3in <- "```{python}\nsome_code_here\n```\n\n```{python}\nmore_code_here\n```\n"
  s3out <- c("some_code_here","more_code_here")
  s4in <- "```{python}\nsome_string<-\"abc\"\n```\n"
  s4out <- "some_string<-\"abc\""
  expect_equal(extract_code(s1in), s1out)
  expect_equal(extract_code(s2in), s2out)
  expect_equal(extract_code(s3in), s3out)
  expect_equal(extract_code(s4in), s4out)
  
})

test_that("extract_as_* works as expected", {
  s1in <- "\n- a\n- b\n- c\n- d"
  s1out <- c("a","b","c","d")
  s2in <- "\n- a\n- b\n   - sub-a\n   - sub-b\n- c"
  s2out <- c("a", "b\n\n<ul>\n<li>sub-a</li>\n<li>sub-b</li>\n</ul>", "c")
  s3in <- "\n-blereasdfasdf\n-asdfasdf\n"
  s4in <- ""
  s5in <- NULL
  
  expect_equal(extract_as_vec(s1in), s1out)
  expect_equal(extract_as_vec(s2in), s2out)
  expect_error(extract_as_vec(s3in))
  expect_equal(extract_as_vec(s4in), "empty")
  expect_equal(extract_as_vec(s5in), "empty")
  
  expect_equal(extract_as_list(s1in), as.list(s1out))
  expect_equal(extract_as_list(s2in), as.list(s2out))
})

test_that("extract_markdown works as expected", {
  s1in <- "```{r}\nsome_code_here\n```\n"
  expect_equal(extract_markdown(s1in,"testname"), toJSON(c(testname = "some_code_here")))
  s2in <- "```{r}\n{{{my_doc.Rmd}}}\nsome_code_here\n```\n"
  expect_equal(extract_markdown(s2in,"testname"), toJSON(c(my_doc.Rmd = "some_code_here")))
  s3in <- "```{r}\n{{{my_doc.Rmd}}}\nsome_code_here\n```\n\n```{r}\n{{{my_doc_2.Rmd}}}\nmore_code_here\n```\n"
  expect_equal(extract_markdown(s3in,"testname"), toJSON(c(my_doc.Rmd = "some_code_here", my_doc_2.Rmd = "more_code_here")))
  s4in <- "```{r}\n{{{my_doc.Rmd}}}\nsome_code_here_tbt_\n```\n\n```{r}\n{{{my_doc_2.Rmd}}}\nmore_code_here_tast_\n```\n"
  expect_equal(extract_markdown(s4in,"testname"), toJSON(c(my_doc.Rmd = "some_code_here```", my_doc_2.Rmd = "more_code_here***")))
})

test_that("extract_named_list works as expected", {
  s1in <- "## Title 1\nText 1\n"
  s1out <- list(list(title = "Title 1", content = "\n<p>Text 1</p>\n"))
  expect_equal(extract_named_list(s1in), s1out)
  s2in <- "## Title 1\nText 1\n## Title 2\nText 2\n"
  s2out <- list(list(title = "Title 1", content = "\n<p>Text 1</p>\n"), list(title = "Title 2", content = "\n<p>Text 2</p>\n"))
  expect_equal(extract_named_list(s2in), s2out)
  s3in <- "## Title 1\nText 1\n\nText 1b\n## Title 2\nText 2\n"
  s3out <- list(list(title = "Title 1", content = "\n<p>Text 1</p>\n\n<p>Text 1b</p>\n"), list(title = "Title 2", content = "\n<p>Text 2</p>\n"))
  expect_equal(extract_named_list(s3in), s3out)
})

test_that("extract_skills works as expected", {
  s1in <- "1,2,3"
  s1out <- list("1","2","3")
  s2in <- "1,2,3"
  s2out <- s1out
  s3in <- list(content = "1,2")
  s3out <- list("1","2")
  s4in <- list(content = "1")
  s4out <- list("1")
  expect_equal(extract_skills(s1in), s1out)
  expect_equal(extract_skills(s2in), s2out)
  expect_equal(extract_skills(s3in), s3out)
  expect_equal(extract_skills(s4in), s4out)
})

test_that("extract_video_link works as expected", {
  s1in <- "//player.vimeo.com/video/144351865"
  s2in <- "\n//player.vimeo.com/video/144351865\n\n"
  s3in <- "```{r,eval=FALSE}\n//player.vimeo.com/video/144351865\n```\n"
  s4in <- "\n\n```{r,eval=FALSE}\n//player.vimeo.com/video/144351865\n```\n"
  s5in <- "```{python,eval=FALSE}\n//player.vimeo.com/video/144351865\n```\n"
  s6in <- "\n\n```{python,eval=FALSE}\n//player.vimeo.com/video/144351865\n```\n"
  sout <- "//player.vimeo.com/video/144351865"
  expect_equal(extract_video_link(s1in), sout)
  expect_equal(extract_video_link(s2in), sout)
  expect_equal(extract_video_link(s3in), sout)
  expect_equal(extract_video_link(s4in), sout)
  expect_equal(extract_video_link(s5in), sout)
  expect_equal(extract_video_link(s6in), sout)
})