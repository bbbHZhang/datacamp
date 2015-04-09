context("extract")

test_that("extract_title works as expected", {
  s1in = "## My Title\n"
  s1out = "My Title"
  s2in = "\n## My Title\n"
  s2out = "My Title"
  s3in = "\n## My Title\nSomeMoreText"
  s3out = "My Title"
  expect_that(extract_title(s1in), equals(s1out))
  expect_that(extract_title(s2in), equals(s2out))
  expect_that(extract_title(s3in), equals(s3out))
  expect_that(extract_title("Nothing"), throws_error())
  expect_that(extract_title("##WrongFormat"), throws_error())
  expect_that(extract_title("## T1\n## T2"), throws_error())
})

test_that("extract_html works as expected", {
  s1in = "Just some text"
  s1out = "<p>Just some text</p>\n"
  s2in = "\n## Title\nAnd some text"
  s2out = "<p>And some text</p>\n"
  s3in = "\n## Title\n\n\nLonely Text\n\n\n"
  s3out = "<p>Lonely Text</p>\n"
  expect_that(extract_html(s1in), equals(s1out))
  expect_that(extract_html(s2in), equals(s2out))
  expect_that(extract_html(s3in), equals(s3out))
})

test_that("extract_code works as expected", {
  s1in = "```{r}\nsome_code_here\n```\n"
  s1out = "some_code_here"
  s2in = "```{r}\n```\n"
  s2out = ""
  s3in = "```{r}\nsome_code_here\n```\n\n```{r}\nmore_code_here\n```\n"
  s3out = c("some_code_here","more_code_here")
  s4in = "```{r}\nsome_string<-\"abc\"\n```\n"
  s4out = "some_string<-\"abc\""
  expect_that(extract_code(s1in), equals(s1out))
  expect_that(extract_code(s2in), equals(s2out))
  expect_that(extract_code(s3in), equals(s3out))
  expect_that(extract_code(s4in), equals(s4out))
})

test_that("extract_mc works as expected", {
  s1in = "\n- a\n- b\n- c\n- d"
  s1out = c("a","b","c","d")
  s2in = "\n-blereasdfasdf\n-asdfasdf\n"
  expect_that(extract_mc(s1in), equals(s1out))
  expect_that(extract_mc(s2in), throws_error())
})


test_that("extract_markdown works as expected", {
  s1in = "```{r}\nsome_code_here\n```\n"
  expect_that(extract_markdown(s1in,"testname"), equals(toJSON(c(testname = "some_code_here"))))
  s2in = "```{r}\n{{{my_doc.Rmd}}}\nsome_code_here\n```\n"
  expect_that(extract_markdown(s2in,"testname"), equals(toJSON(c(my_doc.Rmd = "some_code_here"))))
  s3in = "```{r}\n{{{my_doc.Rmd}}}\nsome_code_here\n```\n\n```{r}\n{{{my_doc_2.Rmd}}}\nmore_code_here\n```\n"
  expect_that(extract_markdown(s3in,"testname"), equals(toJSON(c(my_doc.Rmd = "some_code_here", my_doc_2.Rmd = "more_code_here"))))
  s4in = "```{r}\n{{{my_doc.Rmd}}}\nsome_code_here_tbt_\n```\n\n```{r}\n{{{my_doc_2.Rmd}}}\nmore_code_here_tast_\n```\n"
  expect_that(extract_markdown(s4in,"testname"), equals(toJSON(c(my_doc.Rmd = "some_code_here```", my_doc_2.Rmd = "more_code_here***"))))
})

test_that("extract_named_list works as expected", {
  s1in = "## Title 1\nText 1\n"
  s1out = list(list(title = "Title 1", content = "\n<p>Text 1</p>\n"))
  expect_that(extract_named_list(s1in), equals(s1out))
  s2in = "## Title 1\nText 1\n## Title 2\nText 2\n"
  s2out = list(list(title = "Title 1", content = "\n<p>Text 1</p>\n"), list(title = "Title 2", content = "\n<p>Text 2</p>\n"))
  expect_that(extract_named_list(s2in), equals(s2out))
  s3in = "## Title 1\nText 1\n\nText 1b\n## Title 2\nText 2\n"
  s3out = list(list(title = "Title 1", content = "\n<p>Text 1</p>\n\n<p>Text 1b</p>\n"), list(title = "Title 2", content = "\n<p>Text 2</p>\n"))
  expect_that(extract_named_list(s3in), equals(s3out))
})