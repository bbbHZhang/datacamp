context("helpers")

test_that("extract_code works as expected", {
  html = "<pre><code class=\"r\"># The sample code is what is shown in the students editor by default\n</code></pre>\n"
  expect_that(extract_code(html), equals("# The sample code is what is shown in the students editor by default\n"))
  html = "<pre><code class=\"r\">x &lt;- 5\n\nsum &lt;- 1483\n</code></pre>\n"
  expect_that(extract_code(html), equals("x <- 5\n\nsum <- 1483\n"))
  
  # TO DO HANDLING OF DOUBLE QUOTES!!!
})

test_that("extract_markdown works as expected", {
  html = paste0("<pre><code class=\"r\">{{{my_document.Rmd}}}\n---\ntitle: &quot;Tryout&quot;\noutput: ",
               "html_document\n---\n\nThis sample R Markdown document contains a list:\n\n* A\n* B\n\nIt ",
               "also contains some code:\n\n_tbt_{r}\nx &lt;-\n_tbt_\n</code></pre>\n\n<pre><code ",
               "class=\"r\">{{{styles.css}}}\nh1{\n  color: white;\n  padding: 10px;\n  ",
               "background-color: #3399ff\n}\n\nul {\n  list-style-type: square;  \n}\n</code></pre>\n")
  mdcode = extract_markdown(html, "my_document.Rmd")
  expect_that(names(fromJSON(mdcode))[1], equals("my_document.Rmd"))
  expect_that(names(fromJSON(mdcode))[2], equals("styles.css"))
  expect_that(grepl("title", fromJSON(mdcode)[1]), is_true())
  expect_that(grepl("```\\{r\\}", fromJSON(mdcode)[1]), is_true())
  expect_that(grepl("\n```\n", fromJSON(mdcode)[1]), is_true())
  expect_that(grepl("list-style-type:", fromJSON(mdcode)[2]), is_true())
  
  html = paste0("<pre><code class=\"r\">\n---\ntitle: &quot;Tryout&quot;\noutput: ",
               "html_document\n---\n\nThis sample R Markdown document contains a list:\n\n* A\n* B\n\nIt ",
               "also contains some code:\n\n_tbt_{r}\nx &lt;-\n_tbt_\n</code></pre>\n\n<pre><code ",
               "class=\"r\">{{{styles.css}}}\nh1{\n  color: white;\n  padding: 10px;\n  ",
               "background-color: #3399ff\n}\n\nul {\n  list-style-type: square;  \n}\n</code></pre>\n")
  mdcode = extract_markdown(html, "my_document.Rmd")
  expect_that(names(fromJSON(mdcode))[1], equals("my_document.Rmd"))
  expect_that(names(fromJSON(mdcode))[2], equals("styles.css"))
  
  html = paste0("<pre><code class=\"r\">\n---\ntitle: &quot;Tryout&quot;\noutput: ",
               "html_document\n---\n\nThis sample R Markdown document contains a list:\n\n* A\n* B\n\nIt ",
               "also contains some code:\n\n_tbt_{r}\nx &lt;-\n_tbt_\n</code></pre>\n\n<pre><code ",
               "class=\"r\">\nh1{\n  color: white;\n  padding: 10px;\n  ",
               "background-color: #3399ff\n}\n\nul {\n  list-style-type: square;  \n}\n</code></pre>\n")
  expect_that(extract_markdown(html, "my_document.Rmd"), throws_error())
})

test_that("make_multiple_choice_vector works as expected", {
  options = "<ul>\n<li>Wrong option 1</li>\n<li>Correct option 2</li>\n<li>Wrong option 3</li>\n</ul>\n"
  expect_that(make_multiple_choice_vector(options)[1], equals("Wrong option 1"))
  expect_that(make_multiple_choice_vector(options)[2], equals("Correct option 2"))
  expect_that(make_multiple_choice_vector(options)[3], equals("Wrong option 3"))
})