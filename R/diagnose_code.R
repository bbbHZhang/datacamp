#' @importFrom lintr lint
diagnose_code <- function(code) {
  file <- "code.R"
  file_conn <- file(file)
  writeLines(code, file_conn)
  close(file_conn) 
  lintr::lint(file)
}