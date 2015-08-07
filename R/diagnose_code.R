#' @importFrom lintr lint
diagnose_code <- function(code, lint_info) {
  file <- "code.R"
  write(code, file = file)
  lints <- lintr::lint(file)
  pimped_lints <- lapply(lints, function(x) { 
    x$num <- lint_info$num
    x$code_type <- lint_info$type
    x })
  dc_lints$add(pimped_lints)
}


lint_accessors = function() {
  lints <- list()
  
  get = function() {
    lints
  }
  
  add = function(el) {
    lints <<- c(lints,el)
    invisible(NULL)
  }
  
  clear = function() {
    dc_data <<- list()
    invisible(NULL)
  }
  
  list(get = get, add = add, clear = clear)
}

dc_lints <- lint_accessors()

#' @export
display_lints <- function() {
  lapply(dc_lints$get(), display_single_lint)
  invisible(NULL)
}

display_single_lint <- function(lint) {
  cat(sprintf("Exercise %s - %s\nCode: %s\nLine: %s\nColumn: %s\nMessage: %s\n\n", lint$num, lint$code_type, lint$line, lint$line_number, lint$column, lint$message))
}
