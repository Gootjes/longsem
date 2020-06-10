

capture_warnings <- function(code) {
  private <- environment()

  warnings <- NULL

  withCallingHandlers(code, warning = function(w) {
    private$warnings <- append(private$warnings, list(w))
  })

  warnings
}

prettify_warnings <- function(warnings, collapse = TRUE) {

  stage1 <- sapply(warnings, function(w) {
    #paste("In", paste(deparse(w$call), collapse = "\n"), ": \n  ", w$message) # This was just too ugly.
    w$message
  })
  if(collapse) {
    paste(stage1, collapse = "\n")
  } else {
    stage1
  }
}
