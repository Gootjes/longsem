
#' @importFrom glue glue
#' @importFrom glue glue_collapse
readiness <- function(options) {
  result <- list(reason = NULL, ready = TRUE, report = FALSE)

  factors <- options$factors

  if(length(factors) == 0) {
    result$ready <- FALSE
    result$report <- FALSE
    result$reason <- glue("we need at least 2 factors to run this analysis.")
  } else if(length(factors) == 1) {
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- glue("we need at least 2 factors to run this analysis.")
  } else {
    factor_lengths <- sapply(factors, function(v) length(v$vars))

    if(length(unique(factor_lengths)) != 1) {
      result$ready <- FALSE
      result$report <- TRUE
      result$reason <- glue("factors have unequal lengths: {glue_collapse(factor_lengths, sep=', ')}")
    } else if(unique(factor_lengths)[[1]] == 0) {
      result$ready <- FALSE
      result$report <- FALSE
      result$reason <- glue("the factors have no indicators yet")
    }

  }

  return(result)
}
