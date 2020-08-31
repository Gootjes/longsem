
find_eh_resource_files <- function(path) {
  if(stringr::str_detect(string = getwd(), pattern = "testthat")) {
    path
  } else {
    paste("tests/testthat", path, sep = "/")
  }
}

eh <- readLines(find_eh_resource_files("ellenhamaker/RI-CLPM/lavaan.Rmd"))
eh_RICLPM <- glue_collapse(eh[62:108], sep = "\n")
eh_CLPM <- glue_collapse(eh[116:141], sep = "\n")
eh_RICLPM1 <- glue_collapse(eh[159:205], sep = "\n")
eh_RICLPM2 <- glue_collapse(eh[214:264], sep = "\n")
eh_RICLPM3 <- glue_collapse(eh[273:327], sep = "\n")
eh_RICLPM5 <- glue_collapse(eh[335:390], sep = "\n")
# TODO: implement all

dat <- read.table(find_eh_resource_files("ellenhamaker/RI-CLPM/data/RICLPM.dat"),
                  col.names = c("x1", "x2", "x3", "x4", "x5", "y1", "y2", "y3", "y4", "y5"))

fit_model <- function(syntax) lavaan(syntax, data = dat, missing = 'ML', meanstructure = T, int.ov.free = T)

accuracy <- 6

test_equivalence <- function(eh_fit, my_fit, accuracy = 6) {
  eh_param <- parameterEstimates(eh_fit, standardized = T)
  my_param <- parameterEstimates(my_fit, standardized = T)

  eh_param <- eh_param[order(eh_param$op, eh_param$lhs, eh_param$rhs),]
  my_param <- my_param[-which(my_param$op == ":="),]
  my_param <- my_param[order(my_param$op, my_param$lhs, my_param$rhs),]

  eh_est <- round(eh_param$est, accuracy)
  my_est <- round(my_param$est, accuracy)

  expect_equal(object = sort(eh_est), sort(my_est))
}

test_that("we can run a basic RICLPM equivalent to Hamaker spec", {



  eh_RICLPM_fit <- fit_model(eh_RICLPM)

  my_RICLPM_fit <- fit_model(generate_riclpm_syntax(nfactors = 2, factor_length = 5,
                                                    estimate_observed_intercepts = T))

  test_equivalence(eh_RICLPM_fit, my_RICLPM_fit)
})

test_that("we can run a basic CLPM equivalent to Hamaker spec", {
  accuracy <- 4

  eh_fit <- fit_model(eh_CLPM)
  my_fit <- fit_model(generate_clpm_syntax(nfactors = 2, factor_length = 5,
                                           estimate_observed_intercepts = F,
                                           estimate_latent_intercepts = T))

  eh_param <- parameterEstimates(eh_fit, standardized = T)
  my_param <- parameterEstimates(my_fit, standardized = T)

  eh_param <- eh_param[order(eh_param$op, eh_param$lhs, eh_param$rhs),]

  my_param <- my_param[-which(my_param$op == ":="),]
  my_param <- my_param[-which(my_param$op == "~1" & substr(my_param$lhs, 1, 1) == "x"),]
  my_param <- my_param[-which(my_param$op == "~1" & substr(my_param$lhs, 1, 1) == "y"),]
  my_param <- my_param[-which(my_param$op == "~~" & substr(my_param$lhs, 1, 1) == "x" & my_param$rhs == my_param$lhs),]
  my_param <- my_param[-which(my_param$op == "~~" & substr(my_param$lhs, 1, 1) == "y" & my_param$rhs == my_param$lhs),]
  my_param <- my_param[-which(my_param$op == "=~"),]
  my_param <- my_param[order(my_param$op, my_param$lhs, my_param$rhs),]

  eh_est <- round(eh_param$est, accuracy)
  my_est <- round(my_param$est, accuracy)

  expect_equal(object = eh_est, my_est)
})

test_that("we can run a RICLPM with constrained auto and cross equivalent to Hamaker spec", {



  eh_fit <- fit_model(eh_RICLPM2)

  my_fit <- fit_model(generate_riclpm_syntax(nfactors = 2, factor_length = 5,
                                             estimate_observed_intercepts = T,
                                             constrain_autoregressions = T,
                                             constrain_crosslagged = T))

  test_equivalence(eh_fit, my_fit, accuracy = 4)
})

test_that("we can run a RICLPM with constrained grand means equivalent to Hamaker spec", {



  eh_fit <- fit_model(eh_RICLPM3)

  my_fit <- fit_model(generate_riclpm_syntax(nfactors = 2, factor_length = 5,
                                             estimate_observed_intercepts = T,
                                             constrain_intercepts_over_time = T))

  test_equivalence(eh_fit, my_fit, accuracy = 4)
})

test_that("we can run a RICLPM with constrained grand means, constrained auto and cross, and constrained cov equivalent to Hamaker spec", {



  eh_fit <- fit_model(eh_RICLPM5)

  my_fit <- fit_model(generate_riclpm_syntax(nfactors = 2, factor_length = 5,
                                             estimate_observed_intercepts = T,
                                             constrain_intercepts_over_time = T,
                                             constrain_autoregressions = T,
                                             constrain_crosslagged = T,
                                             constrain_covariances = T,
                                             constrain_latent_variance_min_1 = T))

  test_equivalence(eh_fit, my_fit, accuracy = 6)
})
