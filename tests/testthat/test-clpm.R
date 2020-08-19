test_that("clpm works", {
  expect_silent({
    data("Fakedata_RI-CLPM_R", package = "riclpm")
    data <- `Fakedata_RI-CLPM_R`
    clpm(data = data, factors = list(list(
      label = "x", vars = list("x1", "x2", "x3", "x4")
    ), list(
      label = "y", vars = list("y1", "y2", "y3", "y4")
    )))
  })
})

test_that("generated syntax is sensible", {
  expect_silent({
    lavaan::lavaanify(model = generate_clpm_syntax(factor_length = 4,
                                                   nfactors = 2,
                                                   constrain_autoregressions = F,
                                                   constrain_crosslagged = F,
                                                   constrain_observed_errors = F,
                                                   constrain_latent_variance_min_1 = T,
                                                   constrain_covariances = F,
                                                   factor_names = c("x", "y")))
  })
})

test_that("we can run a lavaan model #1", {
  expect_silent({
    data("Fakedata_RI-CLPM_R", package = "riclpm")
    data <- `Fakedata_RI-CLPM_R`
    lavaan::lavaan(model = generate_clpm_syntax(factor_length = 4,
                                                nfactors = 2,
                                                constrain_autoregressions = F,
                                                constrain_crosslagged = F,
                                                constrain_observed_errors = F,
                                                constrain_covariances = F,
                                                factor_names = c("x", "y")),
                   data = data
                   )
  })
})



test_that("we can run a lavaan model #2", {
  expect_silent({
    data("Fakedata_RI-CLPM_R", package = "riclpm")
    data <- `Fakedata_RI-CLPM_R`
    lavaan::lavaan(model = generate_clpm_syntax(factor_length = 3,
                                                nfactors = 2,
                                                constrain_autoregressions = F,
                                                constrain_crosslagged = F,
                                                constrain_observed_errors = F,
                                                constrain_latent_variance_min_1 = T,
                                                constrain_covariances = F,
                                                factor_names = c("x", "y")),
                   data = data,
                   int.ov.free = F,
                   int.lv.free = F,
                   auto.fix.first = F,
                   auto.fix.single = F,
                   auto.cov.lv.x = F,
                   auto.cov.y = F,
                   auto.var = F
                   )
  })
})



test_that("we can reproduce Flournoy's CLPM", {
  expect_equal(object = {
    data("antiread", package = "riclpm")
    data <- antiread
    names(data) <- gsub(pattern = "^anti([0-9])$", replacement = "x\\1", x = names(data))
    names(data) <- gsub(pattern = "^read([0-9])$", replacement = "y\\1", x = names(data))
    f1 <- lavaan::lavaan(model = generate_clpm_syntax(factor_length = 3,
                                                      nfactors = 2,
                                                      constrain_autoregressions = F,
                                                      constrain_crosslagged = F,
                                                      constrain_observed_errors = F,
                                                      constrain_latent_variance_min_1 = F,
                                                      constrain_covariances = F,
                                                      estimate_observed_intercepts = T,
                                                      estimate_observed_errors = F,
                                                      estimate_latent_intercepts = F,
                                                      factor_names = c("x", "y")),
                         data = data,
                         missing = 'ML',
                         int.ov.free = F,
                         int.lv.free = F,
                         auto.fix.first = F,
                         auto.fix.single = F,
                         auto.cov.lv.x = F,
                         auto.cov.y = F,
                         auto.var = F
    )

    f1_p <- lavaan::parTable(f1)
    f1_p$lhs <- gsub(pattern = "^etax([0-9])$", replacement = "p\\1", f1_p$lhs)
    f1_p$lhs <- gsub(pattern = "^etay([0-9])$", replacement = "q\\1", f1_p$lhs)

    f1_p$rhs <- gsub(pattern = "^etax([0-9])$", replacement = "p\\1", f1_p$rhs)
    f1_p$rhs <- gsub(pattern = "^etay([0-9])$", replacement = "q\\1", f1_p$rhs)

    f1_p <- f1_p[-which(f1_p$op == ":="),]

    round(f1_p[order(f1_p$lhs, f1_p$op, f1_p$rhs),"est"], 3)
  },
  expected = {

    data("antiread", package = "riclpm")

    data <- antiread
    names(data) <- gsub(pattern = "^anti([0-9])$", replacement = "x\\1", x = names(data))
    names(data) <- gsub(pattern = "^read([0-9])$", replacement = "y\\1", x = names(data))

    m_flournoy <- '
    #Note, the data contain x1-3 and y1-3
      #Latent mean Structure with intercepts

      kappa =~ 1*x1 + 1*x2 + 1*x3
    omega =~ 1*y1 + 1*y2 + 1*y3

    x1 ~ mu1*1 #intercepts
    x2 ~ mu2*1
    x3 ~ mu3*1
    y1 ~ pi1*1
    y2 ~ pi2*1
    y3 ~ pi3*1

    kappa ~~ 0*kappa #variance nope
    omega ~~ 0*omega #variance nope
    kappa ~~ 0*omega #covariance not even

    #laten vars for AR and cross-lagged effects
    p1 =~ 1*x1 #each factor loading set to 1
    p2 =~ 1*x2
    p3 =~ 1*x3
    q1 =~ 1*y1
    q2 =~ 1*y2
    q3 =~ 1*y3

    p3 ~ alpha3*p2 + beta3*q2
    p2 ~ alpha2*p1 + beta2*q1

    q3 ~ delta3*q2 + gamma3*p2
    q2 ~ delta2*q1 + gamma2*p1

    p1 ~~ p1 #variance
    p2 ~~ u2*p2
    p3 ~~ u3*p3
    q1 ~~ q1 #variance
    q2 ~~ v2*q2
    q3 ~~ v3*q3

    p1 ~~ q1 #p1 and q1 covariance
    p2 ~~ q2 #p2 and q2 covariance
    p3 ~~ q3 #p2 and q2 covariance'
    f_flournoy <- lavaan::lavaan(model = m_flournoy,
                                 data = data,
                                 missing = 'ML',
                                 int.ov.free = F,
                                 int.lv.free = F,
                                 auto.fix.first = F,
                                 auto.fix.single = F,
                                 auto.cov.lv.x = F,
                                 auto.cov.y = F,
                                 auto.var = F
    )



    f_flournoy_p <- lavaan::parTable(f_flournoy)
    f_flournoy_p <- f_flournoy_p[-which(f_flournoy_p$lhs %in% c("kappa", "omega")),]

    round(f_flournoy_p[order(f_flournoy_p$lhs, f_flournoy_p$op, f_flournoy_p$rhs),"est"], 3)
  })
})
