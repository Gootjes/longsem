test_that("riclpm works", {
  expect_silent({
    data("Fakedata_RI-CLPM_R", package = "longsem")
    data <- `Fakedata_RI-CLPM_R`
    riclpm(data = data, factors = list(list(
      label = "x", vars = list("x1", "x2", "x3", "x4")
    ), list(
      label = "y", vars = list("y1", "y2", "y3", "y4")
    )))
  })
})


test_that("we can replicate Flournoy", {
  expect_equal(
    object = {
      data("antiread", package = "longsem")
      data <- antiread
      names(data) <- gsub(pattern = "^anti([0-9])$", replacement = "x\\1", x = names(data))
      names(data) <- gsub(pattern = "^read([0-9])$", replacement = "y\\1", x = names(data))
      f1 <- lavaan::lavaan(model = generate_riclpm_syntax(factor_length = 3,
                                                          nfactors = 2,
                                                          constrain_autoregressions = F,
                                                          constrain_crosslagged = F,
                                                          estimate_observed_intercepts = T,
                                                          estimate_intercepts_intercepts = F,
                                                          constrain_covariances = F,
                                                          fix_random_intercept_first_wave_covariance_to_zero = T),
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

      f1_p$lhs <- gsub(pattern = "^ix$", replacement = "kappa", f1_p$lhs)
      f1_p$lhs <- gsub(pattern = "^iy$", replacement = "omega", f1_p$lhs)

      f1_p$rhs <- gsub(pattern = "^ix$", replacement = "kappa", f1_p$rhs)
      f1_p$rhs <- gsub(pattern = "^iy$", replacement = "omega", f1_p$rhs)

      f1_p <- f1_p[-which(f1_p$op == ":="),]
      f1_p <- f1_p[-51:-54,]

      sort(round(f1_p[order(f1_p$lhs, f1_p$op, f1_p$rhs),"est"], 3))
    },
    expected = {
      data("antiread", package = "longsem")
      data <- antiread
      names(data) <- gsub(pattern = "^anti([0-9])$", replacement = "x\\1", x = names(data))
      names(data) <- gsub(pattern = "^read([0-9])$", replacement = "y\\1", x = names(data))
      m_flournoy <-
        '
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

kappa ~~ kappa #variance
omega ~~ omega #variance
kappa ~~ omega #covariance

#laten vars for AR and cross-lagged effects
p1 =~ 1*x1 #each factor loading set to 1
p2 =~ 1*x2
p3 =~ 1*x3
q1 =~ 1*y1
q2 =~ 1*y2
q3 =~ 1*y3

#Later, we may constrain autoregression and cross-lagged
#effects to be the same across both lags.
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

      sort(round(f_flournoy_p[order(f_flournoy_p$lhs, f_flournoy_p$op, f_flournoy_p$rhs),"est"], 3))
    }
  )

})
