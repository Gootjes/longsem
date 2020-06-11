
generate_clpm_syntax <-
  function(factor_length = 4,
           nfactors = 2,
           factor_names = c("x", "y", "z", "w"),
           constrain_autoregressions = F,
           constrain_crosslagged = F,
           constrain_observed_errors = F,
           constrain_latent_variance_min_1 = F,
           constrain_covariances = F,
           estimate_observed_intercepts = F,
           estimate_observed_errors = F,
           estimate_latent_intercepts = F) {
    # Reference source: https://osf.io/a4wtz/
    # Data source: https://osf.io/a4wtz/

    falongs <- 1:factor_length

    factor_names <- factor_names[1:nfactors]

    var_pairs <- expand.grid(var1 = 1:nfactors, var2 = 1:nfactors, wave=falongs)
    var_pairs <- var_pairs[var_pairs$var1 < var_pairs$var2,]
    var_pairs$var1 <- lapply(var_pairs$var1, function(i) factor_names[i])
    var_pairs$var2 <- lapply(var_pairs$var2, function(i) factor_names[i])

    var_pairs_min_1 <- expand.grid(var1 = 1:nfactors, var2 = 1:nfactors, wave=falongs[-1])
    var_pairs_min_1 <- var_pairs_min_1[var_pairs_min_1$var1 < var_pairs_min_1$var2,]
    var_pairs_min_1$var1 <- lapply(var_pairs_min_1$var1, function(i) factor_names[i])
    var_pairs_min_1$var2 <- lapply(var_pairs_min_1$var2, function(i) factor_names[i])

    var_pairs_1 <- expand.grid(var1 = 1:nfactors, var2 = 1:nfactors, wave=1)
    var_pairs_1 <- var_pairs_1[var_pairs_1$var1 < var_pairs_1$var2,]
    var_pairs_1$var1 <- lapply(var_pairs_1$var1, function(i) factor_names[i])
    var_pairs_1$var2 <- lapply(var_pairs_1$var2, function(i) factor_names[i])

    vs <- expand.grid(wave = falongs, var = factor_names)

    phantom_latents <-
      glue_data(vs, "eta{var}{wave} =~ 1*{var}{wave}")

    vs_min_1 <- expand.grid(wave = falongs[-1], var = factor_names)

    if (constrain_autoregressions) {
      vs_min_1$label <- glue_data(vs_min_1, "{var}")
    } else {
      vs_min_1$label <- glue_data(vs_min_1, "{var}{wave}")
    }

    autoregressives <-
      glue_data(vs_min_1, "eta{var}{wave} ~ a{label}*eta{var}{wave-1}")


    vs_wide <- data.frame(wave = falongs)
    for (v in factor_names) {
      vs_wide[v] <- v
    }

    vs_wide_min_1 <- vs_wide[-1, ]

    vs_wide_1 <- vs_wide[1, ]

    crosslagged <- NULL

    vs_wides <- sapply(var_pairs, function(pair){

    })

    if (constrain_crosslagged) {
        crosslagged_1_2 <-
          glue_data(var_pairs_min_1, "eta{var1}{wave} ~ c{var2}{var1}*eta{var2}{wave-1}")

        crosslagged_2_1 <-
          glue_data(var_pairs_min_1, "eta{var2}{wave} ~ c{var1}{var2}*eta{var1}{wave-1}")


    } else {

      crosslagged_1_2 <-
        glue_data(var_pairs_min_1, "eta{var1}{wave} ~ c{var2}{var1}{wave}*eta{var2}{wave-1}")

      crosslagged_2_1 <-
        glue_data(var_pairs_min_1, "eta{var2}{wave} ~ c{var1}{var2}{wave}*eta{var1}{wave-1}")

    }

    if(estimate_observed_errors) {
      if (constrain_observed_errors) {
        vs$label <- glue_data(vs, "{var}")
      } else {
        vs$label <- glue_data(vs, "{var}{wave}")
      }

      observed_errors <-
        glue_data(vs, "{var}{wave} ~~ ev{var}{label}*{var}{wave}")
    } else {
      observed_errors <-
        c(glue_data(vs, "{var}{wave} ~~ 0*{var}{wave}"))# TODO: consider this to add labels for 0 constraints:, glue_data(vs, "{var}{wave} ~~ ev{var}{wave}*{var}{wave}"))
    }


    if (constrain_latent_variance_min_1) {
      latent_variances_min_1 <-
        glue_data(vs_min_1, "eta{var}{wave} ~~ v{var}*eta{var}{wave}")
    } else {
      latent_variances_min_1 <-
        glue_data(vs_min_1, "eta{var}{wave} ~~ v{var}{wave}*eta{var}{wave}")
    }



    vs_1 <- expand.grid(wave = falongs[1], var = factor_names)

    latent_variances_1 <-
      glue_data(vs_1, "eta{var}{wave} ~~ v{var}{wave}*eta{var}{wave}")


    vs_wide <- data.frame(wave = falongs)
    for (v in factor_names) {
      vs_wide[v] <- v
    }

    vs_wide_min_1 <- vs_wide[-1, ]

    vs_wide_1 <- vs_wide[1, ]

    if (constrain_covariances) {
      vs_wide_min_1$label <- glue_data(vs_wide_min_1, "e")

      covariances_min_1 <-
        glue_data(
          var_pairs_min_1,
          "eta{var1}{wave} ~~ cove{var1}{var2}*eta{var2}{wave}"
        )
    } else {
      covariances_min_1 <-
        glue_data(
          var_pairs_min_1,
          "eta{var1}{wave} ~~ cove{var1}{var2}{wave}*eta{var2}{wave}"
        )
    }



    covariances_1 <-
      glue_data(
        var_pairs_1,
        "eta{var1}{wave} ~~ cov{var1}{var2}{wave}*eta{var2}{wave}"
      )

    if(estimate_observed_intercepts) {
      intercepts_observed <- glue_data(vs, "{var}{wave} ~ i{var}{wave}*1")
    } else {
      intercepts_observed <- glue_data(vs, "{var}{wave} ~ 0*1")
    }

    if(estimate_latent_intercepts) {
      intercepts_latent <- glue_data(vs, "eta{var}{wave} ~ 1")
    } else {
      intercepts_latent <- glue_data(vs, "eta{var}{wave} ~ 0*1")
    }




    computed_cov1 <-
      glue_data(
        var_pairs_1,
        "cor{var1}{var2}1 := cov{var1}{var2}{wave} / (sqrt(v{var1}{wave}) * sqrt(v{var2}{wave}))"
      )

    # if(constrain_covariances) {
    #   vs_wide_min_1$label <- glue_data(vs_wide_min_1, "e")
    # } else {
    #   vs_wide_min_1$label <- glue_data(vs_wide_min_1, "e{wave}")
    # }
    if (constrain_latent_variance_min_1) {
      if(constrain_covariances) {
        computed_cove <-
          glue_data(
            var_pairs_min_1,
            "cor{var1}{var2}{wave} := cove{var1}{var2} / (sqrt(v{var1}) * sqrt(v{var2}))"
          )
      } else {
        computed_cove <-
          glue_data(
            var_pairs_min_1,
            "cor{var1}{var2}{wave} := cove{var1}{var2}{wave} / (sqrt(v{var1}) * sqrt(v{var2}))"
          )
      }

    } else {
      if(constrain_covariances) {
        computed_cove <-
          glue_data(
            var_pairs_min_1,
            "cor{var1}{var2}{wave} := cove{var1}{var2} / (sqrt(v{var1}{wave}) * sqrt(v{var2}{wave}))"
          )
      } else {
        computed_cove <-
          glue_data(
            var_pairs_min_1,
            "cor{var1}{var2}{wave} := cove{var1}{var2}{wave} / (sqrt(v{var1}{wave}) * sqrt(v{var2}{wave}))"
          )
      }

    }

    c(
      phantom_latents,
      autoregressives,
      crosslagged_1_2,
      crosslagged_2_1,
      observed_errors,
      latent_variances_1,
      latent_variances_min_1,
      covariances_1,
      covariances_min_1,
      intercepts_observed,
      intercepts_latent,
      computed_cov1,
      computed_cove
    )
  }



generate_riclpm_syntax <- function(factor_length = 4,
                                   nfactors = 2,
                                   factor_names = c("x", "y", "z", "w"),
                                   constrain_autoregressions = F,
                                   constrain_crosslagged = F,
                                   constrain_observed_errors = F,
                                   constrain_latent_variance_min_1 = F,
                                   constrain_covariances = F,
                                   estimate_observed_intercepts = F,
                                   estimate_observed_errors = F,
                                   estimate_latent_intercepts = F,
                                   estimate_intercepts_intercepts = FALSE) {

  clpm_syntax <- generate_clpm_syntax(factor_length = factor_length,
                                      nfactors = nfactors,
                                      factor_names = factor_names,
                                      constrain_autoregressions = constrain_autoregressions,
                                      constrain_crosslagged = constrain_crosslagged,
                                      constrain_observed_errors = constrain_observed_errors,
                                      constrain_latent_variance_min_1 = constrain_latent_variance_min_1,
                                      constrain_covariances = constrain_covariances,
                                      estimate_observed_intercepts = estimate_observed_intercepts,
                                      estimate_observed_errors = estimate_observed_errors,
                                      estimate_latent_intercepts = estimate_latent_intercepts)

  falongs <- 1:factor_length

  factor_names <- factor_names[1:nfactors]

  var_pairs <- expand.grid(var1 = 1:nfactors, var2 = 1:nfactors, wave=falongs)
  var_pairs <- var_pairs[var_pairs$var1 < var_pairs$var2,]
  var_pairs$var1 <- lapply(var_pairs$var1, function(i) factor_names[i])
  var_pairs$var2 <- lapply(var_pairs$var2, function(i) factor_names[i])

  var_pairs_min_1 <- expand.grid(var1 = 1:nfactors, var2 = 1:nfactors, wave=falongs[-1])
  var_pairs_min_1 <- var_pairs_min_1[var_pairs_min_1$var1 < var_pairs_min_1$var2,]
  var_pairs_min_1$var1 <- lapply(var_pairs_min_1$var1, function(i) factor_names[i])
  var_pairs_min_1$var2 <- lapply(var_pairs_min_1$var2, function(i) factor_names[i])

  var_pairs_1 <- expand.grid(var1 = 1:nfactors, var2 = 1:nfactors, wave=1)
  var_pairs_1 <- var_pairs_1[var_pairs_1$var1 < var_pairs_1$var2,]
  var_pairs_1$var1 <- lapply(var_pairs_1$var1, function(i) factor_names[i])
  var_pairs_1$var2 <- lapply(var_pairs_1$var2, function(i) factor_names[i])

  vs <- expand.grid(wave = falongs, var = factor_names)

  vs_min_1 <- expand.grid(wave = falongs[-1], var = factor_names)

  vs_1 <- expand.grid(wave = falongs[1], var = factor_names)

  intercept_latents <- glue_data(vs, "i{var} =~ 1*{var}{wave}")

  intercept_variances <- glue(var = factor_names, "i{var} ~~ vi{var}*i{var}")

  i_pairs <- permutations(factor_names, factor_names)

  intercept_covariances <- glue_data(i_pairs,
                                     "i{var1} ~~ covi{var1}{var2}*i{var2}")

  intercept_covariances_1 <- glue_data(expand.grid(var1 = factor_names, var2 = factor_names),
                                       "eta{var1}1 ~~ 0*i{var2}")

  if(estimate_intercepts_intercepts) {
    intercept_intercepts <- glue(var = factor_names, "i{var} ~ 1")
  } else {
    intercept_intercepts <- glue(var = factor_names, "i{var} ~ 0*1")
  }


  intercept_correlations <- glue_data(i_pairs,
                                 "cori{var1}{var2} := covi{var1}{var2} / (sqrt(vi{var1}) * sqrt(vi{var2}))")

  c(
    clpm_syntax,
    intercept_latents,
    intercept_variances,
    intercept_covariances,
    intercept_covariances_1,
    intercept_intercepts,
    intercept_correlations
  )
}

