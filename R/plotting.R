

n_waves = 4; n_factors = 2; factor_names = c("x", "y", "z", "w"); return_spec = T


make_schematic <- function(n_waves = 4, n_factors = 2, factor_names = c("x", "y", "z", "w"), return_spec = T,
                           constrain_autoregressions = F,
                           constrain_crosslagged = F,
                           constrain_covariances = F,
                           include_random_intercept = F,
                           constrain_residual_variances = F) {

  if(n_factors != 2) {
    stop(glue("schematic not supported for any other number of factors ({n_factors}) than 2"))
  }

  factors <- factor_names[1:n_factors]
  waves <- 1:n_waves

  vs <- permutations_long_wave(factors = factors, waves = 1:n_waves)
  vs <- vs[order(vs$factor, vs$wave),]
  vs_min_1 <- permutations_long_wave(factors = factors, waves = 2:n_waves)

  vs_wide <- permutations_wide_wave(factor1 = factors, factor2 = factors, waves = 1:n_waves)
  vs_wide_min_1 <- permutations_wide_wave(factor1 = factors, factor2 = factors, waves = 2:n_waves)
  vs_wide_1 <- permutations_wide_wave(factor1 = factors, factor2 = factors, waves = 1)


  if(include_random_intercept) {
    size <- ((n_waves*n_factors)*2) + n_factors
    A <- matrix(data = 0, nrow = size, ncol = size, byrow = T)
    varnames <- c(c(glue("{fac}{wave}", fac = factors[1], wave = waves),
                    glue("eta{fac}{wave}", fac = factors[1], wave = waves)),
                  c(glue("eta{fac}{wave}", fac = factors[2], wave = waves),
                    glue("{fac}{wave}", fac = factors[2], wave = waves)))
    varnames <- c(glue("{fac}", fac = factors[1]), varnames, glue("{fac}", fac = factors[2]))
  } else {
    A <- matrix(data = 0, nrow = n_waves*n_factors*2, ncol = n_waves*n_factors*2, byrow = T)
    varnames <- c(c(glue("{fac}{wave}", fac = factors[1], wave = waves),
                    glue("eta{fac}{wave}", fac = factors[1], wave = waves)),
                  c(glue("eta{fac}{wave}", fac = factors[2], wave = waves),
                    glue("{fac}{wave}", fac = factors[2], wave = waves)))
  }



  rownames(A) <- varnames
  colnames(A) <- varnames


  positions_first_half <- expand.grid(x = seq(from = .1, to = .9, length.out = n_waves), y = c(.775, .63))
  positions_second_half <- expand.grid(x = seq(from = .1, to = .9, length.out = n_waves), y = rev(c(.225, .37)))
  if(include_random_intercept) {
    positions <- as.matrix(rbind(data.frame(x = .5, y = .925), positions_first_half, positions_second_half, data.frame(x = .5, y = .075)))
  } else {
    positions <- as.matrix(rbind(positions_first_half, positions_second_half))
  }


  box_types <- c(rep("rect", each = n_waves), rep("ellipse", each = n_waves), rep("ellipse", each = n_waves), rep("rect", each = n_waves))

  if(include_random_intercept) {
    box_types <- c("ellipse", box_types, "ellipse")
  }

  box_sizes <- ifelse(box_types == "rect", .02, .04)



  # Latent to indicator

  s <- vs_wide[with(vs_wide, factor1 == factor2),]

  for(i in 1:nrow(s)) {
    d <- s[i,]
    a <- glue_data(d, "{factor1_name}{wave}")
    b <- glue_data(d, "eta{factor2_name}{wave}")
    A[a,b] <- glue_data(d, "1")

    #A[a,b] <- round(rbeta(1, 1, 1), 2)
  }


  # indicator to random intercept

  if(include_random_intercept) {
    s <- vs_wide[with(vs_wide, factor1 == factor2),]

    for(i in 1:nrow(s)) {
      d <- s[i,]
      a <- glue_data(d, "{factor1_name}{wave}")
      b <- glue_data(d, "{factor2_name}")
      A[a,b] <- glue_data(d, "1")

      #A[a,b] <- round(rbeta(1, 1, 1), 2)
    }

  }



  # Cross lagged

  s <- vs_wide_min_1[with(vs_wide_min_1, factor1 < factor2),]

  for(i in 1:nrow(s)) {
    d <- s[i,]
    a <- glue_data(d, "eta{factor1_name}{wave}")
    b <- glue_data(d, "eta{factor2_name}{wave-1}")
    if(constrain_crosslagged) {
      A[a,b] <- glue_data(d, "c{factor2_name}{factor1_name}")
    } else {
      A[a,b] <- glue_data(d, "c{factor2_name}{factor1_name}{wave}")
    }

    #A[a,b] <- round(rbeta(1, 1, 1), 2)
  }

  s <- vs_wide_min_1[with(vs_wide_min_1, factor2 < factor1),]

  for(i in 1:nrow(s)) {
    d <- s[i,]
    a <- glue_data(d, "eta{factor1_name}{wave}")
    b <- glue_data(d, "eta{factor2_name}{wave-1}")
    if(constrain_crosslagged) {
      A[a,b] <- glue_data(d, "c{factor2_name}{factor1_name}")
    } else {
      A[a,b] <- glue_data(d, "c{factor2_name}{factor1_name}{wave}")
    }

  }

  # Auto lagged

  s <- vs_wide_min_1[with(vs_wide_min_1, factor1 == factor2),]

  for(i in 1:nrow(s)) {
    d <- s[i,]
    a <- glue_data(d, "eta{factor1_name}{wave}")
    b <- glue_data(d, "eta{factor2_name}{wave-1}")
    if(constrain_autoregressions) {
      A[a,b] <- glue_data(d, "a{factor2_name}")
    } else {
      A[a,b] <- glue_data(d, "a{factor2_name}{wave}")
    }

  }

  # Covariances

  s <- vs_wide_1[with(vs_wide_1, factor1 > factor2),]

  d <- s[1,]
  a <- glue_data(d, "eta{factor1_name}{wave}")
  b <- glue_data(d, "eta{factor2_name}{wave}")
  A[a,b] <- glue_data(d, "cov{factor2_name}{factor1_name}{wave}")
  A[b,a] <- ""

  s <- vs_wide_min_1[with(vs_wide_min_1, factor1 > factor2),]

  for(i in 1:nrow(s)) {
    d <- s[i,]
    a <- glue_data(d, "eta{factor1_name}{wave}")
    b <- glue_data(d, "eta{factor2_name}{wave}")
    if(constrain_covariances) {
      A[a,b] <- glue_data(d, "cove{factor2_name}{factor1_name}")
    } else {
      A[a,b] <- glue_data(d, "cove{factor2_name}{factor1_name}{wave}")
    }

    A[b,a] <- ""
  }


  spec <- list(A = A, pos = positions, curve = 0,
               name = varnames,
               relsize = 1, box.size = box_sizes, arr.pos = .70, shadow.size = 0, arr.type = "triangle",
               arr.length = .3,
               box.type = box_types,
               cex = .85,
               box.cex = .85,
               dtext = .6
               )

  class(spec) <- c("longsem_schematic", class(spec))

  if(return_spec) {
    return(spec)
  }

  do.call(diagram::plotmat, args = spec)

}

#' @export
#' @param values Should be a lavaan fit object
update.longsem_schematic <- function(object, values, standardized = FALSE) {
  est <- lavaan::parameterEstimates(object = values, standardized = standardized, pvalue = TRUE)

  rel_est <- est[with(est, stringr::str_starts(label, "(a|c|cov|cove)[xy]")),]

  if(standardized) {
    est_values <- rel_est$std.all
  } else {
    est_values <- rel_est$est
  }

  p_values <- rel_est$pvalue

  path_labels <- rel_est$label

  formatted_values <- sapply(X = seq_along(est_values), FUN = function(i) {
    v <- est_values[i]
    p <- p_values[i]
    stars <- ""
    if(p < .001) {
      stars <- "" # "***"
    } else if(p < .01) {
      stars <- "" # "**"
    } else if(p < .05) {
      stars <- "" # "*"
    } else {
      stars <- ""
    }

    paste(format(round(v, 2), nsmall = 2), stars, sep="")
  })

  for(i in seq_along(formatted_values)) {
    lab <- path_labels[i]
    v <- formatted_values[i]

    object$A[object$A == lab] <- v
  }

  object
}

#' @export
plot.longsem_schematic <- function(x) {
  do.call(diagram::plotmat, x)
}
