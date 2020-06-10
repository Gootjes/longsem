

make_schematic <- function(n_waves = 4, n_factors = 2, factor_names = c("x", "y", "z", "w"), return_spec = T) {

  factors <- factor_names[1:n_factors]

  A <- matrix(data = 0, nrow = n_waves*n_factors, ncol = n_waves*n_factors, byrow = T)

  vs <- permutations_long_wave(factors = factors, waves = 1:n_waves)
  vs <- vs[order(vs$factor, vs$wave),]
  vs_min_1 <- permutations_long_wave(factors = factors, waves = 2:n_waves)

  vs_wide <- permutations_wide_wave(factor1 = factors, factor2 = factors, waves = 1:n_waves)
  vs_wide_min_1 <- permutations_wide_wave(factor1 = factors, factor2 = factors, waves = 2:n_waves)

  varnames <- glue_data(vs, "eta{factor_name}{wave}")

  rownames(A) <- varnames
  colnames(A) <- varnames

  # Cross lagged

  s <- vs_wide_min_1[with(vs_wide_min_1, factor1 < factor2),]

  for(i in 1:nrow(s)) {
    d <- s[i,]
    a <- glue_data(d, "eta{factor1_name}{wave}")
    b <- glue_data(d, "eta{factor2_name}{wave-1}")
    A[a,b] <- glue_data(d, "c{factor2_name}{factor1_name}{wave}")
    #A[a,b] <- round(rbeta(1, 1, 1), 2)
  }

  s <- vs_wide_min_1[with(vs_wide_min_1, factor2 < factor1),]

  for(i in 1:nrow(s)) {
    d <- s[i,]
    a <- glue_data(d, "eta{factor1_name}{wave}")
    b <- glue_data(d, "eta{factor2_name}{wave-1}")
    A[a,b] <- glue_data(d, "c{factor2_name}{factor1_name}{wave}")
  }

  # Auto lagged

  s <- vs_wide_min_1[with(vs_wide_min_1, factor1 == factor2),]

  for(i in 1:nrow(s)) {
    d <- s[i,]
    a <- glue_data(d, "eta{factor1_name}{wave}")
    b <- glue_data(d, "eta{factor2_name}{wave-1}")
    A[a,b] <- glue_data(d, "a{factor2_name}{wave}")
  }

  # Covariances

  s <- vs_wide[with(vs_wide, factor1 > factor2),]

  for(i in 1:nrow(s)) {
    d <- s[i,]
    a <- glue_data(d, "eta{factor1_name}{wave}")
    b <- glue_data(d, "eta{factor2_name}{wave}")
    A[a,b] <- glue_data(d, "cove{factor2_name}{factor1_name}{wave}")
    A[b,a] <- ""
  }

  spec <- list(A = A, pos = rep(n_waves, each = n_factors), curve = 0,
               name = varnames,
               relsize = 1, box.size = 0.05, arr.pos = .70, shadow.size = 0, arr.type = "triangle",
               arr.length = .3)

  if(return_spec) {
    return(spec)
  }

  do.call(diagram::plotmat, args = spec)

}
