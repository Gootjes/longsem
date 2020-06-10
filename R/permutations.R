
permutations <- function(var1, var2) {
  var_pairs <- expand.grid(var1 = 1:length(var1), var2 = 1:length(var2))
  var_pairs <- var_pairs[var_pairs$var1 < var_pairs$var2,]
  var_pairs$var1 <- lapply(var_pairs$var1, function(i) var1[i])
  var_pairs$var2 <- lapply(var_pairs$var2, function(i) var2[i])

  var_pairs
}

permutations3 <- function(var1, var2, wave) {
  var_pairs <- expand.grid(var1 = 1:length(var1), var2 = 1:length(var2), wave=wave)
  var_pairs <- var_pairs[var_pairs$var1 < var_pairs$var2,]
  var_pairs$var1 <- lapply(var_pairs$var1, function(i) var1[i])
  var_pairs$var2 <- lapply(var_pairs$var2, function(i) var2[i])

  var_pairs
}

permutations_long_wave <- function(factors, waves) {
  x <- expand.grid(factor = 1:length(factors), wave = waves)
  x$factor_name <- factors[x$factor]

  x
}

permutations_wide <- function(factor1, factor2, filter = FALSE) {
  var_pairs <- expand.grid(factor1 = 1:length(factor1), factor2 = 1:length(factor2))
  if(filter) var_pairs <- var_pairs[var_pairs$factor1 < var_pairs$factor2,]
  var_pairs$factor1_name <- lapply(var_pairs$factor1, function(i) factor1[i])
  var_pairs$factor2_name <- lapply(var_pairs$factor2, function(i) factor2[i])

  var_pairs
}

permutations_wide_wave <- function(factor1, factor2, waves, filter = FALSE) {
  var_pairs <- expand.grid(factor1 = 1:length(factor1), factor2 = 1:length(factor2), wave=waves)
  if(filter) var_pairs <- var_pairs[var_pairs$factor1 < var_pairs$factor2,]
  var_pairs$factor1_name <- lapply(var_pairs$factor1, function(i) factor1[i])
  var_pairs$factor2_name <- lapply(var_pairs$factor2, function(i) factor2[i])

  var_pairs
}
