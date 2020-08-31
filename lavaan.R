## # If necessary, install the 'Lavaan' package.
## # install.packages('lavaan', dependencies = T)
## # Load the required packages.
## require(lavaan)
## 
## # Load in the data.
## ## Traditional RI-CLPM
## dat <- read.table("/Users/jeroenmulder/Git/RICLPM/data/RICLPM.dat",
##                   col.names = c("x1", "x2", "x3", "x4", "x5", "y1", "y2", "y3", "y4", "y5"))
## ## Extension 1
## datZ <- read.table("/Users/jeroenmulder/Git/RICLPM/data/RICLPM-Z.dat",
##                    col.names = c("x1", "x2", "x3", "x4", "x5", "y1", "y2", "y3", "y4", "y5", "z2", "z1"))
## ## Extension 2
## datMG <- read.table("/Users/jeroenmulder/Git/RICLPM/data/RICLPM-MG.dat",
##                     col.names = c("x1", "x2", "x3", "x4", "x5", "y1", "y2", "y3", "y4", "y5", "G"))
## ## Extension 3
## datMI <- read.table("/Users/jeroenmulder/Git/RICLPM/data/RICLPM-MI.dat",
##                    col.names = c("x11", "x12", "x13",
##                                  "x21", "x22", "x23",
##                                  "x31", "x32", "x33",
##                                  "x41", "x42", "x43",
##                                  "x51", "x52", "x53",
## 
##                                  "y11", "y12", "y13",
##                                  "y21", "y22", "y23",
##                                  "y31", "y32", "y33",
##                                  "y41", "y42", "y43",
##                                  "y51", "y52", "y53"))

## RICLPM <- '
##   # Create between components (random intercepts)
##   RIx =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5
##   RIy =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
## 
##   # Create within-person centered variables
##   wx1 =~ 1*x1
##   wx2 =~ 1*x2
##   wx3 =~ 1*x3
##   wx4 =~ 1*x4
##   wx5 =~ 1*x5
##   wy1 =~ 1*y1
##   wy2 =~ 1*y2
##   wy3 =~ 1*y3
##   wy4 =~ 1*y4
##   wy5 =~ 1*y5
## 
##   # Estimate the lagged effects between the within-person centered variables.
##   wx2 + wy2 ~ wx1 + wy1
##   wx3 + wy3 ~ wx2 + wy2
##   wx4 + wy4 ~ wx3 + wy3
##   wx5 + wy5 ~ wx4 + wy4
## 
##   # Estimate the covariance between the within-person centered variables at the first wave.
##   wx1 ~~ wy1 # Covariance
## 
##   # Estimate the covariances between the residuals of the within-person centered variables (the innovations).
##   wx2 ~~ wy2
##   wx3 ~~ wy3
##   wx4 ~~ wy4
##   wx5 ~~ wy5
## 
##   # Estimate the variance and covariance of the individual factors.
##   RIx ~~ RIx
##   RIy ~~ RIy
##   RIx ~~ RIy
## 
##   # Estimate the (residual) variance of the within-person centered variables.
##   wx1 ~~ wx1 # Variances
##   wy1 ~~ wy1
##   wx2 ~~ wx2 # Residual variances
##   wy2 ~~ wy2
##   wx3 ~~ wx3
##   wy3 ~~ wy3
##   wx4 ~~ wx4
##   wy4 ~~ wy4
##   wx5 ~~ wx5
##   wy5 ~~ wy5
## '
## RICLPM.fit <- lavaan(RICLPM, data = dat, missing = 'ML', meanstructure = T, int.ov.free = T)
## summary(RICLPM.fit, standardized = T)

## CLPM <- '
##   # Estimate the lagged effects between the observed variables.
##   x2 + y2 ~ x1 + y1
##   x3 + y3 ~ x2 + y2
##   x4 + y4 ~ x3 + y3
##   x5 + y5 ~ x4 + y4
## 
##   # Estimate the covariance between the observed variables at the first wave.
##   x1 ~~ y1 # Covariance
## 
##   # Estimate the covariances between the residuals of the observed variables.
##   x2 ~~ y2
##   x3 ~~ y3
##   x4 ~~ y4
##   x5 ~~ y5
## 
##   # Estimate the (residual) variance of the observed variables.
##   x1 ~~ x1 # Variances
##   y1 ~~ y1
##   x2 ~~ x2 # Residual variances
##   y2 ~~ y2
##   x3 ~~ x3
##   y3 ~~ y3
##   x4 ~~ x4
##   y4 ~~ y4
##   x5 ~~ x5
##   y5 ~~ y5
## '
## CLPM.fit <- lavaan(CLPM, data = dat, missing = 'ML', meanstructure = T, int.ov.free = T)
## summary(CLPM.fit, standardized = T)

## # Unconstrained factor loadings for the random intercept.
## RICLPM1 <- '
##   # Create between components (random intercepts)
##   RIx =~ 1*x1 + x2 + x3 + x4 + x5
##   RIy =~ 1*y1 + y2 + y3 + y4 + y5
## 
##   # Create within-person centered variables
##   wx1 =~ 1*x1
##   wx2 =~ 1*x2
##   wx3 =~ 1*x3
##   wx4 =~ 1*x4
##   wx5 =~ 1*x5
##   wy1 =~ 1*y1
##   wy2 =~ 1*y2
##   wy3 =~ 1*y3
##   wy4 =~ 1*y4
##   wy5 =~ 1*y5
## 
##   # Estimate the lagged effects between the within-person centered variables.
##   wx2 + wy2 ~ wx1 + wy1
##   wx3 + wy3 ~ wx2 + wy2
##   wx4 + wy4 ~ wx3 + wy3
##   wx5 + wy5 ~ wx4 + wy4
## 
##   # Estimate the covariances between the residuals of the within-person centered variables (the innovations).
##   wx2 ~~ wy2
##   wx3 ~~ wy3
##   wx4 ~~ wy4
##   wx5 ~~ wy5
## 
##   # Estimate the covariance between the within-person centered variables at the first wave.
##   wx1 ~~ wy1 # Covariance
## 
##   # Estimate the variance and covariance of the individual factors.
##   RIx ~~ RIx
##   RIy ~~ RIy
##   RIx ~~ RIy
## 
##   # Estimate the (residual) variance of the within-person centered variables.
##   wx1 ~~ wx1 # Variance
##   wy1 ~~ wy1
##   wx2 ~~ wx2 # Residual variance
##   wy2 ~~ wy2
##   wx3 ~~ wx3
##   wy3 ~~ wy3
##   wx4 ~~ wx4
##   wy4 ~~ wy4
##   wx5 ~~ wx5
##   wy5 ~~ wy5
## '
## RICLPM1.fit <- lavaan(RICLPM1, data = dat, missing = 'ML', meanstructure = T, int.ov.free = T)
## summary(RICLPM1.fit, standardized = T)

## # Fixed autoregressive and cross-lagged relations over time.
## RICLPM2 <- '
##   # Create between components (random intercepts)
##   RIx =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5
##   RIy =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
## 
##   # Create within-person centered variables
##   wx1 =~ 1*x1
##   wx2 =~ 1*x2
##   wx3 =~ 1*x3
##   wx4 =~ 1*x4
##   wx5 =~ 1*x5
##   wy1 =~ 1*y1
##   wy2 =~ 1*y2
##   wy3 =~ 1*y3
##   wy4 =~ 1*y4
##   wy5 =~ 1*y5
## 
##   # Estimate the lagged effects between the within-person centered variables.
##   wx2 ~ a*wx1 + b*wy1
##   wy2 ~ c*wx1 + d*wy1
##   wx3 ~ a*wx2 + b*wy2
##   wy3 ~ c*wx2 + d*wy2
##   wx4 ~ a*wx3 + b*wy3
##   wy4 ~ c*wx3 + d*wy3
##   wx5 ~ a*wx4 + b*wy4
##   wy5 ~ c*wx4 + d*wy4
## 
##   # Estimate the covariances between the residuals of the within-person centered variables (the innovations).
##   wx2 ~~ wy2
##   wx3 ~~ wy3
##   wx4 ~~ wy4
##   wx5 ~~ wy5
## 
##   # Estimate the covariance between the within-person centered variables at the first wave.
##   wx1 ~~ wy1 # Covariance
## 
##   # Estimate the variance and covariance of the individual factors.
##   RIx ~~ RIx
##   RIy ~~ RIy
##   RIx ~~ RIy
## 
##   # Estimate the (residual) variance of the within-person centered variables.
##   wx1 ~~ wx1 # Variance
##   wy1 ~~ wy1
##   wx2 ~~ wx2 # Residual variance
##   wy2 ~~ wy2
##   wx3 ~~ wx3
##   wy3 ~~ wy3
##   wx4 ~~ wx4
##   wy4 ~~ wy4
##   wx5 ~~ wx5
##   wy5 ~~ wy5
## '
## RICLPM2.fit <- lavaan(RICLPM2, data = dat, missing = 'ML', meanstructure = T, int.ov.free = T)
## summary(RICLPM2.fit, standardized = T)

## # Constrained grand means.
## RICLPM3 <- '
##   # Create between components (random intercepts)
##   RIx =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5
##   RIy =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
## 
##   # Create within-person centered variables
##   wx1 =~ 1*x1
##   wx2 =~ 1*x2
##   wx3 =~ 1*x3
##   wx4 =~ 1*x4
##   wx5 =~ 1*x5
##   wy1 =~ 1*y1
##   wy2 =~ 1*y2
##   wy3 =~ 1*y3
##   wy4 =~ 1*y4
##   wy5 =~ 1*y5
## 
##   # Estimate the lagged effects between the within-person centered variables.
##   wx2 ~ wx1 + wy1
##   wy2 ~ wx1 + wy1
##   wx3 ~ wx2 + wy2
##   wy3 ~ wx2 + wy2
##   wx4 ~ wx3 + wy3
##   wy4 ~ wx3 + wy3
##   wx5 ~ wx4 + wy4
##   wy5 ~ wx4 + wy4
## 
##   # Estimate the covariances between the residuals of the within-person centered variables (the innovations).
##   wx2 ~~ wy2
##   wx3 ~~ wy3
##   wx4 ~~ wy4
##   wx5 ~~ wy5
## 
##   # Estimate the covariance between the within-person centered variables at the first wave.
##   wx1 ~~ wy1 # Covariance
## 
##   # Estimate the variance and covariance of the individual factors.
##   RIx ~~ RIx
##   RIy ~~ RIy
##   RIx ~~ RIy
## 
##   # Estimate the (residual) variance of the within-person centered variables.
##   wx1 ~~ wx1 # Variance
##   wy1 ~~ wy1
##   wx2 ~~ wx2 # Residual variance
##   wy2 ~~ wy2
##   wx3 ~~ wx3
##   wy3 ~~ wy3
##   wx4 ~~ wx4
##   wy4 ~~ wy4
##   wx5 ~~ wx5
##   wy5 ~~ wy5
## 
##   # Constrain the grand means over time.
##   x1 + x2 + x3 + x4 + x5 ~ mx*1
##   y1 + y2 + y3 + y4 + y5 ~ my*1
## '
## RICLPM3.fit <- lavaan(RICLPM3, data = dat, missing = 'ML', meanstructure = T, int.ov.free = T)
## summary(RICLPM3.fit, standardized = T)

## RICLPM5 <- '
##   # Create between components (random intercepts)
##   RIx =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5
##   RIy =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
## 
##   # Create within-person centered variables
##   wx1 =~ 1*x1
##   wx2 =~ 1*x2
##   wx3 =~ 1*x3
##   wx4 =~ 1*x4
##   wx5 =~ 1*x5
##   wy1 =~ 1*y1
##   wy2 =~ 1*y2
##   wy3 =~ 1*y3
##   wy4 =~ 1*y4
##   wy5 =~ 1*y5
## 
##   # Estimate the lagged effects between the within-person centered variables (constrained).
##   wx2 ~ a*wx1 + b*wy1
##   wy2 ~ c*wx1 + d*wy1
##   wx3 ~ a*wx2 + b*wy2
##   wy3 ~ c*wx2 + d*wy2
##   wx4 ~ a*wx3 + b*wy3
##   wy4 ~ c*wx3 + d*wy3
##   wx5 ~ a*wx4 + b*wy4
##   wy5 ~ c*wx4 + d*wy4
## 
##   # Estimate the covariances between the residuals of the within-person centered variables
##   # (the innovations, constrained).
##   wx2 ~~ cov*wy2
##   wx3 ~~ cov*wy3
##   wx4 ~~ cov*wy4
##   wx5 ~~ cov*wy5
## 
##   # Estimate the covariance between the within-person centered variables at the first wave.
##   wx1 ~~ wy1 # Covariance
## 
##   # Estimate the variance and covariance of the individual factors.
##   RIx ~~ RIx
##   RIy ~~ RIy
##   RIx ~~ RIy
## 
##   # Estimate the (residual) variance of the within-person centered variables (constrained).
##   wx1 ~~ wx1 # Variance
##   wy1 ~~ wy1
##   wx2 ~~ vx*wx2 # Residual variance
##   wy2 ~~ vy*wy2
##   wx3 ~~ vx*wx3
##   wy3 ~~ vy*wy3
##   wx4 ~~ vx*wx4
##   wy4 ~~ vy*wy4
##   wx5 ~~ vx*wx5
##   wy5 ~~ vy*wy5
## 
##   # Constrain the grand means over time.
##   x1 + x2 + x3 + x4 + x5 ~ mx*1
##   y1 + y2 + y3 + y4 + y5 ~ my*1
## '
## RICLPM5.fit <- lavaan(RICLPM5, data = dat, missing = 'ML', meanstructure = T, int.ov.free = T)
## summary(RICLPM5.fit, standardized = T)

## RICLPM.ext1 <- '
##   # Create between components (random intercepts)
##   RIx =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5
##   RIy =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
## 
##   # Create within-person centered variables
##   wx1 =~ 1*x1
##   wx2 =~ 1*x2
##   wx3 =~ 1*x3
##   wx4 =~ 1*x4
##   wx5 =~ 1*x5
##   wy1 =~ 1*y1
##   wy2 =~ 1*y2
##   wy3 =~ 1*y3
##   wy4 =~ 1*y4
##   wy5 =~ 1*y5
## 
##   # Regression of observed variables on z1 (constrained).
##   x1 + x2 + x3 + x4 + x5 ~ s1*z1 # Constrained over time.
##   y1 + y2 + y3 + y4 + y5 ~ s2*z1 # Constrained over time.
## 
##   # Estimate the lagged effects between the within-person centered variables.
##   wx2 + wy2 ~ wx1 + wy1
##   wx3 + wy3 ~ wx2 + wy2
##   wx4 + wy4 ~ wx3 + wy3
##   wx5 + wy5 ~ wx4 + wy4
## 
##   # Estimate the covariance between the within-person centered variables at the first wave.
##   wx1 ~~ wy1 # Covariance
## 
##   # Estimate the covariances between the residuals of the within-person centered variables (the innovations).
##   wx2 ~~ wy2
##   wx3 ~~ wy3
##   wx4 ~~ wy4
##   wx5 ~~ wy5
## 
##   # Estimate the variance and covariance of the individual factors.
##   RIx ~~ RIx
##   RIy ~~ RIy
##   RIx ~~ RIy
## 
##   # Estimate the (residual) variance of the within-person centered variables.
##   wx1 ~~ wx1 # Variances
##   wy1 ~~ wy1
##   wx2 ~~ wx2 # Residual variances
##   wy2 ~~ wy2
##   wx3 ~~ wx3
##   wy3 ~~ wy3
##   wx4 ~~ wx4
##   wy4 ~~ wy4
##   wx5 ~~ wx5
##   wy5 ~~ wy5
## '
## RICLPM.ext1.fit <- lavaan(RICLPM.ext1, data = datZ, missing = 'ML', meanstructure = T, int.ov.free = T)
## summary(RICLPM.ext1.fit, standardized = T)

## RICLPM1.ext1 <- '
##   # Create between components (random intercepts)
##   RIx =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5
##   RIy =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
## 
##   # Create within-person centered variables
##   wx1 =~ 1*x1
##   wx2 =~ 1*x2
##   wx3 =~ 1*x3
##   wx4 =~ 1*x4
##   wx5 =~ 1*x5
##   wy1 =~ 1*y1
##   wy2 =~ 1*y2
##   wy3 =~ 1*y3
##   wy4 =~ 1*y4
##   wy5 =~ 1*y5
## 
##   # Regression of random intercepts on z1.
##   RIx ~ z1
##   RIy ~ z1
## 
##   # Estimate the lagged effects between the within-person centered variables.
##   wx2 + wy2 ~ wx1 + wy1
##   wx3 + wy3 ~ wx2 + wy2
##   wx4 + wy4 ~ wx3 + wy3
##   wx5 + wy5 ~ wx4 + wy4
## 
##   # Estimate the covariance between the within-person centered variables at the first wave.
##   wx1 ~~ wy1 # Covariance
## 
##   # Estimate the covariances between the residuals of the within-person centered variables (the innovations).
##   wx2 ~~ wy2
##   wx3 ~~ wy3
##   wx4 ~~ wy4
##   wx5 ~~ wy5
## 
##   # Estimate the variance and covariance of the individual factors.
##   RIx ~~ RIx
##   RIy ~~ RIy
##   RIx ~~ RIy
## 
##   # Estimate the (residual) variance of the within-person centered variables.
##   wx1 ~~ wx1 # Variances
##   wy1 ~~ wy1
##   wx2 ~~ wx2 # Residual variances
##   wy2 ~~ wy2
##   wx3 ~~ wx3
##   wy3 ~~ wy3
##   wx4 ~~ wx4
##   wy4 ~~ wy4
##   wx5 ~~ wx5
##   wy5 ~~ wy5
## '
## RICLPM1.ext1.fit <- lavaan(RICLPM1.ext1, data = datZ, missing = 'ML', meanstructure = T, int.ov.free = T)
## summary(RICLPM1.ext1.fit, standardized = T)

## RICLPM.ext2 <- '
##   # Create between components (random intercepts)
##   RIx =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5
##   RIy =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
## 
##   # Create within-person centered variables
##   wx1 =~ 1*x1
##   wx2 =~ 1*x2
##   wx3 =~ 1*x3
##   wx4 =~ 1*x4
##   wx5 =~ 1*x5
##   wy1 =~ 1*y1
##   wy2 =~ 1*y2
##   wy3 =~ 1*y3
##   wy4 =~ 1*y4
##   wy5 =~ 1*y5
##   # Estimate the lagged effects between the within-person centered variables.
##   wx2 + wy2 ~ wx1 + wy1
##   wx3 + wy3 ~ wx2 + wy2
##   wx4 + wy4 ~ wx3 + wy3
##   wx5 + wy5 ~ wx4 + wy4
##   # Estimate the covariance between the within-person centered variables at the first wave.
##   wx1 ~~ wy1 # Covariance
## 
##   # Estimate the covariances between the residuals of the within-person centered variables (the innovations).
##   wx2 ~~ wy2
##   wx3 ~~ wy3
##   wx4 ~~ wy4
##   wx5 ~~ wy5
## 
##   # Estimate the variance and covariance of the individual factors.
##   RIx ~~ RIx
##   RIy ~~ RIy
##   RIx ~~ RIy
##   # Estimate the (residual) variance of the within-person centered variables.
##   wx1 ~~ wx1 # Variances
##   wy1 ~~ wy1
##   wx2 ~~ wx2 # Residual variances
##   wy2 ~~ wy2
##   wx3 ~~ wx3
##   wy3 ~~ wy3
##   wx4 ~~ wx4
##   wy4 ~~ wy4
##   wx5 ~~ wx5
##   wy5 ~~ wy5
## '
## RICLPM.ext2.fit <- lavaan(RICLPM.ext2, data = datMG, missing = 'ML', group = "G", meanstructure = T, int.ov.free = T)
## summary(RICLPM.ext2.fit)

## RICLPM1.ext2 <- '
##   # Create between components (random intercepts)
##   RIx =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5
##   RIy =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
## 
##   # Create within-person centered variables
##   wx1 =~ 1*x1
##   wx2 =~ 1*x2
##   wx3 =~ 1*x3
##   wx4 =~ 1*x4
##   wx5 =~ 1*x5
##   wy1 =~ 1*y1
##   wy2 =~ 1*y2
##   wy3 =~ 1*y3
##   wy4 =~ 1*y4
##   wy5 =~ 1*y5
## 
##   # Estimate the lagged effects between the within-person centered variables. Constrain the
##   # autoregressive effects across groups.
##   wx2 ~ c(a1, a1)*wx1 + c(b1, b1)*wy1
##   wy2 ~ c(c1, c1)*wx1 + c(d1, d1)*wy1
##   wx3 ~ c(a2, a2)*wx2 + c(b2, b2)*wy2
##   wy3 ~ c(c2, c2)*wx2 + c(d2, d2)*wy2
##   wx4 ~ c(a3, a3)*wx3 + c(b3, b3)*wy3
##   wy4 ~ c(c3, c3)*wx3 + c(d3, d3)*wy3
##   wx5 ~ c(a4, a4)*wx4 + c(b4, b4)*wy4
##   wy5 ~ c(c4, c4)*wx4 + c(d4, d4)*wy4
## 
##   # Estimate the covariance between the within-person centered variables at the first wave.
##   wx1 ~~ wy1 # Covariance
## 
##   # Estimate the covariances between the residuals of the within-person centered variables (the innovations).
##   wx2 ~~ wy2
##   wx3 ~~ wy3
##   wx4 ~~ wy4
##   wx5 ~~ wy5
## 
##   # Estimate the variance and covariance of the individual factors.
##   RIx ~~ RIx
##   RIy ~~ RIy
##   RIx ~~ RIy
## 
##   # Estimate the (residual) variance of the within-person centered variables.
##   wx1 ~~ wx1 # Variances
##   wy1 ~~ wy1
##   wx2 ~~ wx2 # Residual variances
##   wy2 ~~ wy2
##   wx3 ~~ wx3
##   wy3 ~~ wy3
##   wx4 ~~ wx4
##   wy4 ~~ wy4
##   wx5 ~~ wx5
##   wy5 ~~ wy5
## '
## RICLPM1.ext2.fit <- lavaan(RICLPM1.ext2, data = datMG, missing = 'ML', group = "G", meanstructure = T, int.ov.free = T)
## summary(RICLPM1.ext2.fit)

## RICLPM1.ext3 <- '
## 
##   ################
##   # BETWEEN PART #
##   ################
## 
##   # Create between factors (random intercepts) for each indicator separately.
##   RIX1 =~ 1*x11 + 1*x21 + 1*x31 + 1*x41 + 1*x51
##   RIX2 =~ 1*x12 + 1*x22 + 1*x32 + 1*x42 + 1*x52
##   RIX3 =~ 1*x13 + 1*x23 + 1*x33 + 1*x43 + 1*x53
## 
##   RIY1 =~ 1*y11 + 1*y21 + 1*y31 + 1*y41 + 1*y51
##   RIY2 =~ 1*y12 + 1*y22 + 1*y32 + 1*y42 + 1*y52
##   RIY3 =~ 1*y13 + 1*y23 + 1*y33 + 1*y43 + 1*y53
## 
##   ##################################
##   # WITHIN PART: MEASUREMENT MODEL #
##   ##################################
## 
##   # Factor models for x at 5 waves.
##   WFX1 =~ x11 + x12 + x13
##   WFX2 =~ x21 + x22 + x23
##   WFX3 =~ x31 + x32 + x33
##   WFX4 =~ x41 + x42 + x43
##   WFX5 =~ x51 + x52 + x53
## 
##   # Factor models for y at 5 waves.
##   WFY1 =~ y11 + y12 + y13
##   WFY2 =~ y21 + y22 + y23
##   WFY3 =~ y31 + y32 + y33
##   WFY4 =~ y41 + y42 + y43
##   WFY5 =~ y51 + y52 + y53
## 
##   #########################
##   # WITHIN PART: DYNAMICS #
##   #########################
## 
##   # Specify the lagged effects between the within-person centered latent variables.
##   WFX2 + WFY2 ~ WFX1 + WFY1
##   WFX3 + WFY3 ~ WFX2 + WFY2
##   WFX4 + WFY4 ~ WFX3 + WFY3
##   WFX5 + WFY5 ~ WFX4 + WFY4
## 
##   # Estimate the correlations within the same wave.
##   WFX1 ~~ WFY1
##   WFX2 ~~ WFY2
##   WFX3 ~~ WFY3
##   WFX4 ~~ WFY4
##   WFX5 ~~ WFY5
## 
##   ##########################
##   # ADDITIONAL CONSTRAINTS #
##   ##########################
## 
##   # Constrain covariance of the between factors and exogenous within factors to 0.
##   RIX1 + RIX2 + RIX3 + RIY1 + RIY2 + RIY3 ~~ 0*WFY1 + 0*WFX1
## '
## RICLPM1.ext3.fit <- cfa(RICLPM1.ext3, data = datMI, missing = 'ML')
## summary(RICLPM1.ext3.fit, standardized = T)

## RICLPM2.ext3 <- '
## 
##   ################
##   # BETWEEN PART #
##   ################
## 
##   # Create between factors (random intercepts) for each indicator separately.
##   RIX1 =~ 1*x11 + 1*x21 + 1*x31 + 1*x41 + 1*x51
##   RIX2 =~ 1*x12 + 1*x22 + 1*x32 + 1*x42 + 1*x52
##   RIX3 =~ 1*x13 + 1*x23 + 1*x33 + 1*x43 + 1*x53
## 
##   RIY1 =~ 1*y11 + 1*y21 + 1*y31 + 1*y41 + 1*y51
##   RIY2 =~ 1*y12 + 1*y22 + 1*y32 + 1*y42 + 1*y52
##   RIY3 =~ 1*y13 + 1*y23 + 1*y33 + 1*y43 + 1*y53
## 
##   ##################################
##   # WITHIN PART: MEASUREMENT MODEL #
##   ##################################
## 
##   # Factor models for x at 5 waves (constrained).
##   WFX1 =~ a*x11 + b*x12 + c*x13
##   WFX2 =~ a*x21 + b*x22 + c*x23
##   WFX3 =~ a*x31 + b*x32 + c*x33
##   WFX4 =~ a*x41 + b*x42 + c*x43
##   WFX5 =~ a*x51 + b*x52 + c*x53
## 
##   # Factor models for y at 5 waves (constrained).
##   WFY1 =~ d*y11 + e*y12 + f*y13
##   WFY2 =~ d*y21 + e*y22 + f*y23
##   WFY3 =~ d*y31 + e*y32 + f*y33
##   WFY4 =~ d*y41 + e*y42 + f*y43
##   WFY5 =~ d*y51 + e*y52 + f*y53
## 
##   #########################
##   # WITHIN PART: DYNAMICS #
##   #########################
## 
##   # Specify the lagged effects between the within-person centered latent variables.
##   WFX2 + WFY2 ~ WFX1 + WFY1
##   WFX3 + WFY3 ~ WFX2 + WFY2
##   WFX4 + WFY4 ~ WFX3 + WFY3
##   WFX5 + WFY5 ~ WFX4 + WFY4
## 
##   # Estimate the correlations within the same wave.
##   WFX1 ~~ WFY1
##   WFX2 ~~ WFY2
##   WFX3 ~~ WFY3
##   WFX4 ~~ WFY4
##   WFX5 ~~ WFY5
## 
##   ##########################
##   # ADDITIONAL CONSTRAINTS #
##   ##########################
## 
##   # Constrain covariance of the between factors and exogenous within factors to 0.
##   RIX1 + RIX2 + RIX3 + RIY1 + RIY2 + RIY3 ~~ 0*WFY1 + 0*WFX1
## '
## RICLPM2.ext3.fit <- cfa(RICLPM2.ext3, data = datMI, missing = 'ML')
## summary(RICLPM2.ext3.fit, standardized = T)

## RICLPM3.ext3 <- '
## 
##   ################
##   # BETWEEN PART #
##   ################
## 
##   # Create between factors (random intercepts) for each indicator separately.
##   RIX1 =~ 1*x11 + 1*x21 + 1*x31 + 1*x41 + 1*x51
##   RIX2 =~ 1*x12 + 1*x22 + 1*x32 + 1*x42 + 1*x52
##   RIX3 =~ 1*x13 + 1*x23 + 1*x33 + 1*x43 + 1*x53
## 
##   RIY1 =~ 1*y11 + 1*y21 + 1*y31 + 1*y41 + 1*y51
##   RIY2 =~ 1*y12 + 1*y22 + 1*y32 + 1*y42 + 1*y52
##   RIY3 =~ 1*y13 + 1*y23 + 1*y33 + 1*y43 + 1*y53
## 
##   ##################################
##   # WITHIN PART: MEASUREMENT MODEL #
##   ##################################
## 
##   # Factor models for x at 5 waves (constrained).
##   WFX1 =~ a*x11 + b*x12 + c*x13
##   WFX2 =~ a*x21 + b*x22 + c*x23
##   WFX3 =~ a*x31 + b*x32 + c*x33
##   WFX4 =~ a*x41 + b*x42 + c*x43
##   WFX5 =~ a*x51 + b*x52 + c*x53
## 
##   # Factor models for y at 5 waves (constrained).
##   WFY1 =~ d*y11 + e*y12 + f*y13
##   WFY2 =~ d*y21 + e*y22 + f*y23
##   WFY3 =~ d*y31 + e*y32 + f*y33
##   WFY4 =~ d*y41 + e*y42 + f*y43
##   WFY5 =~ d*y51 + e*y52 + f*y53
## 
##   # Constrained intercepts over time (this is necessary for strong factorial
##   # invariance; without these contraints we have week factorial invariance).
##   x11 + x21 + x31 + x41 + x51 ~ g*1
##   x12 + x22 + x32 + x42 + x52 ~ h*1
##   x13 + x23 + x33 + x43 + x53 ~ i*1
##   y11 + y21 + y31 + y41 + y51 ~ j*1
##   y12 + y22 + y32 + y42 + y52 ~ k*1
##   y13 + y23 + y33 + y43 + y53 ~ l*1
## 
##   # Free latent means from t = 2 onward (only do this in combination with the
##   # constraints on the intercepts; without these, this would not be specified).
##   WFX2 + WFX3 + WFX4 + WFX5 + WFY2 + WFY3 + WFY4 + WFY5 ~ 1
## 
##   #########################
##   # WITHIN PART: DYNAMICS #
##   #########################
## 
##   # Specify the lagged effects between the within-person centered latent variables.
##   WFX2 + WFY2 ~ WFX1 + WFY1
##   WFX3 + WFY3 ~ WFX2 + WFY2
##   WFX4 + WFY4 ~ WFX3 + WFY3
##   WFX5 + WFY5 ~ WFX4 + WFY4
## 
##   # Estimate the correlations within the same wave.
##   WFX1 ~~ WFY1
##   WFX2 ~~ WFY2
##   WFX3 ~~ WFY3
##   WFX4 ~~ WFY4
##   WFX5 ~~ WFY5
## 
##   ##########################
##   # ADDITIONAL CONSTRAINTS #
##   ##########################
## 
##   # Constrain covariance of the between factors and exogenous within factors to 0.
##   RIX1 + RIX2 + RIX3 + RIY1 + RIY2 + RIY3 ~~ 0*WFY1 + 0*WFX1
## '
## RICLPM3.ext3.fit <- cfa(RICLPM3.ext3, data = datMI, missing = 'ML')
## summary(RICLPM3.ext3.fit, standardized = T)

## RICLPM4.ext3 <- '
## 
##   ################
##   # BETWEEN PART #
##   ################
## 
##   # Create between factors (random intercepts) for each indicator separately.
##   RIX1 =~ 1*x11 + 1*x21 + 1*x31 + 1*x41 + 1*x51
##   RIX2 =~ 1*x12 + 1*x22 + 1*x32 + 1*x42 + 1*x52
##   RIX3 =~ 1*x13 + 1*x23 + 1*x33 + 1*x43 + 1*x53
## 
##   RIY1 =~ 1*y11 + 1*y21 + 1*y31 + 1*y41 + 1*y51
##   RIY2 =~ 1*y12 + 1*y22 + 1*y32 + 1*y42 + 1*y52
##   RIY3 =~ 1*y13 + 1*y23 + 1*y33 + 1*y43 + 1*y53
## 
##   # Create a single random intercept for al x variables, and another for all
##   # y variables and constrain the factor loadings to be identical to the
##   # within-person factor loadings.
## 
##   RIX =~ a*RIX1 + b*RIX2 + c*RIX3
##   RIY =~ d*RIY1 + e*RIY2 + f*RIY3
## 
##   # Add (co)variance between two higher-order random intercepts.
##   RIX ~~ RIY
##   RIX ~~ RIX
##   RIY ~~ RIY
## 
##   # Constrain measurement error variances of the second order factor model to 0.
##   RIX1 ~~ 0*RIX1
##   RIX2 ~~ 0*RIX2
##   RIX3 ~~ 0*RIX3
##   RIY1 ~~ 0*RIY1
##   RIY2 ~~ 0*RIY2
##   RIY3 ~~ 0*RIY3
## 
##   ##################################
##   # WITHIN PART: MEASUREMENT MODEL #
##   ##################################
## 
##   # Factor models for x at 5 waves (constrained).
##   WFX1 =~ a*x11 + b*x12 + c*x13
##   WFX2 =~ a*x21 + b*x22 + c*x23
##   WFX3 =~ a*x31 + b*x32 + c*x33
##   WFX4 =~ a*x41 + b*x42 + c*x43
##   WFX5 =~ a*x51 + b*x52 + c*x53
## 
##   # Factor models for y at 5 waves (constrained).
##   WFY1 =~ d*y11 + e*y12 + f*y13
##   WFY2 =~ d*y21 + e*y22 + f*y23
##   WFY3 =~ d*y31 + e*y32 + f*y33
##   WFY4 =~ d*y41 + e*y42 + f*y43
##   WFY5 =~ d*y51 + e*y52 + f*y53
## 
##   # Constrained intercepts over time (this is necessary for strong factorial
##   # invariance; without these contraints we have week factorial invariance).
##   x11 + x21 + x31 + x41 + x51 ~ g*1
##   x12 + x22 + x32 + x42 + x52 ~ h*1
##   x13 + x23 + x33 + x43 + x53 ~ i*1
##   y11 + y21 + y31 + y41 + y51 ~ j*1
##   y12 + y22 + y32 + y42 + y52 ~ k*1
##   y13 + y23 + y33 + y43 + y53 ~ l*1
## 
##   # Free latent means from t = 2 onward (only do this in combination with the
##   # constraints on the intercepts; without these, this would not be specified).
##   WFX2 + WFX3 + WFX4 + WFX5 + WFY2 + WFY3 + WFY4 + WFY5 ~ 1
## 
##   #########################
##   # WITHIN PART: DYNAMICS #
##   #########################
## 
##   # Specify the lagged effects between the within-person centered latent variables.
##   WFX2 + WFY2 ~ WFX1 + WFY1
##   WFX3 + WFY3 ~ WFX2 + WFY2
##   WFX4 + WFY4 ~ WFX3 + WFY3
##   WFX5 + WFY5 ~ WFX4 + WFY4
## 
##   # Estimate the correlations within the same wave.
##   WFX1 ~~ WFY1
##   WFX2 ~~ WFY2
##   WFX3 ~~ WFY3
##   WFX4 ~~ WFY4
##   WFX5 ~~ WFY5
## 
##   ##########################
##   # ADDITIONAL CONSTRAINTS #
##   ##########################
## 
##   # Constrain covariance of the between factors and exogenous within factors to 0.
##   RIX + RIY + RIX1 + RIX2 + RIX3 + RIY1 + RIY2 + RIY3 ~~ 0*WFY1 + 0*WFX1
## '
## RICLPM4.ext3.fit <- cfa(RICLPM4.ext3, data = datMI, missing = 'ML')
## summary(RICLPM4.ext3.fit, standardized = T)

## RICLPM5.ext3 <- '
## 
##   #####################
##   # MEASUREMENT MODEL #
##   #####################
## 
##   # Factor models for x at 5 waves (constrained).
##   FX1 =~ a*x11 + b*x12 + c*x13
##   FX2 =~ a*x21 + b*x22 + c*x23
##   FX3 =~ a*x31 + b*x32 + c*x33
##   FX4 =~ a*x41 + b*x42 + c*x43
##   FX5 =~ a*x51 + b*x52 + c*x53
## 
##   # Factor models for y at 5 waves (constrained).
##   FY1 =~ d*y11 + e*y12 + f*y13
##   FY2 =~ d*y21 + e*y22 + f*y23
##   FY3 =~ d*y31 + e*y32 + f*y33
##   FY4 =~ d*y41 + e*y42 + f*y43
##   FY5 =~ d*y51 + e*y52 + f*y53
## 
##   # Constrained intercepts over time (this is necessary for strong factorial
##   # invariance; without these contraints we have week factorial invariance).
##   x11 + x21 + x31 + x41 + x51 ~ g*1
##   x12 + x22 + x32 + x42 + x52 ~ h*1
##   x13 + x23 + x33 + x43 + x53 ~ i*1
##   y11 + y21 + y31 + y41 + y51 ~ j*1
##   y12 + y22 + y32 + y42 + y52 ~ k*1
##   y13 + y23 + y33 + y43 + y53 ~ l*1
## 
##   # Free latent means from t = 2 onward (only do this in combination with the
##   # constraints on the intercepts; without these, this would not be specified).
##   FX2 + FX3 + FX4 + FX5 + FY2 + FY3 + FY4 + FY5 ~ 1
## 
##   ################
##   # BETWEEN PART #
##   ################
## 
##   # Create between factors (random intercepts).
##   RIX =~ 1*FX1 + 1*FX2 + 1*FX3 + 1*FX4 + 1*FX5
##   RIY =~ 1*FY1 + 1*FY2 + 1*FY3 + 1*FY4 + 1*FY5
##   # Set the residual variances of all FX and FY variables to 0.
##   FX1 ~~ 0*FX1
##   FX2 ~~ 0*FX2
##   FX3 ~~ 0*FX3
##   FX4 ~~ 0*FX4
##   FX5 ~~ 0*FX5
##   FY1 ~~ 0*FY1
##   FY2 ~~ 0*FY2
##   FY3 ~~ 0*FY3
##   FY4 ~~ 0*FY4
##   FY5 ~~ 0*FY5
## 
##   ###############
##   # WITHIN PART #
##   ###############
## 
##   # Create the within-part.
##   WFX1 =~ 1*FX1
##   WFX2 =~ 1*FX2
##   WFX3 =~ 1*FX3
##   WFX4 =~ 1*FX4
##   WFX5 =~ 1*FX5
## 
##   WFY1 =~ 1*FY1
##   WFY2 =~ 1*FY2
##   WFY3 =~ 1*FY3
##   WFY4 =~ 1*FY4
##   WFY5 =~ 1*FY5
## 
##   # Specify the lagged effects between the within-person centered latent variables.
##   WFX2 + WFY2 ~ WFX1 + WFY1
##   WFX3 + WFY3 ~ WFX2 + WFY2
##   WFX4 + WFY4 ~ WFX3 + WFY3
##   WFX5 + WFY5 ~ WFX4 + WFY4
## 
##   # Estimate the correlations within the same wave.
##   WFX2 ~~ WFY2
##   WFX3 ~~ WFY3
##   WFX4 ~~ WFY4
##   WFX5 ~~ WFY5
## 
##   ##########################
##   # ADDITIONAL CONSTRAINTS #
##   ##########################
## 
##   # Set correlations between the between-factors (random intercepts) and within-
##   # factors at wave 1 at 0.
##   RIX + RIY ~~ 0*WFX1 + 0*WFY1
## '
## RICLPM5.ext3.fit <- cfa(RICLPM5.ext3, data = datMI, missing = 'ML')
## summary(RICLPM5.ext3.fit, standardized = T)

## # Install and load the required packages.
## ## library(devtools)
## ## install_github("rebeccakuiper/ChiBarSq.DiffTest")
## library(ChiBarSq.DiffTest)
## 
## # Step 1
## ## Fit the RI-CLPM (RICLPM.fit) and the CLPM (CLPM.fit).
## 
## # Step 2
## ## Check which indices you need to get the covariance matrix of the random intercepts.
## vcov(RICLPM.fit) # Full covariance matrix
## 
## ## The 22nd and the 23rd indices regard the random intercepts
## indices <- c(22, 23)
## q <- length(indices) # Number of random intercepts
## S <- vcov(RICLPM.fit)[indices, indices] # Extract full covariance matrix of the random intercepts
## 
## # Step 3
## Chi2_clpm <- summary(CLPM.fit, fit.measures = TRUE)[1]$FIT[c("chisq")] # Extract chi-square value of CLPM
## Chi2_riclpm <- summary(RICLPM.fit, fit.measures = TRUE)[1]$FIT[c("chisq")] # Extract chi-square value of RI-CLPM
## 
## df_clpm <- summary(CLPM.fit, fit.measures = TRUE)[1]$FIT[c("df")] # Extract df of CLPM
## df_riclpm <- summary(RICLPM.fit, fit.measures = TRUE)[1]$FIT[c("df")] # Extract df of RI-CLPM
## 
## # Step 4
## ## Run function to do chi-bar-square test (and also obtain Chi-bar-square weigths)
## ChiBar2DiffTest <- ChiBarSq.DiffTest(q, S, Chi2_clpm, Chi2_riclpm, df_clpm, df_riclpm)
## ChiBar2DiffTest
## ChiBar2DiffTest$p_value
