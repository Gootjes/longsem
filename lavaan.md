---
title: "Using lavaan (R)"
output: 
  html_document:
    toc:  true
    toc_float: true
    code_folding: show
bibliography: references.bib
---

---

## Setup
Below you can find the code for installing and loading the required package `lavaan` [@lavaan], as well as for reading in the data for the Random Intercept Cross-Lagged Panel Model (RI-CLPM) and its 3 extensions. You can specify the path to the data yourself, or through a menu by using the `file.choose()`-function. You can download the simulated example datasets [here](https://github.com/ellenhamaker/RI-CLPM/tree/master/data). 


```r
# If necessary, install the 'Lavaan' package. 
# install.packages('lavaan', dependencies = T)
# Load the required packages. 
require(lavaan) 

# Load in the data. 
## Traditional RI-CLPM
dat <- read.table("/Users/jeroenmulder/Git/RICLPM/data/RICLPM.dat", 
                  col.names = c("x1", "x2", "x3", "x4", "x5", "y1", "y2", "y3", "y4", "y5")) 
## Extension 1
datZ <- read.table("/Users/jeroenmulder/Git/RICLPM/data/RICLPM-Z.dat",
                   col.names = c("x1", "x2", "x3", "x4", "x5", "y1", "y2", "y3", "y4", "y5", "z2", "z1"))
## Extension 2
datMG <- read.table("/Users/jeroenmulder/Git/RICLPM/data/RICLPM-MG.dat",
                    col.names = c("x1", "x2", "x3", "x4", "x5", "y1", "y2", "y3", "y4", "y5", "G")) 
## Extension 3
datMI <- read.table("/Users/jeroenmulder/Git/RICLPM/data/RICLPM-MI.dat",
                   col.names = c("x11", "x12", "x13",
                                 "x21", "x22", "x23",
                                 "x31", "x32", "x33",
                                 "x41", "x42", "x43",
                                 "x51", "x52", "x53", 
                                
                                 "y11", "y12", "y13",
                                 "y21", "y22", "y23",
                                 "y31", "y32", "y33", 
                                 "y41", "y42", "y43", 
                                 "y51", "y52", "y53"))
```

---

## The RI-CLPM {.tabset .tabset-fade}
To specify the RI-CLPM we need four parts. 

* A *between part*, consisting of the random intercepts. It is specified  using the `=~` command, `RIx =~ 1*x1 1*x2 ...`, where `1*` fixes the factor loading to one.
* A *within part*, consisting of within-unit fluctuations. It is also specified using the `=~` command, `wx1 =~ 1*x1; wx2 =~ 1*x2; ...`. 
* The *lagged regressions* between the within-unit components, using `wx2 ~ wx1 wy1; wx3 ~ wx2 wy2; ...`. 
* Relevant *covariances* in both the between and within part. In the within part the components at wave 1, and their residuals at waves 2 and further are correlated within each wave, using `wx1 ~~ wy1; wx2 ~~ wy2;...`. We also need to specify their (residual) variances here using `wx1 ~~ wx1; wx2 ~~ wx2; ...`. For the between part we have to specify the variances and covariance of the random intercepts using `RIx ~~ RIy;`. 

### The basic model
The code for specifying the basic RI-CLPM is given below. 


```r
RICLPM <- '
  # Create between components (random intercepts)
  RIx =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5
  RIy =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
  
  # Create within-person centered variables
  wx1 =~ 1*x1
  wx2 =~ 1*x2
  wx3 =~ 1*x3 
  wx4 =~ 1*x4
  wx5 =~ 1*x5
  wy1 =~ 1*y1
  wy2 =~ 1*y2
  wy3 =~ 1*y3
  wy4 =~ 1*y4
  wy5 =~ 1*y5

  # Estimate the lagged effects between the within-person centered variables.
  wx2 + wy2 ~ wx1 + wy1
  wx3 + wy3 ~ wx2 + wy2
  wx4 + wy4 ~ wx3 + wy3
  wx5 + wy5 ~ wx4 + wy4

  # Estimate the covariance between the within-person centered variables at the first wave. 
  wx1 ~~ wy1 # Covariance
  
  # Estimate the covariances between the residuals of the within-person centered variables (the innovations).
  wx2 ~~ wy2
  wx3 ~~ wy3
  wx4 ~~ wy4
  wx5 ~~ wy5
  
  # Estimate the variance and covariance of the individual factors. 
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy

  # Estimate the (residual) variance of the within-person centered variables.
  wx1 ~~ wx1 # Variances
  wy1 ~~ wy1 
  wx2 ~~ wx2 # Residual variances
  wy2 ~~ wy2 
  wx3 ~~ wx3 
  wy3 ~~ wy3 
  wx4 ~~ wx4 
  wy4 ~~ wy4 
  wx5 ~~ wx5
  wy5 ~~ wy5
'
RICLPM.fit <- lavaan(RICLPM, data = dat, missing = 'ML', meanstructure = T, int.ov.free = T) 
summary(RICLPM.fit, standardized = T)
```


```r
CLPM <- '
  # Estimate the lagged effects between the observed variables.
  x2 + y2 ~ x1 + y1
  x3 + y3 ~ x2 + y2
  x4 + y4 ~ x3 + y3
  x5 + y5 ~ x4 + y4

  # Estimate the covariance between the observed variables at the first wave. 
  x1 ~~ y1 # Covariance
  
  # Estimate the covariances between the residuals of the observed variables.
  x2 ~~ y2
  x3 ~~ y3
  x4 ~~ y4
  x5 ~~ y5
  
  # Estimate the (residual) variance of the observed variables.
  x1 ~~ x1 # Variances
  y1 ~~ y1 
  x2 ~~ x2 # Residual variances
  y2 ~~ y2 
  x3 ~~ x3 
  y3 ~~ y3 
  x4 ~~ x4 
  y4 ~~ y4 
  x5 ~~ x5
  y5 ~~ y5
'
CLPM.fit <- lavaan(CLPM, data = dat, missing = 'ML', meanstructure = T, int.ov.free = T) 
summary(CLPM.fit, standardized = T)
```
---

### Constraints over time
Imposing constraints to the model can be achieved through **pre-multiplication**. It means that we have to prepend the number that we want to fix the parameter to, and an asterisk, to the parameter in the model specification. For example, `F =~ 0*x1` fixes the factor loading of item `x1` to factor `F` to 0. Using pre-multiplication we can also constrain parameters to be the same by giving them the same label. Below we specify a RI-CLPM with the following constraints: 

1) fixed auto-regressive and cross-lagged relations over time, `wx2 ~ a*wx1 + b*wy1; ...`, 
2) time-invariant (residual) (co-)variances in the within-person part `wx2 ~~ cov*wy2; ...`, `wx2 ~~ vx*wx2; ...`, and `wy2 ~~ vy*wy2; ...`, and 
3) constrained grand means over time, `x1 + ... ~ mx*1` and `y1 + ... ~ my*1`. 









```r
RICLPM5 <- '
  # Create between components (random intercepts)
  RIx =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5
  RIy =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
  
  # Create within-person centered variables
  wx1 =~ 1*x1
  wx2 =~ 1*x2
  wx3 =~ 1*x3 
  wx4 =~ 1*x4
  wx5 =~ 1*x5
  wy1 =~ 1*y1
  wy2 =~ 1*y2
  wy3 =~ 1*y3
  wy4 =~ 1*y4
  wy5 =~ 1*y5
  
  # Estimate the lagged effects between the within-person centered variables (constrained).
  wx2 ~ a*wx1 + b*wy1 
  wy2 ~ c*wx1 + d*wy1
  wx3 ~ a*wx2 + b*wy2
  wy3 ~ c*wx2 + d*wy2
  wx4 ~ a*wx3 + b*wy3
  wy4 ~ c*wx3 + d*wy3
  wx5 ~ a*wx4 + b*wy4
  wy5 ~ c*wx4 + d*wy4
  
  # Estimate the covariances between the residuals of the within-person centered variables 
  # (the innovations, constrained).
  wx2 ~~ cov*wy2
  wx3 ~~ cov*wy3
  wx4 ~~ cov*wy4 
  wx5 ~~ cov*wy5
  
  # Estimate the covariance between the within-person centered variables at the first wave. 
  wx1 ~~ wy1 # Covariance
  
  # Estimate the variance and covariance of the individual factors. 
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy
  
  # Estimate the (residual) variance of the within-person centered variables (constrained).
  wx1 ~~ wx1 # Variance
  wy1 ~~ wy1 
  wx2 ~~ vx*wx2 # Residual variance
  wy2 ~~ vy*wy2 
  wx3 ~~ vx*wx3 
  wy3 ~~ vy*wy3 
  wx4 ~~ vx*wx4 
  wy4 ~~ vy*wy4 
  wx5 ~~ vx*wx5
  wy5 ~~ vy*wy5
  
  # Constrain the grand means over time. 
  x1 + x2 + x3 + x4 + x5 ~ mx*1
  y1 + y2 + y3 + y4 + y5 ~ my*1
'
RICLPM5.fit <- lavaan(RICLPM5, data = dat, missing = 'ML', meanstructure = T, int.ov.free = T) 
summary(RICLPM5.fit, standardized = T)
```

---

## Ext. 1: time-invariant predictor {.tabset .tabset-fade}
Use the tabs below to navigate to the model specification of the RI-CLPM with

* a time-invariant predictor $z_{1}$ of the observed variables (constrained),
* a time-invariant predictor $z_{1}$ of the random intercepts,
* random intercepts predicting a time-invariant outcome $z_{2}$, or
* within components predicting a time-invariant outcome $z_{2}$.

### $z_{1}$ predicting observed
Below you can find the code for a RI-CLPM with 5 waves and a time-invariant predictor $z_{1}$ for the observed variables. The effect of $z_{1}$ on the observed variables is constrained to be the same across waves.


```r
RICLPM.ext1 <- '
  # Create between components (random intercepts)
  RIx =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5
  RIy =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
  
  # Create within-person centered variables
  wx1 =~ 1*x1
  wx2 =~ 1*x2
  wx3 =~ 1*x3 
  wx4 =~ 1*x4
  wx5 =~ 1*x5
  wy1 =~ 1*y1
  wy2 =~ 1*y2
  wy3 =~ 1*y3
  wy4 =~ 1*y4
  wy5 =~ 1*y5
  
  # Regression of observed variables on z1 (constrained). 
  x1 + x2 + x3 + x4 + x5 ~ s1*z1 # Constrained over time. 
  y1 + y2 + y3 + y4 + y5 ~ s2*z1 # Constrained over time. 
  
  # Estimate the lagged effects between the within-person centered variables.
  wx2 + wy2 ~ wx1 + wy1
  wx3 + wy3 ~ wx2 + wy2
  wx4 + wy4 ~ wx3 + wy3
  wx5 + wy5 ~ wx4 + wy4
  
  # Estimate the covariance between the within-person centered variables at the first wave. 
  wx1 ~~ wy1 # Covariance
  
  # Estimate the covariances between the residuals of the within-person centered variables (the innovations).
  wx2 ~~ wy2
  wx3 ~~ wy3
  wx4 ~~ wy4
  wx5 ~~ wy5
  
  # Estimate the variance and covariance of the individual factors. 
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy
  
  # Estimate the (residual) variance of the within-person centered variables.
  wx1 ~~ wx1 # Variances
  wy1 ~~ wy1 
  wx2 ~~ wx2 # Residual variances
  wy2 ~~ wy2 
  wx3 ~~ wx3 
  wy3 ~~ wy3 
  wx4 ~~ wx4 
  wy4 ~~ wy4 
  wx5 ~~ wx5
  wy5 ~~ wy5
'
RICLPM.ext1.fit <- lavaan(RICLPM.ext1, data = datZ, missing = 'ML', meanstructure = T, int.ov.free = T) 
summary(RICLPM.ext1.fit, standardized = T)
```

---

### $z_{1}$ predicting RIs
Below you can find the code for a RI-CLPM with 5 waves and a time-invariant predictor $z_{1}$ for the random intercepts. 

```r
RICLPM1.ext1 <- '
  # Create between components (random intercepts)
  RIx =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5
  RIy =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
  
  # Create within-person centered variables
  wx1 =~ 1*x1
  wx2 =~ 1*x2
  wx3 =~ 1*x3 
  wx4 =~ 1*x4
  wx5 =~ 1*x5
  wy1 =~ 1*y1
  wy2 =~ 1*y2
  wy3 =~ 1*y3
  wy4 =~ 1*y4
  wy5 =~ 1*y5
  
  # Regression of random intercepts on z1. 
  RIx ~ z1 
  RIy ~ z1 
  
  # Estimate the lagged effects between the within-person centered variables.
  wx2 + wy2 ~ wx1 + wy1
  wx3 + wy3 ~ wx2 + wy2
  wx4 + wy4 ~ wx3 + wy3
  wx5 + wy5 ~ wx4 + wy4
  
  # Estimate the covariance between the within-person centered variables at the first wave. 
  wx1 ~~ wy1 # Covariance
  
  # Estimate the covariances between the residuals of the within-person centered variables (the innovations).
  wx2 ~~ wy2
  wx3 ~~ wy3
  wx4 ~~ wy4
  wx5 ~~ wy5
  
  # Estimate the variance and covariance of the individual factors. 
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy
  
  # Estimate the (residual) variance of the within-person centered variables.
  wx1 ~~ wx1 # Variances
  wy1 ~~ wy1 
  wx2 ~~ wx2 # Residual variances
  wy2 ~~ wy2 
  wx3 ~~ wx3 
  wy3 ~~ wy3 
  wx4 ~~ wx4 
  wy4 ~~ wy4 
  wx5 ~~ wx5
  wy5 ~~ wy5
'
RICLPM1.ext1.fit <- lavaan(RICLPM1.ext1, data = datZ, missing = 'ML', meanstructure = T, int.ov.free = T) 
summary(RICLPM1.ext1.fit, standardized = T)
```

---

### RI's predicting $z_{2}$
*lavaan* code will arrive soon for this model.


### Within predicting $z_{2}$
*lavaan* code will arrive soon for this model.

## Ext. 2: multiple group {.tabset .tabset-fade}
Use the tabs below to navigate to the model specification of the basic multiple-group model, or the model with constrained lagged parameters (and intercepts across groups). 

### The basic model
Below you can find the code for a multiple group RI-CLPM with 5 waves.


```r
RICLPM.ext2 <- '
  # Create between components (random intercepts)
  RIx =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5
  RIy =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
  
  # Create within-person centered variables
  wx1 =~ 1*x1
  wx2 =~ 1*x2
  wx3 =~ 1*x3 
  wx4 =~ 1*x4
  wx5 =~ 1*x5
  wy1 =~ 1*y1
  wy2 =~ 1*y2
  wy3 =~ 1*y3
  wy4 =~ 1*y4
  wy5 =~ 1*y5
  # Estimate the lagged effects between the within-person centered variables.
  wx2 + wy2 ~ wx1 + wy1
  wx3 + wy3 ~ wx2 + wy2
  wx4 + wy4 ~ wx3 + wy3
  wx5 + wy5 ~ wx4 + wy4
  # Estimate the covariance between the within-person centered variables at the first wave. 
  wx1 ~~ wy1 # Covariance
  
  # Estimate the covariances between the residuals of the within-person centered variables (the innovations).
  wx2 ~~ wy2
  wx3 ~~ wy3
  wx4 ~~ wy4
  wx5 ~~ wy5
  
  # Estimate the variance and covariance of the individual factors. 
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy
  # Estimate the (residual) variance of the within-person centered variables.
  wx1 ~~ wx1 # Variances
  wy1 ~~ wy1 
  wx2 ~~ wx2 # Residual variances
  wy2 ~~ wy2 
  wx3 ~~ wx3 
  wy3 ~~ wy3 
  wx4 ~~ wx4 
  wy4 ~~ wy4 
  wx5 ~~ wx5
  wy5 ~~ wy5
'
RICLPM.ext2.fit <- lavaan(RICLPM.ext2, data = datMG, missing = 'ML', group = "G", meanstructure = T, int.ov.free = T)
summary(RICLPM.ext2.fit)
```

---

### Constrained lagged-parameters
Below you can find the code for a multiple group RI-CLPM with 4 waves. The lagged-parameters are constrained to be equal over time.


```r
RICLPM1.ext2 <- '
  # Create between components (random intercepts)
  RIx =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5
  RIy =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
  
  # Create within-person centered variables
  wx1 =~ 1*x1
  wx2 =~ 1*x2
  wx3 =~ 1*x3 
  wx4 =~ 1*x4
  wx5 =~ 1*x5
  wy1 =~ 1*y1
  wy2 =~ 1*y2
  wy3 =~ 1*y3
  wy4 =~ 1*y4
  wy5 =~ 1*y5
  
  # Estimate the lagged effects between the within-person centered variables. Constrain the  
  # autoregressive effects across groups. 
  wx2 ~ c(a1, a1)*wx1 + c(b1, b1)*wy1
  wy2 ~ c(c1, c1)*wx1 + c(d1, d1)*wy1
  wx3 ~ c(a2, a2)*wx2 + c(b2, b2)*wy2
  wy3 ~ c(c2, c2)*wx2 + c(d2, d2)*wy2
  wx4 ~ c(a3, a3)*wx3 + c(b3, b3)*wy3
  wy4 ~ c(c3, c3)*wx3 + c(d3, d3)*wy3 
  wx5 ~ c(a4, a4)*wx4 + c(b4, b4)*wy4
  wy5 ~ c(c4, c4)*wx4 + c(d4, d4)*wy4
  
  # Estimate the covariance between the within-person centered variables at the first wave. 
  wx1 ~~ wy1 # Covariance
  
  # Estimate the covariances between the residuals of the within-person centered variables (the innovations).
  wx2 ~~ wy2
  wx3 ~~ wy3
  wx4 ~~ wy4
  wx5 ~~ wy5
  
  # Estimate the variance and covariance of the individual factors. 
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy
  
  # Estimate the (residual) variance of the within-person centered variables.
  wx1 ~~ wx1 # Variances
  wy1 ~~ wy1 
  wx2 ~~ wx2 # Residual variances
  wy2 ~~ wy2 
  wx3 ~~ wx3 
  wy3 ~~ wy3 
  wx4 ~~ wx4 
  wy4 ~~ wy4 
  wx5 ~~ wx5
  wy5 ~~ wy5
'
RICLPM1.ext2.fit <- lavaan(RICLPM1.ext2, data = datMG, missing = 'ML', group = "G", meanstructure = T, int.ov.free = T)
summary(RICLPM1.ext2.fit)
```

---

## Ext. 3: multiple indicator {.tabset .tabset-fade}
Use the tabs below to navigate to the model specification of a multiple indicator RI-CLPM, 5 waves and 3 indicators for each variable at each wave. The five steps correspond to:

- the configural model (Step 1),
- weak factorial invariance (Step 2),
- strong factorial invariance (Step 3),
- strong factorial invariance with factor loadings equal to the within-person factor loadings (Extra), and
- the latent RI-CLPM (Step 4).

You can download the simulated example dataset *MIRICLPM.dat* [here](https://jeroendmulder.github.io/RI-CLPM/data/MIRICLPM.dat).

### Step 1
When we have three indicators $X$, measured at five waves, we specify three random intercepts to capture the trait-like part of each indicator, that is, `RIX1 =~ 1*x11 1*x21 ...`, `RIX2 =~ 1*x121 1*x22@1 ...`, and `RIX3 =~ 1*x13 1*x23 ...`. In addition, we specify five within-unit components that capture the state-like part at each wave, using `WFX1 =~ x11 x12 x13; WFX2 =~ x21 x22 x23; ...`.

At the latent within-unit level, we specify the dynamic model in Mplus using `WFX2 ~ WFY1 + WFX1; WFX3 ~ WFY2 + WFX2; ...`. In addition, we allow the within-person factors at the first wave, and their residuals at subsequent waves to be correlated within each wave, `WFX1 ~~ WFY1; WFX2 ~~ WFY2; ...`. The six random intercepts are allowed to be freely correlated with each other through `RIX1 + RIX2 + ... ~~ RIX1 + RIX2 + ...`. 


```r
RICLPM1.ext3 <- '
  
  ################
  # BETWEEN PART #
  ################
  
  # Create between factors (random intercepts) for each indicator separately.
  RIX1 =~ 1*x11 + 1*x21 + 1*x31 + 1*x41 + 1*x51
  RIX2 =~ 1*x12 + 1*x22 + 1*x32 + 1*x42 + 1*x52
  RIX3 =~ 1*x13 + 1*x23 + 1*x33 + 1*x43 + 1*x53
  
  RIY1 =~ 1*y11 + 1*y21 + 1*y31 + 1*y41 + 1*y51
  RIY2 =~ 1*y12 + 1*y22 + 1*y32 + 1*y42 + 1*y52
  RIY3 =~ 1*y13 + 1*y23 + 1*y33 + 1*y43 + 1*y53
  
  ##################################
  # WITHIN PART: MEASUREMENT MODEL #
  ##################################
  
  # Factor models for x at 5 waves.
  WFX1 =~ x11 + x12 + x13
  WFX2 =~ x21 + x22 + x23
  WFX3 =~ x31 + x32 + x33
  WFX4 =~ x41 + x42 + x43
  WFX5 =~ x51 + x52 + x53
  
  # Factor models for y at 5 waves.
  WFY1 =~ y11 + y12 + y13
  WFY2 =~ y21 + y22 + y23
  WFY3 =~ y31 + y32 + y33
  WFY4 =~ y41 + y42 + y43
  WFY5 =~ y51 + y52 + y53
  
  #########################
  # WITHIN PART: DYNAMICS #
  #########################
  
  # Specify the lagged effects between the within-person centered latent variables.
  WFX2 + WFY2 ~ WFX1 + WFY1
  WFX3 + WFY3 ~ WFX2 + WFY2
  WFX4 + WFY4 ~ WFX3 + WFY3
  WFX5 + WFY5 ~ WFX4 + WFY4
  
  # Estimate the correlations within the same wave.
  WFX1 ~~ WFY1
  WFX2 ~~ WFY2
  WFX3 ~~ WFY3 
  WFX4 ~~ WFY4
  WFX5 ~~ WFY5
  
  ##########################
  # ADDITIONAL CONSTRAINTS #
  ##########################
  
  # Constrain covariance of the between factors and exogenous within factors to 0. 
  RIX1 + RIX2 + RIX3 + RIY1 + RIY2 + RIY3 ~~ 0*WFY1 + 0*WFX1
'
RICLPM1.ext3.fit <- cfa(RICLPM1.ext3, data = datMI, missing = 'ML')
summary(RICLPM1.ext3.fit, standardized = T)
```

---

### Step 2
In second step, we constrain the factor loadings to be invariant over time using the labels `a*`, `b*`, etc.


```r
RICLPM2.ext3 <- '
  
  ################
  # BETWEEN PART #
  ################
  
  # Create between factors (random intercepts) for each indicator separately.
  RIX1 =~ 1*x11 + 1*x21 + 1*x31 + 1*x41 + 1*x51
  RIX2 =~ 1*x12 + 1*x22 + 1*x32 + 1*x42 + 1*x52
  RIX3 =~ 1*x13 + 1*x23 + 1*x33 + 1*x43 + 1*x53
  
  RIY1 =~ 1*y11 + 1*y21 + 1*y31 + 1*y41 + 1*y51
  RIY2 =~ 1*y12 + 1*y22 + 1*y32 + 1*y42 + 1*y52
  RIY3 =~ 1*y13 + 1*y23 + 1*y33 + 1*y43 + 1*y53
  
  ##################################
  # WITHIN PART: MEASUREMENT MODEL #
  ##################################
  
  # Factor models for x at 5 waves (constrained).
  WFX1 =~ a*x11 + b*x12 + c*x13
  WFX2 =~ a*x21 + b*x22 + c*x23
  WFX3 =~ a*x31 + b*x32 + c*x33
  WFX4 =~ a*x41 + b*x42 + c*x43
  WFX5 =~ a*x51 + b*x52 + c*x53
  
  # Factor models for y at 5 waves (constrained).
  WFY1 =~ d*y11 + e*y12 + f*y13
  WFY2 =~ d*y21 + e*y22 + f*y23
  WFY3 =~ d*y31 + e*y32 + f*y33
  WFY4 =~ d*y41 + e*y42 + f*y43
  WFY5 =~ d*y51 + e*y52 + f*y53
  
  #########################
  # WITHIN PART: DYNAMICS #
  #########################
  
  # Specify the lagged effects between the within-person centered latent variables.
  WFX2 + WFY2 ~ WFX1 + WFY1
  WFX3 + WFY3 ~ WFX2 + WFY2
  WFX4 + WFY4 ~ WFX3 + WFY3
  WFX5 + WFY5 ~ WFX4 + WFY4
  
  # Estimate the correlations within the same wave.
  WFX1 ~~ WFY1
  WFX2 ~~ WFY2
  WFX3 ~~ WFY3 
  WFX4 ~~ WFY4
  WFX5 ~~ WFY5
  
  ##########################
  # ADDITIONAL CONSTRAINTS #
  ##########################
  
  # Constrain covariance of the between factors and exogenous within factors to 0. 
  RIX1 + RIX2 + RIX3 + RIY1 + RIY2 + RIY3 ~~ 0*WFY1 + 0*WFX1
'
RICLPM2.ext3.fit <- cfa(RICLPM2.ext3, data = datMI, missing = 'ML')
summary(RICLPM2.ext3.fit, standardized = T)
```

---

### Step 3
Multiple indicator RI-CLPM 4 waves with 3 indicators for each variable at each wave (24 observed variables). Fitting a model with constraints to ensure strong factorial invariance, with a random intercept for each indicator separately.


```r
RICLPM3.ext3 <- '
  
  ################
  # BETWEEN PART #
  ################
  
  # Create between factors (random intercepts) for each indicator separately.
  RIX1 =~ 1*x11 + 1*x21 + 1*x31 + 1*x41 + 1*x51
  RIX2 =~ 1*x12 + 1*x22 + 1*x32 + 1*x42 + 1*x52
  RIX3 =~ 1*x13 + 1*x23 + 1*x33 + 1*x43 + 1*x53
  
  RIY1 =~ 1*y11 + 1*y21 + 1*y31 + 1*y41 + 1*y51
  RIY2 =~ 1*y12 + 1*y22 + 1*y32 + 1*y42 + 1*y52
  RIY3 =~ 1*y13 + 1*y23 + 1*y33 + 1*y43 + 1*y53
  
  ##################################
  # WITHIN PART: MEASUREMENT MODEL #
  ##################################
  
  # Factor models for x at 5 waves (constrained).
  WFX1 =~ a*x11 + b*x12 + c*x13
  WFX2 =~ a*x21 + b*x22 + c*x23
  WFX3 =~ a*x31 + b*x32 + c*x33
  WFX4 =~ a*x41 + b*x42 + c*x43
  WFX5 =~ a*x51 + b*x52 + c*x53
  
  # Factor models for y at 5 waves (constrained).
  WFY1 =~ d*y11 + e*y12 + f*y13
  WFY2 =~ d*y21 + e*y22 + f*y23
  WFY3 =~ d*y31 + e*y32 + f*y33
  WFY4 =~ d*y41 + e*y42 + f*y43
  WFY5 =~ d*y51 + e*y52 + f*y53
  
  # Constrained intercepts over time (this is necessary for strong factorial 
  # invariance; without these contraints we have week factorial invariance). 
  x11 + x21 + x31 + x41 + x51 ~ g*1
  x12 + x22 + x32 + x42 + x52 ~ h*1
  x13 + x23 + x33 + x43 + x53 ~ i*1
  y11 + y21 + y31 + y41 + y51 ~ j*1
  y12 + y22 + y32 + y42 + y52 ~ k*1
  y13 + y23 + y33 + y43 + y53 ~ l*1
  
  # Free latent means from t = 2 onward (only do this in combination with the 
  # constraints on the intercepts; without these, this would not be specified).
  WFX2 + WFX3 + WFX4 + WFX5 + WFY2 + WFY3 + WFY4 + WFY5 ~ 1
  
  #########################
  # WITHIN PART: DYNAMICS #
  #########################
  
  # Specify the lagged effects between the within-person centered latent variables.
  WFX2 + WFY2 ~ WFX1 + WFY1
  WFX3 + WFY3 ~ WFX2 + WFY2
  WFX4 + WFY4 ~ WFX3 + WFY3
  WFX5 + WFY5 ~ WFX4 + WFY4
  
  # Estimate the correlations within the same wave.
  WFX1 ~~ WFY1
  WFX2 ~~ WFY2
  WFX3 ~~ WFY3 
  WFX4 ~~ WFY4
  WFX5 ~~ WFY5
  
  ##########################
  # ADDITIONAL CONSTRAINTS #
  ##########################
  
  # Constrain covariance of the between factors and exogenous within factors to 0. 
  RIX1 + RIX2 + RIX3 + RIY1 + RIY2 + RIY3 ~~ 0*WFY1 + 0*WFX1
'
RICLPM3.ext3.fit <- cfa(RICLPM3.ext3, data = datMI, missing = 'ML')
summary(RICLPM3.ext3.fit, standardized = T)
```

---

### Extra
Multiple indicator RI-CLPM, 5 waves with 3 indicators for each variable at each wave (30 observed variables). Fitting a model With constraints to ensure strong factorial invariance, with a random intercept for each indicator separately, for which a factor model is specified, with factor loadings equal to the within- person factor loadings. 


```r
RICLPM4.ext3 <- '
  
  ################
  # BETWEEN PART #
  ################
  
  # Create between factors (random intercepts) for each indicator separately.
  RIX1 =~ 1*x11 + 1*x21 + 1*x31 + 1*x41 + 1*x51
  RIX2 =~ 1*x12 + 1*x22 + 1*x32 + 1*x42 + 1*x52
  RIX3 =~ 1*x13 + 1*x23 + 1*x33 + 1*x43 + 1*x53
  
  RIY1 =~ 1*y11 + 1*y21 + 1*y31 + 1*y41 + 1*y51
  RIY2 =~ 1*y12 + 1*y22 + 1*y32 + 1*y42 + 1*y52
  RIY3 =~ 1*y13 + 1*y23 + 1*y33 + 1*y43 + 1*y53
  
  # Create a single random intercept for al x variables, and another for all 
  # y variables and constrain the factor loadings to be identical to the 
  # within-person factor loadings. 
  
  RIX =~ a*RIX1 + b*RIX2 + c*RIX3
  RIY =~ d*RIY1 + e*RIY2 + f*RIY3
  
  # Add (co)variance between two higher-order random intercepts. 
  RIX ~~ RIY
  RIX ~~ RIX
  RIY ~~ RIY
  
  # Constrain measurement error variances of the second order factor model to 0. 
  RIX1 ~~ 0*RIX1
  RIX2 ~~ 0*RIX2
  RIX3 ~~ 0*RIX3
  RIY1 ~~ 0*RIY1
  RIY2 ~~ 0*RIY2
  RIY3 ~~ 0*RIY3
  
  ##################################
  # WITHIN PART: MEASUREMENT MODEL #
  ##################################
  
  # Factor models for x at 5 waves (constrained).
  WFX1 =~ a*x11 + b*x12 + c*x13
  WFX2 =~ a*x21 + b*x22 + c*x23
  WFX3 =~ a*x31 + b*x32 + c*x33
  WFX4 =~ a*x41 + b*x42 + c*x43
  WFX5 =~ a*x51 + b*x52 + c*x53
  
  # Factor models for y at 5 waves (constrained).
  WFY1 =~ d*y11 + e*y12 + f*y13
  WFY2 =~ d*y21 + e*y22 + f*y23
  WFY3 =~ d*y31 + e*y32 + f*y33
  WFY4 =~ d*y41 + e*y42 + f*y43
  WFY5 =~ d*y51 + e*y52 + f*y53
  
  # Constrained intercepts over time (this is necessary for strong factorial 
  # invariance; without these contraints we have week factorial invariance). 
  x11 + x21 + x31 + x41 + x51 ~ g*1
  x12 + x22 + x32 + x42 + x52 ~ h*1
  x13 + x23 + x33 + x43 + x53 ~ i*1
  y11 + y21 + y31 + y41 + y51 ~ j*1
  y12 + y22 + y32 + y42 + y52 ~ k*1
  y13 + y23 + y33 + y43 + y53 ~ l*1
  
  # Free latent means from t = 2 onward (only do this in combination with the 
  # constraints on the intercepts; without these, this would not be specified).
  WFX2 + WFX3 + WFX4 + WFX5 + WFY2 + WFY3 + WFY4 + WFY5 ~ 1
  
  #########################
  # WITHIN PART: DYNAMICS #
  #########################
  
  # Specify the lagged effects between the within-person centered latent variables.
  WFX2 + WFY2 ~ WFX1 + WFY1
  WFX3 + WFY3 ~ WFX2 + WFY2
  WFX4 + WFY4 ~ WFX3 + WFY3
  WFX5 + WFY5 ~ WFX4 + WFY4
  
  # Estimate the correlations within the same wave.
  WFX1 ~~ WFY1
  WFX2 ~~ WFY2
  WFX3 ~~ WFY3 
  WFX4 ~~ WFY4
  WFX5 ~~ WFY5
  
  ##########################
  # ADDITIONAL CONSTRAINTS #
  ##########################
  
  # Constrain covariance of the between factors and exogenous within factors to 0. 
  RIX + RIY + RIX1 + RIX2 + RIX3 + RIY1 + RIY2 + RIY3 ~~ 0*WFY1 + 0*WFX1
'
RICLPM4.ext3.fit <- cfa(RICLPM4.ext3, data = datMI, missing = 'ML')
summary(RICLPM4.ext3.fit, standardized = T)
```

---

### Step 4
 Multiple indicator RI-CLPM, 5 waves with 3 indicators for each variable at each wave (30 observed variables). Fitting a model with constraints to ensure strong factorial invariance, with the RI-CLPM at the latent level.


```r
RICLPM5.ext3 <- '
  
  #####################
  # MEASUREMENT MODEL #
  #####################
  
  # Factor models for x at 5 waves (constrained). 
  FX1 =~ a*x11 + b*x12 + c*x13
  FX2 =~ a*x21 + b*x22 + c*x23
  FX3 =~ a*x31 + b*x32 + c*x33
  FX4 =~ a*x41 + b*x42 + c*x43
  FX5 =~ a*x51 + b*x52 + c*x53
  
  # Factor models for y at 5 waves (constrained).
  FY1 =~ d*y11 + e*y12 + f*y13
  FY2 =~ d*y21 + e*y22 + f*y23
  FY3 =~ d*y31 + e*y32 + f*y33
  FY4 =~ d*y41 + e*y42 + f*y43
  FY5 =~ d*y51 + e*y52 + f*y53
  
  # Constrained intercepts over time (this is necessary for strong factorial 
  # invariance; without these contraints we have week factorial invariance). 
  x11 + x21 + x31 + x41 + x51 ~ g*1
  x12 + x22 + x32 + x42 + x52 ~ h*1
  x13 + x23 + x33 + x43 + x53 ~ i*1
  y11 + y21 + y31 + y41 + y51 ~ j*1
  y12 + y22 + y32 + y42 + y52 ~ k*1
  y13 + y23 + y33 + y43 + y53 ~ l*1
  
  # Free latent means from t = 2 onward (only do this in combination with the 
  # constraints on the intercepts; without these, this would not be specified).
  FX2 + FX3 + FX4 + FX5 + FY2 + FY3 + FY4 + FY5 ~ 1
  
  ################
  # BETWEEN PART #
  ################
  
  # Create between factors (random intercepts). 
  RIX =~ 1*FX1 + 1*FX2 + 1*FX3 + 1*FX4 + 1*FX5
  RIY =~ 1*FY1 + 1*FY2 + 1*FY3 + 1*FY4 + 1*FY5
  # Set the residual variances of all FX and FY variables to 0. 
  FX1 ~~ 0*FX1
  FX2 ~~ 0*FX2
  FX3 ~~ 0*FX3
  FX4 ~~ 0*FX4
  FX5 ~~ 0*FX5
  FY1 ~~ 0*FY1
  FY2 ~~ 0*FY2
  FY3 ~~ 0*FY3
  FY4 ~~ 0*FY4
  FY5 ~~ 0*FY5
  
  ###############
  # WITHIN PART #
  ###############
 
  # Create the within-part.
  WFX1 =~ 1*FX1
  WFX2 =~ 1*FX2
  WFX3 =~ 1*FX3
  WFX4 =~ 1*FX4
  WFX5 =~ 1*FX5
  
  WFY1 =~ 1*FY1
  WFY2 =~ 1*FY2
  WFY3 =~ 1*FY3
  WFY4 =~ 1*FY4
  WFY5 =~ 1*FY5
  
  # Specify the lagged effects between the within-person centered latent variables.
  WFX2 + WFY2 ~ WFX1 + WFY1
  WFX3 + WFY3 ~ WFX2 + WFY2
  WFX4 + WFY4 ~ WFX3 + WFY3
  WFX5 + WFY5 ~ WFX4 + WFY4
  
  # Estimate the correlations within the same wave.
  WFX2 ~~ WFY2
  WFX3 ~~ WFY3 
  WFX4 ~~ WFY4
  WFX5 ~~ WFY5
  
  ##########################
  # ADDITIONAL CONSTRAINTS #
  ##########################
  
  # Set correlations between the between-factors (random intercepts) and within-
  # factors at wave 1 at 0. 
  RIX + RIY ~~ 0*WFX1 + 0*WFY1
'
RICLPM5.ext3.fit <- cfa(RICLPM5.ext3, data = datMI, missing = 'ML')
summary(RICLPM5.ext3.fit, standardized = T)
```

---

## $\bar{\chi}^{2}$-test

The use of the chi-square difference test is wide-spread in the SEM community to test constaints on parameters. However, when constraints are placed on the bound of the parameter space, we should use the chi-bar-square test ($\bar{\chi}^{2}$-test) [@Stoel2006]. For example, if we constrain the variances of all random intercepts (and their covariance) in the RI-CLPM to zero, we obtain a model that is nested under the RI-CLPM, and that is statistically equivalent to the traditional cross-lagged panel model (CLPM). Below you can find R code for performing the chi-bar-square test (code by [Rebecca M. Kuiper](https://www.uu.nl/staff/rmkuiper/Software)) for comparing these two models. It involves 

1. fitting both the RI-CLPM (`RICLPM.fit`) and CLPM (`CLPM.fit`);
2. extracting the covariance matrix of the random intercepts;
3. extracting the $\chi^{2}$ and degrees of freedom of both models; and 
4. performing the $\bar{\chi}^{2}$-test using the `ChiBarSq.DiffTest` package [@Kuiper2020].


```r
# Install and load the required packages. 
## library(devtools)
## install_github("rebeccakuiper/ChiBarSq.DiffTest")
library(ChiBarSq.DiffTest)

# Step 1
## Fit the RI-CLPM (RICLPM.fit) and the CLPM (CLPM.fit).

# Step 2
## Check which indices you need to get the covariance matrix of the random intercepts. 
vcov(RICLPM.fit) # Full covariance matrix

## The 22nd and the 23rd indices regard the random intercepts
indices <- c(22, 23) 
q <- length(indices) # Number of random intercepts
S <- vcov(RICLPM.fit)[indices, indices] # Extract full covariance matrix of the random intercepts

# Step 3
Chi2_clpm <- summary(CLPM.fit, fit.measures = TRUE)[1]$FIT[c("chisq")] # Extract chi-square value of CLPM
Chi2_riclpm <- summary(RICLPM.fit, fit.measures = TRUE)[1]$FIT[c("chisq")] # Extract chi-square value of RI-CLPM

df_clpm <- summary(CLPM.fit, fit.measures = TRUE)[1]$FIT[c("df")] # Extract df of CLPM
df_riclpm <- summary(RICLPM.fit, fit.measures = TRUE)[1]$FIT[c("df")] # Extract df of RI-CLPM

# Step 4
## Run function to do chi-bar-square test (and also obtain Chi-bar-square weigths)
ChiBar2DiffTest <- ChiBarSq.DiffTest(q, S, Chi2_clpm, Chi2_riclpm, df_clpm, df_riclpm)
ChiBar2DiffTest
ChiBar2DiffTest$p_value
```

---

## References
