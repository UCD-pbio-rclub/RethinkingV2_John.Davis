---
title: "Ch. 7 HW"
author: "John D."
date: "July 17, 2019"
output: 
  html_document: 
    keep_md: yes
---




```r
library(tidyverse)
library(rethinking)
```

## 6M1. Write down and compare the definitions of AIC, DIC, and WAIC. Which of these criteria is most general? Which assumptions are required to transform a more general criterion into a less general one?

### AIC
  AIC = −2lppd + 2p  
  Assumes flat priors, approximately multivariate gaussian posterior distribution, and N >> k

### DIC
  I'm assuming DIC = D_theta_bar + 2PD  
  Allows informative priors but assumes approximately multivariate gaussian posterior distribution and N >> k
  
### WAIC
  WAIC = -2*( sum(lppd) - sum(pWAIC))  
  Makes no assumption of posterior shape or priors. Not sure about sample size and number of parameters
  
  
WAIC is the most general. To make a criterion less general you must begin to assume the shape of the posterior distribution and priors.

## 6M2. Explain the difference between model selection and model averaging. What information is lost under model selection? What information is lost under model averaging?

Model selection you are picking the model with the best criterion value (lowest). This kind of selection procedure discards the information about relative model accuracy contained in the differences among the LOOCV/LOOIS/WAIC values. Model averaging is methods for combining the predictions of multiple models. I assume we weaken the effects of certain parameters.

## 6M3. When comparing models with an information criterion, why must all models be fit to exactly the same observations? What would happen to the information criterion values, if the models were fit to different numbers of observations? Perform some experiments, if you are not sure.


```r
data(cars)
set.seed(120)
m1 <- quap(
  alist(
    dist ~ dnorm(mu,sigma),
    mu <- a + b*speed,
    a ~ dnorm(0,100),
    b ~ dnorm(0,10),
    sigma ~ dexp(1)
    ),
  data=cars[1:25,]
  )
set.seed(120)
m2 <- quap(
  alist(
    dist ~ dnorm(mu,sigma),
    mu <- a + b*speed,
    a ~ dnorm(0,100),
    b ~ dnorm(0,10),
    sigma ~ dexp(1)
    ),
  data=cars
  )
WAIC(m1)
```

```
## [1] 213.8275
## attr(,"lppd")
## [1] -99.74424
## attr(,"pWAIC")
## [1] 7.16952
## attr(,"se")
## [1] 22.90026
```

```r
WAIC(m2)
```

```
## [1] 423.4783
## attr(,"lppd")
## [1] -206.969
## attr(,"pWAIC")
## [1] 4.770138
## attr(,"se")
## [1] 17.79706
```

```r
compare(m1,m2)
```

```
## Warning in compare(m1, m2): Different numbers of observations found for at least two models.
## Information criteria only valid for comparing models fit to exactly same observations.
## Number of observations for each model:
## m1 25 
## m2 50
```

```
##        WAIC    pWAIC    dWAIC       weight       SE      dSE
## m1 214.7377 7.535869   0.0000 1.000000e+00 23.82693       NA
## m2 422.8115 4.476055 208.0739 6.566546e-46 17.26235 20.85066
```

More observations will lead higher WAIC making models with less observations look better.

## 6M4. What happens to the effective number of parameters, as measured by DIC or WAIC, as a prior becomes more concentrated? Why? Perform some experiments, if you are not sure.


```r
set.seed(120)
m1 <- quap(
  alist(
    dist ~ dnorm(mu,sigma),
    mu <- a + b*speed,
    a ~ dnorm(0,1),
    b ~ dnorm(0,1),
    sigma ~ dexp(1)
    ),
  data=cars
  )
set.seed(120)
m2 <- quap(
  alist(
    dist ~ dnorm(mu,sigma),
    mu <- a + b*speed,
    a ~ dnorm(0,100),
    b ~ dnorm(0,10),
    sigma ~ dexp(1)
    ),
  data=cars
  )
WAIC(m1)
```

```
## [1] 427.6125
## attr(,"lppd")
## [1] -210.1153
## attr(,"pWAIC")
## [1] 3.690952
## attr(,"se")
## [1] 17.88879
```

```r
WAIC(m2)
```

```
## [1] 423.4783
## attr(,"lppd")
## [1] -206.969
## attr(,"pWAIC")
## [1] 4.770138
## attr(,"se")
## [1] 17.79706
```

```r
compare(m1,m2)
```

```
##        WAIC    pWAIC    dWAIC    weight       SE      dSE
## m2 422.8115 4.476055 0.000000 0.8951167 17.26235       NA
## m1 427.0997 3.404671 4.288211 0.1048833 17.62291 5.782902
```

pWAIC becomes smaller as more noise is filtered out due to being skeptic of prior values.

## 6M5. Provide an informal explanation of why informative priors reduce overfitting.

Informative priors reduce overfitting by reducing how much a model learns from an observation causing irregular observations to have a lesser effect on moving the coefficient.

## 6M6. Provide an information explanation of why overly informative priors result in underfitting.

Too informative of priors will cause the model to learn too little from the observations it is presented causing the observations to be overpowered by the stringency of the priors. This then leads to underfitting because the model is not taking advantage of all the information it is being provided.
