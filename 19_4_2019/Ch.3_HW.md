---
title: "R Notebook"
output:
  html_document:
    keep_md: yes
  html_notebook: default
  pdf_document: default
editor_options:
  chunk_output_type: inline
---

# Homework questions

## Easy


```r
library(rethinking)
```

```
## Loading required package: rstan
```

```
## Loading required package: ggplot2
```

```
## Loading required package: StanHeaders
```

```
## rstan (Version 2.18.2, GitRev: 2e1f913d3ca3)
```

```
## For execution on a local, multicore CPU with excess RAM we recommend calling
## options(mc.cores = parallel::detectCores()).
## To avoid recompilation of unchanged Stan programs, we recommend calling
## rstan_options(auto_write = TRUE)
```

```
## For improved execution time, we recommend calling
## Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
## although this causes Stan to throw an error on a few processors.
```

```
## Loading required package: parallel
```

```
## rethinking (Version 1.88)
```

```r
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
plot( p_grid , posterior , type="b" , xlab="probability of water" , ylab="posterior probability" )
```

![](Ch.3_HW_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

### 3E1. How much posterior probability lies below p = 0.2?

```r
# add up posterior probability where p < 0.2
sum( samples < 0.2) / 10000
```

```
## [1] 5e-04
```

### 3E2. How much posterior probability lies above p = 0.8?

```r
# add up posterior probability where p > 0.8
sum( samples > 0.8 ) / 10000
```

```
## [1] 0.1117
```

### 3E3. How much posterior probability lies between p = 0.2 and p = 0.8?

```r
# add up posterior probability where 0.2 < p < 0.8
sum( samples < 0.8 & samples > 0.2 ) / 10000
```

```
## [1] 0.8878
```

### 3E4. 20% of the posterior probability lies below which value of p?

```r
quantile( samples , 0.2 )
```

```
##       20% 
## 0.5195195
```

### 3E5. 20% of the posterior probability lies above which value of p?

```r
quantile( samples , 0.8 )
```

```
##       80% 
## 0.7567568
```

### 3E6. Which values of p contain the narrowest interval equal to 66% of the posterior probability?

```r
HPDI( samples , prob=0.66 )
```

```
##     |0.66     0.66| 
## 0.5205205 0.7847848
```

### 3E7. Which values of p contain 66% of the posterior probability, assuming equal posterior probability both below and above the interval?

```r
PI( samples , prob=0.66 )
```

```
##       17%       83% 
## 0.5005005 0.7687688
```

## Medium

### 3M1. Suppose the globe tossing data had turned out to be 8 water in 15 tosses. Construct the posterior distribution, using grid approximation. Use the same flat prior as before.

```r
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
plot( p_grid , posterior , type="b" , xlab="probability of water" , ylab="posterior probability" )
```

![](Ch.3_HW_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

### 3M2. Draw 10,000 samples from the grid approximation from above. Then use the samples to calculate the 90% HPDI for p.

```r
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
HPDI(samples, prob = .9)
```

```
##      |0.9      0.9| 
## 0.3243243 0.7157157
```

### 3M3. Construct a posterior predictive check for this model and data. This means simulate the distribution of samples, averaging over the posterior uncertainty in p. What is the probability of observing 8 water in 15 tosses?

```r
w <- rbinom(1e4 , size= 15, prob=samples)
sum(w == 8) / 1e4
```

```
## [1] 0.1475
```

### 3M4. Using the posterior distribution constructed from the new (8/15) data, now calculate the probability of observing 6 water in 9 tosses.

```r
w <- rbinom(1e4 , size= 9, prob=samples)
sum(w == 6) / 1e4
```

```
## [1] 0.1766
```

