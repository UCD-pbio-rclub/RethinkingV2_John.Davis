---
title: "Ch.8 HW"
author: "John D."
date: "July 29, 2019"
output: 
  html_document: 
    keep_md: yes
---




```r
library(tidyverse)
```

```
## -- Attaching packages ------------------------------------------------------- tidyverse 1.2.1 --
```

```
## v ggplot2 3.1.1     v purrr   0.3.2
## v tibble  2.1.3     v dplyr   0.8.1
## v tidyr   0.8.3     v stringr 1.4.0
## v readr   1.3.1     v forcats 0.4.0
```

```
## -- Conflicts ---------------------------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(rethinking)
```

```
## Loading required package: rstan
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
## 
## Attaching package: 'rstan'
```

```
## The following object is masked from 'package:tidyr':
## 
##     extract
```

```
## Loading required package: parallel
```

```
## rethinking (Version 1.88)
```

```
## 
## Attaching package: 'rethinking'
```

```
## The following object is masked from 'package:purrr':
## 
##     map
```

## 7E1. For each of the causal relationships below, name a hypothetical third variable that would lead to an interaction effect.

  1. Bread dough rises because of yeast.
  
  The amount of moisture in the dough. Yeast need water, too little and bread will not rise and too much and it will not rise
  
  2. Education leads to higher income
  
  The field of study. There is a large variance in income based on field of study 
  
  3. Gasoline makes a car go
  
  The transmission, bad transmission, car no go.
  Spark plugs, spark plugs make gas go boom and boom makes car go. No spark plug no boom
  
  
## 7M3. In parts of North America, ravens depend upon wolves for their food. This is because ravens are carnivorous but cannot usually kill or open carcasses of prey. Wolves however can and do kill and tear open animals, and they tolerate ravens co-feeding at their kills. This species relationship is generally described as a “species interaction.” Can you invent a hypothetical set of data on raven population size in which this relationship would manifest as a statistical interaction? Do you think the biological interaction could be linear? Why or why not?

ravens is the size raven population  
i is a population of ravens  
W is the number of wolves  
A is the area of the wolve territory  
$$
ravens_i \sim Normal(\mu_i,\sigma) \\
\mu_i = \alpha + \beta_WW_i + \beta_AA_I + \beta_{WA}W_iA_i
$$
I believe this relationship could be linear up to a certain point. Each wolf requires a certain amount of area and each wolve provides food for a certain number of ravens. Too small of area leads to less wolves which leads to less kills which leads to less food for ravens. Too many wolves leads to less food for ravens. Wolves also hunt in packs and are territorial which will also probably affect number of ravens each pack can feed based on pack size and territory.


## 7H3. Consider again the data(rugged) data on economic development and terrain ruggedness, examined in this chapter. One of the African countries in that example, Seychelles, is far outside the cloud of other nations, being a rare country with both relatively high GDP and high ruggedness. Seychelles is also unusual, in that it is a group of islands far from the coast of mainland Africa, and its main economic activity is tourism.

## One might suspect that this one nation is exerting a strong influence on the conclusions. In this problem, I want you to drop Seychelles from the data and re-evaluate the hypothesis that the relationship of African economies with ruggedness is different from that on other continents.

  a. Begin by using map to fit just the interaction model:
  
  $$
  y_i \sim Normal(\mu_i, \sigma) \\
  u_i = \alpha + \beta_AA_i + \beta_RR_i + \beta_{AR}A_iR_i
  $$
  I don't like this format, he talks against it in the book. This section must not be updated.

## where y is log GDP per capita in the year 2000 (log of rgdppc_2000); A is cont_africa, the dummy variable for being an African nation; and R is the variable rugged. Choose your own priors. Compare the inference from this model fit to the data without Seychelles to the same model fit to the full data. Does it still seem like the effect of ruggedness depends upon continent? How much has the expected relationship changed?




```r
data(rugged)
rugged$log_gdp <- log(rugged$rgdppc_2000)
dd <- rugged[ complete.cases(rugged$rgdppc_2000), ]
dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
dd$rugged_std <- dd$rugged / max(dd$rugged)
dd$cid <- ifelse( dd$cont_africa==1 , 1 , 2 )
dd.small <- dd %>% filter(country != "Seychelles")
rugged_seq <- seq( from=-0.1 , to=1.1 , length.out=30 )
dd.A1 <- dd[ dd$cont_africa==1 , ] # Africa
dd.A0 <- dd[ dd$cont_africa==0 , ] # not Africa
dd.small.A1 <- dd.small[ dd.small$cont_africa==1 , ] # Africa
dd.small.A0 <- dd.small[ dd.small$cont_africa==0 , ] # not Africa
```


```r
H3A <- quap(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a + bA*cont_africa + bR*rugged + bAR*cont_africa*rugged ,
    a ~ dnorm( 1 , 0.1 ),
    bA ~ dnorm(0, 1),
    bR ~ dnorm( 0 , 0.3 ),
    bAR ~ dnorm(0,1),
    sigma ~ dexp( 1 )
    ) ,
  data=dd )

H3B <- quap(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a + bA*cont_africa + bR*rugged + bAR*cont_africa*rugged ,
    a ~ dnorm( 1 , 0.1 ),
    bA ~ dnorm(0, 1),
    bR ~ dnorm( 0 , 0.3 ),
    bAR ~ dnorm(0,1),
    sigma ~ dexp( 1 )
    ) ,
  data=dd.small)
```


```r
precis(H3A, depth = 2)
```

```
##              mean          sd        5.5%        94.5%
## a      1.08069355 0.015984011  1.05514801  1.106239087
## bA    -0.22640537 0.026218707 -0.26830793 -0.184502815
## bR    -0.02284086 0.008900384 -0.03706539 -0.008616324
## bAR    0.04516789 0.015219169  0.02084472  0.069491059
## sigma  0.10947151 0.005932000  0.09999102  0.118951987
```

```r
precis(H3B, depth = 2)
```

```
##              mean          sd         5.5%        94.5%
## a      1.08073030 0.015863794  1.055376894  1.106083707
## bA    -0.21874783 0.026323591 -0.260818011 -0.176677646
## bR    -0.02285813 0.008832798 -0.036974648 -0.008741615
## bAR    0.03394513 0.016191929  0.008067302  0.059822962
## sigma  0.10862623 0.005904623  0.099189505  0.118062961
```


Relationship is weaker, but effect of ruggedness does depend on continent

  b. Now plot the predictions of the interaction model, with and without Seychelles. Does it still seem like the effect of ruggedness depends upon continent? How much has the expected relationship changed?


```r
par(mfrow=c(2,2))
plot( dd.A1$rugged_std , dd.A1$log_gdp_std , pch=16 , col=rangi2 ,
      xlab="ruggedness (standardized)" , ylab="log GDP (as proportion of mean)" ,
      xlim=c(0,1) )
mu <- link( H3A , data=data.frame( cont_africa=1 , rugged=rugged_seq ) )
mu_mean <- apply( mu , 2 , mean )
mu_ci <- apply( mu , 2 , PI , prob=0.97 )
lines( rugged_seq , mu_mean , lwd=2 )
shade( mu_ci , rugged_seq , col=col.alpha(rangi2,0.3) )
mtext("African nations with Seychelles")

plot( dd.A0$rugged_std , dd.A0$log_gdp_std , pch=16 , col=rangi2 ,
      xlab="ruggedness (standardized)" , ylab="log GDP (as proportion of mean)" ,
      xlim=c(0,1) )
mu <- link( H3A , data=data.frame( cont_africa=0 , rugged=rugged_seq ) )
mu_mean <- apply( mu , 2 , mean )
mu_ci <- apply( mu , 2 , PI , prob=0.97 )
lines( rugged_seq , mu_mean , lwd=2 )
shade( mu_ci , rugged_seq , col=col.alpha(rangi2,0.3) )
mtext("Non-African nations with Seychelles")

plot( dd.small.A1$rugged_std , dd.small.A1$log_gdp_std , pch=16 , col=rangi2 ,
      xlab="ruggedness (standardized)" , ylab="log GDP (as proportion of mean)" ,
      xlim=c(0,1) )
mu <- link( H3B , data=data.frame( cont_africa=1 , rugged=rugged_seq ) )
mu_mean <- apply( mu , 2 , mean )
mu_ci <- apply( mu , 2 , PI , prob=0.97 )
lines( rugged_seq , mu_mean , lwd=2 )
shade( mu_ci , rugged_seq , col=col.alpha(rangi2,0.3) )
mtext("African nations without Seychelles")

plot( dd.A0$rugged_std , dd.A0$log_gdp_std , pch=16 , col=rangi2 ,
      xlab="ruggedness (standardized)" , ylab="log GDP (as proportion of mean)" ,
      xlim=c(0,1) )
mu <- link( H3B , data=data.frame( cont_africa=0 , rugged=rugged_seq ) )
mu_mean <- apply( mu , 2 , mean )
mu_ci <- apply( mu , 2 , PI , prob=0.97 )
lines( rugged_seq , mu_mean , lwd=2 )
shade( mu_ci , rugged_seq , col=col.alpha(rangi2,0.3) )
mtext("Non-African nations without Seychelles")
```

![](Ch.8_HW_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Does not look like Seychelles affected much
  
  c.  Finally, conduct a model comparison analysis, using WAIC. Fit three models to the data without Seychelles:
  
  $$
  Model\ 1: y_i \sim Normal(mu_i,\sigma) \\
  \mu_i = \alpha + \beta_rR_i
  $$
  
  $$
  Model\ 2: y_i \sim Normal(\mu_i,\sigma) \\
  \mu_i = \alpha + \beta_AA_i + \beta_RR_i
  $$
  
  $$
  Model\ 3 = y_i \sim Norma(\mu_i,\sigma) \\
  \mu_i = \alpha + \beta_AA_i + \beta_rR_i + \beta_{AR}A_iR_i
  $$
  

```r
H31 <- quap(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged,
    a ~ dnorm( 1 , 0.1 ),
    bR ~ dnorm( 0 , 0.3 ),
    sigma ~ dexp( 1 )
    ) ,
  data=dd.small )

H32 <- quap(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a + bA*cont_africa + bR*rugged,
    a ~ dnorm( 1 , 0.1 ),
    bA ~ dnorm(0, 1),
    bR ~ dnorm( 0 , 0.3 ),
    sigma ~ dexp( 1 )
    ) ,
  data=dd.small )

H33 <- quap(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a + bA*cont_africa + bR*rugged + bAR*cont_africa*rugged ,
    a ~ dnorm( 1 , 0.1 ),
    bA ~ dnorm(0, 1),
    bR ~ dnorm( 0 , 0.3 ),
    bAR ~ dnorm(0,1),
    sigma ~ dexp( 1 )
    ) ,
  data=dd.small )

compare(H31,H32,H33)
```

```
##          WAIC    pWAIC     dWAIC       weight       SE       dSE
## H33 -261.0517 4.499277  0.000000 8.472689e-01 15.23824        NA
## H32 -257.6251 4.104183  3.426678 1.527311e-01 14.53204  3.845608
## H31 -188.2139 2.511732 72.837842 1.292662e-16 13.30807 15.617742
```


## Use whatever priors you think are sensible. Plot the model-averaged predictions of this model set. Do your inferences differ from those in (b)? Why or why not?

uhh

## Use the tomato.csv (attached) data set and evaluate whether hypocotyl length ("hyp") is affected by shade ("trt"), species ("species") and their interaction.


```r
dat <- read_csv("Tomato.csv") %>% select(hyp,trt,species) %>% na.omit()
```

```
## Parsed with column specification:
## cols(
##   .default = col_double(),
##   shelf = col_character(),
##   col = col_character(),
##   acs = col_character(),
##   trt = col_character(),
##   date = col_character(),
##   species = col_character(),
##   who = col_character()
## )
```

```
## See spec(...) for full column specifications.
```

```r
head(dat)
```

```
## # A tibble: 6 x 3
##     hyp trt   species        
##   <dbl> <chr> <chr>          
## 1  19.5 H     S. pennellii   
## 2  31.3 H     S. peruvianum  
## 3  56.6 H     S. peruvianum  
## 4  35.2 H     S. chilense    
## 5  35.3 H     S. chilense    
## 6  28.7 H     S. chmielewskii
```

```r
unique(dat$trt)
```

```
## [1] "H" "L"
```

```r
unique(dat$species)
```

```
## [1] "S. pennellii"    "S. peruvianum"   "S. chilense"     "S. chmielewskii"
## [5] "S. habrochaites"
```

```r
dat$hyp_std <- scale(dat$hyp)
dat$trt_factor <- ifelse(dat$trt == "L",0,1)
dat$species_factor <- as.numeric(as.factor(dat$species))
head(dat)
```

```
## # A tibble: 6 x 6
##     hyp trt   species         hyp_std[,1] trt_factor species_factor
##   <dbl> <chr> <chr>                 <dbl>      <dbl>          <dbl>
## 1  19.5 H     S. pennellii         -1.40           1              4
## 2  31.3 H     S. peruvianum        -0.209          1              5
## 3  56.6 H     S. peruvianum         2.34           1              5
## 4  35.2 H     S. chilense           0.183          1              1
## 5  35.3 H     S. chilense           0.198          1              1
## 6  28.7 H     S. chmielewskii      -0.464          1              2
```

```r
T1 <- quap(
  alist(
    hyp_std ~ dnorm(mu,sigma),
    mu <- a[species_factor] + bT*trt_factor,
    a[species_factor] ~ dnorm(0,.5),
    bT ~ dnorm(0,1),
    sigma ~ dexp(1)
  ),
  data = dat
)
T2 <- quap(
  alist(
    hyp_std ~ dnorm(mu,sigma),
    mu <- a[species_factor] + bT[species_factor]*trt_factor,
    a[species_factor] ~ dnorm(0,.5),
    bT[species_factor] ~ dnorm(0,1),
    sigma ~ dexp(1)
  ),
  data = dat
)
precis(T1, depth = 2)
```

```
##              mean         sd        5.5%       94.5%
## a[1]   0.39351677 0.06828895  0.28437785  0.50265570
## a[2]   0.12339651 0.06564746  0.01847919  0.22831384
## a[3]   0.04681255 0.06669889 -0.05978515  0.15341026
## a[4]  -0.21607190 0.08188992 -0.34694781 -0.08519598
## a[5]   0.76131397 0.06678539  0.65457801  0.86804992
## bT    -0.52583410 0.05664874 -0.61636972 -0.43529847
## sigma  0.90655451 0.02017818  0.87430588  0.93880314
```

```r
precis(T2, depth = 2)
```

```
##              mean         sd       5.5%       94.5%
## a[1]   0.43805228 0.08617538  0.3003274  0.57577718
## a[2]   0.01049486 0.08178879 -0.1202194  0.14120915
## a[3]  -0.04974804 0.08539195 -0.1862209  0.08672478
## a[4]  -0.03348385 0.10319314 -0.1984064  0.13143871
## a[5]   0.79519900 0.08354050  0.6616851  0.92871286
## bT[1] -0.61694338 0.12323067 -0.8138898 -0.41999697
## bT[2] -0.28822512 0.11817287 -0.4770882 -0.09936205
## bT[3] -0.33959903 0.11811778 -0.5283740 -0.15082401
## bT[4] -0.93792116 0.15389342 -1.1838726 -0.69196976
## bT[5] -0.59683563 0.12052202 -0.7894531 -0.40421817
## sigma  0.89983423 0.02003071  0.8678213  0.93184718
```

```r
compare(T1,T2)
```

```
##        WAIC     pWAIC    dWAIC     weight       SE      dSE
## T2 2670.864 11.363778 0.000000 0.97843521 58.39296       NA
## T1 2678.494  7.724206 7.629786 0.02156479 58.04254 6.764712
```
