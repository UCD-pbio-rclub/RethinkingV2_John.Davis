Untitled
================
Your Name
The Date

``` r
library(rethinking)
```

    ## Loading required package: rstan

    ## Loading required package: StanHeaders

    ## Loading required package: ggplot2

    ## rstan (Version 2.19.3, GitRev: 2e1f913d3ca3)

    ## For execution on a local, multicore CPU with excess RAM we recommend calling
    ## options(mc.cores = parallel::detectCores()).
    ## To avoid recompilation of unchanged Stan programs, we recommend calling
    ## rstan_options(auto_write = TRUE)

    ## For improved execution time, we recommend calling
    ## Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')
    ## although this causes Stan to throw an error on a few processors.

    ## Loading required package: parallel

    ## Loading required package: dagitty

    ## rethinking (Version 1.95)

    ## 
    ## Attaching package: 'rethinking'

    ## The following object is masked from 'package:stats':
    ## 
    ##     rstudent

``` r
library(tidyverse)
```

    ## -- Attaching packages ----------------------------------------------------------------------------------------- tidyverse 1.3.0 --

    ## v tibble  2.1.3     v dplyr   0.8.4
    ## v tidyr   1.0.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0
    ## v purrr   0.3.3

    ## -- Conflicts -------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x tidyr::extract() masks rstan::extract()
    ## x dplyr::filter()  masks stats::filter()
    ## x dplyr::lag()     masks stats::lag()
    ## x purrr::map()     masks rethinking::map()

  
![
y\_i \\sim Normal(\\mu\_i, \\sigma) \\\\
\\mu\_i = \\alpha\_{group\[i\]} + \\beta x\_i \\\\
\\alpha\_{group} \\sim Normal(\\alpha, \\sigma\_\\alpha) \\\\
\\alpha \\sim Normal(0, 10) \\\\
\\beta \\sim Normal(0, 1) \\\\
\\sigma \\sim HalfCauchy(0, 2) \\\\
\\sigma\_\\alpha \\sim HalfCauchy(0, 2)
](https://latex.codecogs.com/png.latex?%0Ay_i%20%5Csim%20Normal%28%5Cmu_i%2C%20%5Csigma%29%20%5C%5C%0A%5Cmu_i%20%3D%20%5Calpha_%7Bgroup%5Bi%5D%7D%20%2B%20%5Cbeta%20x_i%20%5C%5C%0A%5Calpha_%7Bgroup%7D%20%5Csim%20Normal%28%5Calpha%2C%20%5Csigma_%5Calpha%29%20%5C%5C%0A%5Calpha%20%5Csim%20Normal%280%2C%2010%29%20%5C%5C%0A%5Cbeta%20%5Csim%20Normal%280%2C%201%29%20%5C%5C%0A%5Csigma%20%5Csim%20HalfCauchy%280%2C%202%29%20%20%5C%5C%0A%5Csigma_%5Calpha%20%5Csim%20HalfCauchy%280%2C%202%29%0A
"
y_i \\sim Normal(\\mu_i, \\sigma) \\\\
\\mu_i = \\alpha_{group[i]} + \\beta x_i \\\\
\\alpha_{group} \\sim Normal(\\alpha, \\sigma_\\alpha) \\\\
\\alpha \\sim Normal(0, 10) \\\\
\\beta \\sim Normal(0, 1) \\\\
\\sigma \\sim HalfCauchy(0, 2)  \\\\
\\sigma_\\alpha \\sim HalfCauchy(0, 2)
")
