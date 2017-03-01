Programming Exercises
================
Cheng Yee Lim
February 1, 2017

Load necessary libraries
------------------------

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.3.2

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Warning: package 'ggplot2' was built under R version 3.3.2

    ## Warning: package 'tibble' was built under R version 3.3.2

    ## Warning: package 'tidyr' was built under R version 3.3.2

    ## Warning: package 'readr' was built under R version 3.3.2

    ## Warning: package 'purrr' was built under R version 3.3.2

    ## Warning: package 'dplyr' was built under R version 3.3.2

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
library(knitr)
```

    ## Warning: package 'knitr' was built under R version 3.3.2

Compute the number of unique values in each column of `iris`
------------------------------------------------------------

### Using a `for` loop

``` r
dfIris <- as_tibble(iris)
uniqueIris <- vector(mode="double", length = ncol(dfIris))
for (i in seq_along(dfIris)){
  uniqueIris[i] <- unique(dfIris[i], na.rm = TRUE)
  print(length(uniqueIris[[i]]))
}
```

    ## [1] 35
    ## [1] 23
    ## [1] 43
    ## [1] 22
    ## [1] 3

### Using a `map` function

``` r
uniqueMapIris <- map(dfIris, unique)
for(i in seq_along(uniqueMapIris)){
  print(length(uniqueMapIris[[i]]))
}
```

    ## [1] 35
    ## [1] 23
    ## [1] 43
    ## [1] 22
    ## [1] 3

Calculate the square of each element in vector `x`
--------------------------------------------------

``` r
x <- 1:30
```

### Using a `for` loop

``` r
xSq <- vector(mode = "double", length = length(x))
for (i in x) {
  xSq[[i]] <- x[[i]]*x[[i]] 
}
xSq
```

    ##  [1]   1   4   9  16  25  36  49  64  81 100 121 144 169 196 225 256 289
    ## [18] 324 361 400 441 484 529 576 625 676 729 784 841 900

### Using a `map` function

``` r
squared <- function(a) {
  square = a^2
  return(square)
}

xSqMap <- map_dbl(x, squared)
xSqMap
```

    ##  [1]   1   4   9  16  25  36  49  64  81 100 121 144 169 196 225 256 289
    ## [18] 324 361 400 441 484 529 576 625 676 729 784 841 900

Write a function to calculate length of sides in a right-triangle using the Pythagorean Theorem
-----------------------------------------------------------------------------------------------

``` r
pythagorean <-function(a=NA,b=NA,c=NA) {
  if (sum(is.na(c(a,b,c))) > 1) {
    return("You have provided lengths for less than two sides. Only provide the length for two sides")
  }
  else if (sum(is.na(c(a,b,c))) == 0) {
    return("You have provided lengths for all three sides. Only provide the length for two sides.")
  }
  else {
    try(a+b+c, return(" least one argument is not numeric. Only provide numbers."))
  }
  
  if (is.na(a)) {
    ans = (c^2 - b^2)^0.5
  }
  else if (is.na(b)) {
    ans = (-a^2 + c^2)^0.5
  }
  else if (is.na(c)) {
    ans = (b^2 + a^2)^0.5
  }
  return(ans)
}

pythagorean(a = 3, b = 4)
```

    ## [1] 5

``` r
pythagorean(b = 4, c = 5)
```

    ## [1] 3

``` r
pythagorean(a = 3, c = 5)
```

    ## [1] 4

``` r
pythagorean(a = 3, b = 4, c = 5)
```

    ## [1] "You have provided lengths for all three sides. Only provide the length for two sides."

``` r
pythagorean(a = 3)
```

    ## [1] "You have provided lengths for less than two sides. Only provide the length for two sides"

``` r
pythagorean(a = 3, b = "4")
```

    ## [1] " least one argument is not numeric. Only provide numbers."

Session info
------------

``` r
devtools::session_info()
```

    ## Session info --------------------------------------------------------------

    ##  setting  value                       
    ##  version  R version 3.3.1 (2016-06-21)
    ##  system   x86_64, mingw32             
    ##  ui       RTerm                       
    ##  language (EN)                        
    ##  collate  English_United States.1252  
    ##  tz       America/Chicago             
    ##  date     2017-02-01

    ## Packages ------------------------------------------------------------------

    ##  package    * version date       source        
    ##  assertthat   0.1     2013-12-06 CRAN (R 3.3.2)
    ##  backports    1.0.4   2016-10-24 CRAN (R 3.3.2)
    ##  colorspace   1.3-2   2016-12-14 CRAN (R 3.3.2)
    ##  DBI          0.5-1   2016-09-10 CRAN (R 3.3.2)
    ##  devtools     1.12.0  2016-06-24 CRAN (R 3.3.2)
    ##  digest       0.6.11  2017-01-03 CRAN (R 3.3.2)
    ##  dplyr      * 0.5.0   2016-06-24 CRAN (R 3.3.2)
    ##  evaluate     0.10    2016-10-11 CRAN (R 3.3.2)
    ##  ggplot2    * 2.2.1   2016-12-30 CRAN (R 3.3.2)
    ##  gtable       0.2.0   2016-02-26 CRAN (R 3.3.2)
    ##  htmltools    0.3.5   2016-03-21 CRAN (R 3.3.2)
    ##  knitr      * 1.15.1  2016-11-22 CRAN (R 3.3.2)
    ##  lazyeval     0.2.0   2016-06-12 CRAN (R 3.3.2)
    ##  magrittr     1.5     2014-11-22 CRAN (R 3.3.2)
    ##  memoise      1.0.0   2016-01-29 CRAN (R 3.3.2)
    ##  munsell      0.4.3   2016-02-13 CRAN (R 3.3.2)
    ##  plyr         1.8.4   2016-06-08 CRAN (R 3.3.2)
    ##  purrr      * 0.2.2   2016-06-18 CRAN (R 3.3.2)
    ##  R6           2.2.0   2016-10-05 CRAN (R 3.3.2)
    ##  Rcpp         0.12.8  2016-11-17 CRAN (R 3.3.2)
    ##  readr      * 1.0.0   2016-08-03 CRAN (R 3.3.2)
    ##  rmarkdown    1.3     2016-12-21 CRAN (R 3.3.2)
    ##  rprojroot    1.1     2016-10-29 CRAN (R 3.3.2)
    ##  scales       0.4.1   2016-11-09 CRAN (R 3.3.2)
    ##  stringi      1.1.2   2016-10-01 CRAN (R 3.3.2)
    ##  stringr      1.1.0   2016-08-19 CRAN (R 3.3.2)
    ##  tibble     * 1.2     2016-08-26 CRAN (R 3.3.2)
    ##  tidyr      * 0.6.0   2016-08-12 CRAN (R 3.3.2)
    ##  tidyverse  * 1.0.0   2016-09-09 CRAN (R 3.3.2)
    ##  withr        1.0.2   2016-06-20 CRAN (R 3.3.2)
    ##  yaml         2.1.14  2016-11-12 CRAN (R 3.3.2)
