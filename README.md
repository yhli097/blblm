# blblm

<!-- badges: start -->
<!-- badges: end -->

## Examples

``` r
library(blblm)
library(parallel)
cl = makeCluster(4)
fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, cluster = cl, method = "lmC")
coef(fit)
#> (Intercept)          wt          hp       wt:hp 
#> 48.88428523 -7.88702986 -0.11576659  0.02600976
confint(fit, c("wt", "hp")， cluser = cl)
#>           2.5%       97.5%
#> wt -10.7902240 -5.61586271
#> hp  -0.1960903 -0.07049867
sigma(fit)
#> [1] 1.838911
sigma(fit, confidence = TRUE)
#>    sigma      lwr      upr 
#> 1.838911 1.350269 2.276347
predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)))
#>        1        2 
#> 21.55538 18.80785
predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE)
#>        fit      lwr      upr
#> 1 21.55538 20.02457 22.48764
#> 2 18.80785 17.50654 19.71772
stopCluster(cl)

file_names <- file.path("inst/extdata/flights", list.files("inst/extdata/flights"))
m <- length(file_names)
n <- nrow(nycflights13::flights)
cl <- makeCluster(4)
fit <- blblm(arr_delay ~ dep_delay + flight + distance, file_names = file_names, n = n, B = 5000, cluster = cl)
sigma(fit, confidence = TRUE)
#>   sigma      lwr      upr 
#> 17.92746 17.84843 18.00715 
confint(fit, cluster = cl)
#>                    2.5%         97.5%
#> dep_delay  1.015971e+00  1.0199569652
#> flight     7.723085e-05  0.0001578702
#> distance  -2.529749e-03 -0.0023211470
predict(fit, data.frame(dep_delay = c(-10,-30), flight = c(1000, 500), distance = c(1500,3000) ), confidence = TRUE, cluster = cl)
#>       fit       lwr       upr
#> 1 -17.2709 -17.35900 -17.18289
#> 2 -41.3272 -41.54626 -41.11114
stopCluster(cl)



```
