# blblm

<!-- badges: start -->
<!-- badges: end -->

This is the final project of STA 141C 2020 Spring. The goal is to implement little bag of bootstrap in linear regression, to get the estimation as well as confidence interval of coefficients, sigma and prediction. In order to speed up, I use the parallel package to make parallel computation. Users can set up clusters and apply it in this process. Also, I provide method to help users distribute files into clusters to reduce the physical memory. What is more, users can choose method in linear regression like lm, lmR and lmC. The last one is a C++ function which allows a faster computation.
    
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


file_names <- file.path("vignettes/files", list.files("vignettes/files"))
fit <- blblm(y~x, files = file_names, B = 50, cluster = cl)
coef(fit)
#> (Intercept)           x 
#> -0.01803419  0.02715120 

stopCluster(cl)
```
For more detail and analysis, you may refer to the vignettes.
