#' @import purrr
#' @import stats
#' @import parallel
#' @importFrom readr cols
#' @importFrom readr read_csv
#' @importFrom magrittr %>%
#' @aliases NULL
#' @details
#' Linear Regression with Little Bag of Bootstraps
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))

#' the main function to implement linear regression with little bag of bootstraps
#'
#' This function will devide the original data into several subsamples
#' and do little bag of bootsrap.
#'
#' @param formula formula for linear regression
#' @param files allow user to inpute the path of files to do distribution, only support csv file
#' @param n the size of whole data, if NULL, we will assume the size of subdata is even
#' @param data data for linear regression
#' @param m the number of subsamples divided by the whole data
#' @param B the time of little bag of bootstrap
#' @param method the method to compute coef and sigma, choice: "lm","lmR","lmC", default as "lm"
#' @param cluster a set of copies of R running in parallel, default as NULL only one core
#' @param seed set.seed to make the result reproducible, default as 141
#'
#' @return list including estimates of coefficients and sigma
#' in each time of little bag of bootsrap in each subsample
#' and the formula.
#' @export
#' @examples
#' blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
blblm <- function(formula, files = NULL, n = NULL, data = NULL, m = 10, B = 5000, method = "lm", cluster = NULL, seed = 141) {
  set.seed(seed)
  if(is.null(files)){
    data_list <- split_data(data, m)

    if(is.null(cluster)) {
      estimates <- map(
        data_list,
        ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B, method = method))
    }

    else {
      # cl <- makeCluster(n_cores)
      clusterExport(cluster, c("lm_each_subsample","lm_each_boot","lm1","blbcoef","blbsigma", "lmC"),envir=environment())
      #if(method == "lmC") clusterEvalQ(cluster, { Rcpp::sourceCpp("src/lmC.cpp"); })
      estimates <- parLapply(cluster, data_list,
            function(x) lm_each_subsample(formula = formula, data = x, n = nrow(data), B = B, method = method))
      # stopCluster(cl)
    }
  }
  else{
    if(is.null(cluster)){
      estimates <- lapply(files, function(fname) {
        df <- read_csv(fname, col_types = cols())
        if(is.null(n)) n <- nrow(df) * length(files)
        lm_each_subsample(formula = formula, data = df, n = n, B = B, method = method)
      })
    }
    else{
      # we will to manually load tidyverse in the workers
      invisible(clusterEvalQ(cluster, {
        library(tidyverse)
        NULL
      }))
      clusterExport(cluster, c("lm_each_subsample","lm_each_boot","lm1","blbcoef","blbsigma", "lmC"),envir=environment())
      #if(method == "lmC") clusterEvalQ(cluster, { Rcpp::sourceCpp("src/lmC.cpp"); })

      estimates <- parLapply(cluster, files, function(fname) {
        df <- read_csv(fname, col_types = cols())
        if(is.null(n)) n <- nrow(df) * length(files)
        lm_each_subsample(formula = formula, data = df, n = n, B = B, method = method)
      })
    }

  }

  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}


#' split data into m parts of approximated equal sizes
#'
#' @param data the original data
#' @param m the number of subsamples
#'
#' @return the list of subsamples
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}


#' compute the estimates for each little bag of bootstrap
#'
#' @param formula the formula for linear regression
#' @param data the subsample data
#' @param n the row number of the original data
#' @param B the times of little bag of bootstrap
#' @param method the method to compute coef and sigma, choice: "lm","lmR","lmC", default as "lm"
#'
#' @return the estimate coefficients and sigma
#' in B little bag of bootstraps for one subsample
lm_each_subsample <- function(formula, data, n, B, method) {
  replicate(B, lm_each_boot(formula, data, n, method), simplify = FALSE)
}


#' compute the regression estimates for a blb dataset given a generated weight
#'
#' @param formula the formula for linear regression
#' @param data the subsample data
#' @param n the row number of original data
#' @param method the method to compute coef and sigma, choice: "lm","lmR","lmC", default as "lm"
#'
#' @return the estimate of coefficients and sigma in one weighted linear regression
lm_each_boot <- function(formula, data, n, method) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  lm1(formula, data, freqs, method)
}


#' estimate the regression estimates based on given the number of repetitions
#' @param formula the formula for linear regression
#' @param data the subsample data
#' @param freqs the weights for each case in subsample data
#' @param method the method to compute coef and sigma, choice: "lm","lmR","lmC", default as "lm"
#'
#' @return the estimate of coefficients and sigma in one weighted linear regression
lm1 <- function(formula, data, freqs, method) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wront variable from the global scope.
  environment(formula) <- environment()
  if(method == "lm"){
    fit <- lm(formula, data, weights = freqs)
    list(coef = blbcoef(fit), sigma = blbsigma(fit))
  }
  else{
    X <- model.matrix(formula, data)
    name <- colnames(X)
    p <- length(name)
    y <- model.response(model.frame(formula, data))
    W <- diag(as.vector(freqs))
    n <- sum(freqs)
    if(method == "lmR"){
      coef_lmR <- solve(t(X)%*%W%*%X, t(X)%*%W%*%y)
      coef_lmR <- as.numeric(coef_lmR)
      names(coef_lmR) <- name

      sigma_lmR <- sqrt(sum(W %*%(y-X %*% coef_lmR)^2)/(n-p))

      list(coef = coef_lmR, sigma = sigma_lmR)
    }
    else if(method == "lmC"){
      fitC <- lmC(X,y,W,n,p)
      fitC$coef <- as.numeric(fitC$coef)
      names(fitC$coef) <- name
      fitC
    }
    else {
      stop("There are only three methods: lm, lmR, lmC")
    }
  }

}


#' compute the coefficients from fit
#' @param fit the result of lm()
#'
#' @return the coef of fit
blbcoef <- function(fit) {
  coef(fit)
}


#' compute sigma from fit
#' @param fit the result of lm()
#'
#' @return sigma from fit
blbsigma <- function(fit) {
  p <- fit$rank
  y <- model.extract(fit$model, "response")
  e <- fitted(fit) - y
  w <- fit$weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}

#' @importFrom utils capture.output
#' @export
#' @method print blblm
print.blblm <- function(x, ...) {
  cat("blblm model:", capture.output(x$formula))
  cat("\n")
}


#' @export
#' @method sigma blblm
sigma.blblm <- function(object, confidence = FALSE, level = 0.95, cluster = NULL, ...) {
  est <- object$estimates
  if(is.null(cluster)){
    sigma <- mean(map_dbl(est, ~ mean(map_dbl(., "sigma"))))
    if (confidence) {
      alpha <- 1 - level
      limits <- est %>%
        map_mean(~ quantile(map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)) %>%
        set_names(NULL)
      return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
    } else {
      return(sigma)
    }
  }
  else {
    # cl <- makeCluster(n_cores)
    clusterExport(cluster, c("map_dbl"),envir=environment())
    sigma <- mean(parSapply(cluster, est, function(x) mean(map_dbl(x, "sigma"))))
    if (confidence) {
      alpha <- 1 - level
      limits <- parSapply(cluster, est,
                  function(x) quantile(map_dbl(x, "sigma"), c(alpha / 2, 1 - alpha / 2), na.rm = TRUE))
      limits <- parApply(cluster, limits, 1, mean)
      limits <- as.numeric(limits)
      return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
    } else {
      return(sigma)
    }
    # stopCluster(cl)
  }

}

#' @export
#' @method coef blblm
coef.blblm <- function(object, cluster = NULL,...) {
  est <- object$estimates
  if(is.null(cluster)){
    map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
  }
  else {
    #cl <- makeCluster(n_cores)
    clusterExport(cluster, c("map_cbind","%>%","map","reduce"),envir=environment())
    coefs = parSapply(cluster, est, function(x) apply(map_cbind(x,"coef"),1,mean))
    parApply(cluster, coefs, 1, mean)
    #stopCluster(cl)
    #coefs
  }
}

#' @export
#' @method confint blblm
confint.blblm <- function(object, parm = NULL, level = 0.95, cluster = NULL,...) {
  if (is.null(parm)) {
    parm <- attr(terms(object$formula), "term.labels")
  }
  alpha <- 1 - level
  est <- object$estimates
  if(is.null(cluster)){
    out <- map_rbind(parm, function(p) {
      map_mean(est, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2), na.rm = TRUE))
    })
  }
  else{
    #cl <- makeCluster(n_cores)
    clusterExport(cluster, c("map_dbl"),envir=environment())
    get_interval <- function(parm){
      coef_int = sapply(est, function(x) quantile(map_dbl(x, list("coef", parm)),c(alpha / 2, 1 - alpha / 2), na.rm = TRUE))
      apply(coef_int, 1, mean)
    }
    out <- parSapply(cluster, parm, get_interval)
    out <- t(out)
    #stopCluster(cl)
  }
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}

#' @export
#' @method predict blblm
predict.blblm <- function(object, new_data, confidence = FALSE, level = 0.95, cluster = NULL,...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms.formula(object$formula,data = new_data), "term.labels")), new_data)
  if(is.null(cluster)){
    if (confidence) {
      map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
                 apply(1, mean_lwr_upr, level = level) %>%
                 t())
    } else {
      map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
    }
  }
  else{
    #cl <- makeCluster(n_cores)
    if (confidence) {
      alpha <- 1 - level
      pred_get_interval = function(x){
        t(apply(sapply(x, function(y) X %*% y$coef),1,
            function(y) c( mean(y), quantile(y,c(alpha / 2, 1 - alpha / 2), na.rm = TRUE))))}
      intervals <- parLapply(cluster, est, pred_get_interval)
      result <- Reduce("+",intervals)/length(intervals)
      rownames(result) <- seq_len(dim(result)[1])
      colnames(result) <- c("fit", "lwr", "upr")
    } else {
      pred <- parSapply(cluster, est, function(x) apply(sapply(x, function(y) X %*% y$coef),1,mean))
      result <- parApply(cluster, pred, 1, mean)
      names(result) <- seq_len(length(result))
    }
    #stopCluster(cl)
    return(result)
  }

}

#' get the mean as well as quantile from a data
#' @param x list of data
#' @param level the confidence level of confidence interval
mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2), na.rm = TRUE) %>% set_names(c("lwr", "upr")))
}

#' get the mean of the estimates in subsamples by column
#' @param .x lits of data
#' @param .f function
#' @param ... other arguments
#'
#' @return the mean of the list for each element
map_mean <- function(.x, .f, ...) {
  (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

#' make a list as a form of column bind
#' @param .x list of data
#' @param .f function
#' @param ... other arguments
#'
#' @return the column bind of the list
map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}

#' make a list as a form of row bind
#' @param .x list of data
#' @param .f function
#' @param ... other arguments
#'
#' @return the row bind of the list
map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}
