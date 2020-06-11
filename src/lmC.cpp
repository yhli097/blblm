#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

//' Compute coefficients and sigma in a weighted linear regression
//'
//' In order to speed up the function of lm in R, I write this Rcpp file to solve
//' coefficients and sigma on my own. Firstly, it needs RcppArmadillo to make basic
//' linear computation. We should first input design matrix X, response vector y,
//' weights diagnoal matrix W, the size of whole data and the rank p.
//' Then coef = (X^T W X)^(-1)(X^T W y) and sigma = sqrt(W * (X*coef - y)^2/(n-p)).
//'
//'
//' @param X design matrix with rank p.
//' @param y response vector
//' @param W weighted diagonal matrix
//' @param n the size of whole data
//' @param p the rank of X
//' @return a list including coef and sigma
//' @export
//[[Rcpp::export()]]
Rcpp::List lmC(arma::mat X, arma::colvec y, arma::mat W, int n, int p) {
  arma::colvec coef = arma::solve(X.t()*W*X, X.t()*W*y) ; // fit model y ~ X with weights
  arma::colvec res  = y - X*coef;           // residuals
  arma::colvec resw = W * res;       //weighted residuals

  // weighted MSE
  double sigma = std::inner_product(resw.begin(), resw.end(), res.begin(), 0.0)/(n - p);
  sigma = sqrt(sigma);

  return Rcpp::List::create(Rcpp::Named("coef") = coef,
                            Rcpp::Named("sigma") = sigma);
}