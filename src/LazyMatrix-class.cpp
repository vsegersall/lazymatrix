#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;
// [[Rcpp::export]]
arma::vec lazy_crossprod_vec(arma::mat x,
                            arma::vec s,
                            arma::vec c,
                            arma::vec y) {
    arma::vec s_inv = 1/s;
    double sum_y = arma::sum(y);
    arma::vec xty = x.t() * y;
    int p = x.n_cols;
    arma::vec xtb(p);
    for (int i = 0;  i < p; i++){
        xtb(i) = s_inv(i) * xty(i) - s_inv(i) * c(i) * sum_y;
    }
  return xtb;
}

// [[Rcpp::export]]
arma::vec lazy_crossprod_vec_sp(arma::sp_mat x,
                                arma::vec s,
                                arma::vec c,
                                arma::vec y) {
    arma::vec s_inv = 1/s;
    double sum_y = arma::sum(y);
    arma::vec xty = x.t() * y;
    int p = x.n_cols;
    arma::vec xtb(p);
    for (int i = 0;  i < p; i++){
        xtb(i) = s_inv(i) * xty(i) - s_inv(i) * c(i) * sum_y;
    }
  return xtb;
}