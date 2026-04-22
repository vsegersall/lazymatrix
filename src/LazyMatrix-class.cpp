#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

//' Fast crossprod for LazyMatrix (dense case)
//' @param x Dense matrix
//' @param s Column scale inverse vector (1 / col_scales)
//' @param c Column location vector (col_locations)
//' @param y Vector to multiply
//' @return Vector result of t(X_tilde) * y
// [[Rcpp::export]]
arma::vec lazy_crossprod_vec(const arma::mat& x,
                              const arma::vec& s,
                              const arma::vec& c,
                              const arma::vec& y) {
    // Dimension validation
    if (x.n_rows != y.n_elem) {
        Rcpp::stop("Dimension mismatch: x has %d rows but y has %d elements",
                   x.n_rows, y.n_elem);
    }
    if (x.n_cols != s.n_elem) {
        Rcpp::stop("Dimension mismatch: x has %d cols but s has %d elements",
                   x.n_cols, s.n_elem);
    }
    if (x.n_cols != c.n_elem) {
        Rcpp::stop("Dimension mismatch: x has %d cols but c has %d elements",
                   x.n_cols, c.n_elem);
    }
    if (!y.is_vec()) {
        Rcpp::stop("y must be a vector, not a matrix");
    }

    // Compute t(X_tilde) * y = s .* (t(X) * y) - s .* c * sum(y)
    arma::vec s_inv = s;
    double sum_y = arma::accu(y);
    arma::vec xty = x.t() * y;
    int p = x.n_cols;
    arma::vec result(p);

    for (int j = 0; j < p; j++) {
        result(j) = s_inv(j) * xty(j) - s_inv(j) * c(j) * sum_y;
    }

    return result;
}

//' Fast crossprod for LazyMatrix (sparse case)
//' @param x Sparse matrix (dgCMatrix)
//' @param s Column scale inverse vector (1 / col_scales)
//' @param c Column location vector (col_locations)
//' @param y Vector to multiply
//' @return Vector result of t(X_tilde) * y
// [[Rcpp::export]]
arma::vec lazy_crossprod_vec_sp(const arma::sp_mat& x,
                                 const arma::vec& s,
                                 const arma::vec& c,
                                 const arma::vec& y) {
    // Dimension validation
    if (x.n_rows != y.n_elem) {
        Rcpp::stop("Dimension mismatch: x has %d rows but y has %d elements",
                   x.n_rows, y.n_elem);
    }
    if (x.n_cols != s.n_elem) {
        Rcpp::stop("Dimension mismatch: x has %d cols but s has %d elements",
                   x.n_cols, s.n_elem);
    }
    if (x.n_cols != c.n_elem) {
        Rcpp::stop("Dimension mismatch: x has %d cols but c has %d elements",
                   x.n_cols, c.n_elem);
    }
    if (!y.is_vec()) {
        Rcpp::stop("y must be a vector, not a matrix");
    }

    // Compute t(X_tilde) * y = s .* (t(X) * y) - s .* c * sum(y)
    arma::vec s_inv = s;
    double sum_y = arma::accu(y);
    arma::vec xty = x.t() * y;
    int p = x.n_cols;
    arma::vec result(p);

    for (int j = 0; j < p; j++) {
        result(j) = s_inv(j) * xty(j) - s_inv(j) * c(j) * sum_y;
    }

    return result;
}