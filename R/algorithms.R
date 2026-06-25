#--------------------------------------------------
# prcomp() ####
#--------------------------------------------------
#' Performs a principal component analysis on the LazyMatrix object using irlba:s sparse svd.
#'
#' @param x a LazyMatrix object.
#' @param retx a logical value indicating whether the rotated variables should be returned.
#' @param tol a value indicating the magnitude below which components should be omitted. (Components are omitted if their standard deviations are less than or equal to tol times the standard deviation of the first component.) With the default null setting, no components are omitted (unless rank. is specified less than min(dim(x)).). Other settings for tol could be tol = 0 or tol = sqrt(.Machine$double.eps), which would omit essentially constant components.
#' @param rank. optionally, a number specifying the maximal rank, i.e., maximal number of principal components to be used. Can be set as alternative or in addition to tol, useful notably when the desired rank is considerably smaller than the dimensions of the matrix.
#' @param ... Additional arguments passed to underlying methods.

#' @return A list of class \code{\"prcomp\"} containing:
#'   \item{sdev}{The standard deviations of the principal components (i.e., the square roots of the eigenvalues of the covariance/correlation matrix, calculated using the singular values of the data matrix).}
#'   \item{rotation}{The matrix of variable loadings (columns are eigenvectors).}
#'   \item{x}{If \code{retx} is TRUE, the value of the rotated data (centered and optionally scaled, multiplied by the rotation matrix).}
#'   \item{center}{The centering used, or \code{FALSE}.}
#'   \item{scale}{The scaling applied to the data, or \code{FALSE}}
#' @export
#'
#' @examples
#' set.seed(123)
#' mat_a <- matrix(rnorm(500), nrow=50, ncol=10)
#' lazy_a <- LazyMatrix(mat_a, "sd", "mean")
#' pca_lazy <- prcomp(lazy_a)
setMethod(
  "prcomp",
  "LazyMatrix",
  function(x, retx = TRUE, tol = NULL, rank. = NULL, ...) {
    cen <- x@col_locations
    sc <- x@col_scales
    if (any(sc == 0)) {
      stop("cannot rescale a constant/zero column to unit variance")
    }
    n <- nrow(x)
    p <- ncol(x)
    k <- if (!is.null(rank.)) {
      stopifnot(length(rank.) == 1, is.finite(rank.), as.integer(rank.) > 0)
      min(as.integer(rank.), n, p)
    } else {
      min(n, p)
    }
    k <- min(k, min(n, p) - 1)
    s <- svd(x, nu = 0, nv = k)
    j <- base::seq_len(k)
    s$d <- s$d / base::sqrt(max(1, n - 1))
    if (!is.null(tol)) {
      rank <- sum(s$d > (s$d[1L] * tol))
      if (rank < k) {
        k <- rank
        j <- seq_len(k)
        s$v <- s$v[, j, drop = FALSE]
      }
    }
    center <- if (length(cen) > 0) cen else FALSE
    scale <- if (length(sc) > 0) sc else FALSE
    dimnames(s$v) <- list(colnames(x), paste0("PC", j))
    r <- list(sdev = s$d, rotation = s$v, center = center, scale = scale)
    if (retx) {
      r$x <- x %*% s$v
    }
    base::class(r) <- "prcomp"
    r
  }
)

#--------------------------------------------------
# lsqr() ####
#--------------------------------------------------
#' Performs least squares estimation on LazyMatrix object using the iterative lsqr algorithm.
#'
#' @param x A LazyMatrix object.
#' @param y A response vector.
#' @param ... Additional arguments (currently unused).
#'
#' @returns A Matrix-object with the regression coefficients of the covariates.
#' @export
#'
#' @examples
#' set.seed(123)
#' mat_a <- base::matrix(rnorm(500), nrow = 50, ncol = 10)
#' lazy_a <- LazyMatrix(mat_a, "sd", "mean")
#' response_vector <- rnorm(nrow(mat_a))
#' lsqr(lazy_a, response_vector)
setGeneric("lsqr", function(x, y, ...) standardGeneric("lsqr"))
#' @rdname lsqr
#' @export
setMethod("lsqr", c("LazyMatrix", "ANY"), function(x, y) {
  A <- x
  b <- Matrix::Matrix(y)
  convergence <- FALSE
  iter <- 0
  max_iter <- 100
  tolerance <- 1e-6

  # 1. Initialization
  beta_1 <- sqrt(sum(b^2))
  u_1 <- b / beta_1
  A_tu_1 <- crossprod(A, u_1)
  alpha_1 <- sqrt(sum(A_tu_1^2))
  v_1 <- A_tu_1 / alpha_1
  w_1 <- v_1
  x_0 <- rep(0, ncol(A))
  phi_1_bar <- beta_1
  rho_1_bar <- alpha_1

  # 2. For i=1,2,... 3 repeat steps 3-6
  while (!convergence && iter < max_iter) {
    iter <- iter + 1

    # 3. Continue the bidiagonalization
    beta_u <- A %*% v_1 - alpha_1 * u_1
    beta_2 <- sqrt(sum(beta_u^2))
    u_2 <- beta_u / beta_2
    alpha_v <- crossprod(A, u_2) - beta_2 * v_1
    alpha_2 <- sqrt(sum(alpha_v^2))
    v_2 <- alpha_v / alpha_2

    # 4. Construct and apply next orthogonal transformation
    rho_1 <- sqrt(rho_1_bar^2 + beta_2^2)
    c_1 <- rho_1_bar / rho_1
    s_1 <- beta_2 / rho_1
    theta_2 <- s_1 * alpha_2
    rho_2_bar <- -c_1 * alpha_2
    phi_1 <- c_1 * phi_1_bar
    phi_2_bar <- s_1 * phi_1_bar

    # 5. Update x, w
    x_1 <- x_0 + phi_1 / rho_1 * w_1
    w_2 <- v_2 - theta_2 / rho_1 * w_1

    # reset the loop-variables
    beta_1 <- beta_2
    u_1 <- u_2
    alpha_1 <- alpha_2
    v_1 <- v_2
    rho_1_bar <- rho_2_bar
    phi_1_bar <- phi_2_bar
    x_0 <- x_1
    w_1 <- w_2

    # 6. Check for convergence
    residual <- b - A %*% x_0
    residual_norm <- Matrix::norm(residual, "F")
    if (
      residual_norm <=
        tolerance *
          norm(A) *
          Matrix::norm(x_0, "F") +
          tolerance *
            Matrix::norm(b, "F")
    ) {
      convergence <- TRUE
    }
  }
  x_0
})
