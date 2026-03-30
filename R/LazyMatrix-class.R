#' @importFrom methods new

#' @name LazyMatrix-class
#' @title LazyMatrix S4 class
#'
#' @description An S4 class to represent a lazily transformed matrix with scaling and location parameters.
#'
#' @slot data The underlying matrix.
#' @slot col_scales Numeric vector of column scales.
#' @slot row_scales Numeric vector of row scales.
#' @slot col_locations Numeric vector of column locations.
#' @slot row_locations Numeric vector of row locations.
#' @export
#' @examples
#' mat <- matrix(1:6, nrow=2, ncol=3)
#' obj <- LazyMatrix(mat, "sd", "mean")
setClass(
  "LazyMatrix",
  slots = c(
    data = "ANY",
    col_scales = "numeric",
    row_scales = "numeric",
    col_locations = "numeric",
    row_locations = "numeric"
  ),
  prototype = list(
    data = matrix(0),
    col_scales = numeric(0),
    row_scales = numeric(0),
    col_locations = numeric(0),
    row_locations = numeric(0)
  )
)
# Helper ####

#' Constructs a LazyMatrix object.
#'
#' @param data a matrix object.
#' @param scale optional scaling parameter.
#' @param location optional location parameter.
#'
#' @returns A LazyMatrix object.
#' @export
#'
#' @examples
#' mat_a <- matrix(1:6, nrow=3, ncol=2)
#' lazy_a <- LazyMatrix(mat_a, scale="sd", location="mean")
#' lazy_a
LazyMatrix <- function(data, scale = NULL, location = NULL) {
  # code for constructing helper
  col_scales <- if (is.null(scale)) {
    numeric(0)
  } else if (scale == "sd") {
    base::apply(data, 2, stats::sd)
  } else {
    numeric(0)
  }
  row_scales <- if (is.null(scale)) {
    numeric(0)
  } else if (scale == "sd") {
    base::apply(data, 1, stats::sd)
  } else {
    numeric(0)
  }
  col_locations <- if (is.null(location)) {
    numeric(0)
  } else if (location == "mean") {
    Matrix::colMeans(data)
  } else {
    numeric(0)
  }
  row_locations <- if (is.null(location)) {
    numeric(0)
  } else if (location == "mean") {
    Matrix::rowMeans(data)
  } else {
    numeric(0)
  }
  new(
    "LazyMatrix",
    data = data,
    col_scales = col_scales,
    row_scales = row_scales,
    col_locations = col_locations,
    row_locations = row_locations
  )
}

# Validity checks on the arguments
setValidity("LazyMatrix", function(object) {
  # 1. check that data is a matrix: this is crucial for how the methods are
  ## implemented
  # 2. Check for location and scale
})

# nrow ####
setGeneric("nrow")
#' Returns the number of rows of the data matrix
#'
#' @param x A LazyMatrix object.
#'
#' @returns an integer of length 1 or NULL.
#' @export
#'
#' @examples
#' mat_a <- base::matrix(rep(1, 6), nrow=2, ncol=3)
#' lazy_a <- LazyMatrix(mat_a, "sd", "mean")
#' nrow(lazy_a)
setMethod("nrow", "LazyMatrix", function(x) {
  base::nrow(x@data)
})

# ncol ####
setGeneric("ncol")
#' Returns the number of columns of the data matrix
#'
#' @param x A LazyMatrix object.
#'
#' @returns an integer of length 1 or NULL.
#' @export
#'
#' @examples
#' mat_a <- base::matrix(rep(1, 6), nrow=2, ncol=3)
#' lazy_a <- LazyMatrix(mat_a, "sd", "mean")
#' ncol(lazy_a)
setMethod("ncol", "LazyMatrix", function(x) {
  base::ncol(x@data)
})

# dim ####
setGeneric("dim")
#' Returns the dimension of a LazyMarix Object.
#'
#' @param x A LazyMatrix object.
#'
#' @returns For an array (and hence in particular, for a matrix) dim retrieves the dim attribute of the object. It is NULL or a vector of mode integer. The replacement method changes the "dim" attribute (provided the new value is compatible) and removes any "dimnames" and "names" attributes.
#' @export
#'
#' @examples
#' mat_a <- base::matrix(rep(1, 6), nrow=2, ncol=3)
#' lazy_a <- LazyMatrix(mat_a, "sd", "mean")
#' dim(lazy_a)
setMethod("dim", "LazyMatrix", function(x) {
  base::dim(x@data)
})

# colnames ####
setGeneric("colnames")
#' Retrieve or set the row or column names of a LazyMatrix object.
#'
#' @param x A LazyMatrix object.
#'
#' @returns A character vector of column names, or NULL if the matrix has no column names.
#' @export
#'
#' @examples
#' mat_a <- base::matrix(rep(1, 6), nrow=2, ncol=3)
#' lazy_a <- LazyMatrix(mat_a, "sd", "mean")
#' colnames(lazy_a)
setMethod("colnames", "LazyMatrix", function(x) {
  base::colnames(x@data)
})

# as.matrix ####
#' Attempts to turn the data matrix to a scaled matrix-object.
#'
#' @param x A LazyMatrix object.
#'
#' @returns A matrix-object with scaled entries.
#' @export
#'
#' @examples
#' mat_a <- base::matrix(rep(1, 6), nrow=2, ncol=3)
#' lazy_a <- LazyMatrix(mat_a, "sd", "mean")
#' as.matrix(lazy_a)
setMethod("as.matrix", "LazyMatrix", function(x) {
  # X_tilde = X S^-1 - C S^-1
  s <- 1 / x@col_scales
  S_inv <- Matrix::Diagonal(length(x@col_scales), s)
  c <- x@col_locations
  first_term <- x@data %*% S_inv
  second_term <- Matrix::Matrix(
    c * s,
    nrow = nrow(first_term),
    ncol = length(s),
    byrow = TRUE
  )
  result <- first_term - second_term
  base::as.matrix(result)
})

# transpose ####
#' Given a LazyMatrix x, t returns the transpose of x.
#'
#' @param x A LazyMatrix object.
#'
#' @returns A LazyMatrix object with the transposed data matrix.
#' @export
#'
#' @examples
#' mat_a <- base::matrix(rep(1, 6), nrow=2, ncol=3)
#' lazy_a <- LazyMatrix(mat_a, "sd", "mean")
#' lazy_t <- t(lazy_a)
setMethod("t", "LazyMatrix", function(x) {
  x_transpose <- t(x@data)
  new(
    "LazyMatrix",
    data = x_transpose,
    col_scales = x@row_scales,
    row_scales = x@col_scales,
    col_locations = x@row_locations,
    row_locations = x@col_locations
  )
})

# colnames ####
setGeneric("colnames")
#' Retrieve or set the row or column names of a LazyMatrix object.
#'
#' @param x A LazyMatrix object.
#'
#' @returns A character vector of column names, or NULL if the matrix has no column names.
#' @export
#'
#' @examples
#' mat_a <- base::matrix(rep(1, 6), nrow=2, ncol=3)
#' lazy_a <- LazyMatrix(mat_a, "sd", "mean")
#' colnames(lazy_a)
setMethod("colnames", "LazyMatrix", function(x) {
  base::colnames(x@data)
})

# matrix multiplication ####
## LazyMatrix & vector
#' Matrix multiplication for LazyMatrix and vector
#'
#' @description Multiplies a LazyMatrix object by a vector.
#'
#' @param x A LazyMatrix object.
#' @param y A numeric vector.
#' @returns A numeric matrix.
#' @export
#' @examples
#' mat_a <- base::matrix(rep(1, 6), nrow=2, ncol=3)
#' b <- c(1, 2, 3)
#' lazy_a <- LazyMatrix(mat_a, "sd", "mean")
#' lazy_a %*% b
setMethod("%*%", c("LazyMatrix", "ANY"), function(x, y) {
  # X_tilde b = X S^-1 b - C S^-1 b
  s <- 1 / x@col_scales
  c <- x@col_locations
  x@data %*% (s * y) - sum(c * s * y)
})

## Vector & LazyMatrix
#' Matrix multiplication for vector and LazyMatrix
#'
#' @description Multiplies a LazyMatrix object by a vector.
#'
#' @param x A numeric vector.
#' @param y A LazyMatrix object.
#' @returns A Matrix object of class dgeMatrix.
#' @export
#' @examples
#' mat_a <- base::matrix(rep(1, 6), nrow=2, ncol=3)
#' b <- c(1, 2)
#' lazy_a <- LazyMatrix(mat_a, "sd", "mean")
#' b %*% lazy_a
setMethod("%*%", c("ANY", "LazyMatrix"), function(x, y) {
  # t(x) %*%  y
  # x is vector and y LazyMatrix
  # b^t X_tilde = b^t X S^1 - b^t C S^1
  s <- 1 / y@col_scales
  c <- y@col_locations
  #b_tx <- Matrix::Matrix(0, nrow = 1,
  #ncol = ncol(y@data), sparse = FALSE)
  b_tx <- base::matrix(0, nrow = 1, ncol = ncol(y@data))
  sum_b <- base::sum(x)
  for (j in seq_along(s)) {
    b_tx[j] <- s[j] * sum(x * y@data[, j]) - s[j] * sum_b * c[j]
  }
  b_tx
  # t(crossprod(y, x))
})

## LazyMatrix & matrix ####
#' Matrix multiplication for LazyMatrix and matrix-object.
#'
#' @description Multiplies a LazyMatrix object by a matrix
#'
#' @param x A LazyMatrix object.
#' @param y A matrix-object.
#' @returns A matrix-object with the product of the lazy and non lazy object.
#' @export
#' @examples
#' mat_a <- matrix(rep(1, 6), nrow = 2, ncol = 3)
#' lazy_a <- LazyMatrix(mat_a, "sd", "mean")
#' set.seed(123)
#' m <- matrix(rnorm(6), nrow = 3, ncol = 2)
#' lazy_a %*% m
setMethod("%*%", c("LazyMatrix", "matrix"), function(x, y) {
  # X_tilde M = X S^-1 M - C S^-1 M
  s <- 1 / x@col_scales
  c <- x@col_locations
  first_term <- x@data %*% (s * y)
  centering_row <- as.vector((c * s) %*% y)
  centering_matrix <- base::matrix(
    centering_row,
    nrow = nrow(x),
    ncol = ncol(y),
    byrow = TRUE
  )
  first_term - centering_matrix
})

# crossprod ####
#' Crossproduct for LazyMatrix
#'
#' Computes the crossproduct of a LazyMatrix object with itself or with another vector/matrix.
#'
#' @param x A LazyMatrix object.
#' @param y An optional numeric vector or matrix. If NULL, computes the Gram matrix of x.
#' @returns A matrix: the Gram matrix if y is NULL, otherwise the crossproduct result.
#' @export
#' @aliases crossprod,LazyMatrix-method
#' @examples
#' mat_a <- matrix(rep(1, 6), nrow=2, ncol=3)
#' b <- c(1, 2)
#' lazy_a <- LazyMatrix(mat_a, scale="sd", location="mean")
#' crossprod(lazy_a)
#' crossprod(lazy_a, b)
setMethod("crossprod", c("LazyMatrix", "ANY"), function(x, y = NULL) {
  if (is.null(y)) {
    # gram matrix
    # X_tilde^T X_tilde = S^-1 X^T X S^-1 - n S^-1 c c^T S^-1
    s <- 1 / x@col_scales
    S_inv <- Matrix::Diagonal(length(x@col_scales), s)
    c <- x@col_locations
    n <- nrow(x@data)
    xt_x <- base::crossprod(x@data)
    first_term <- S_inv %*% xt_x %*% S_inv
    cc_t <- c %*% t(c)
    second_term <- n * S_inv %*% cc_t %*% S_inv
    first_term - second_term
  } else {
    # t(X) %*% y
    # X_tilde^T b = S^1 X^T b - S^1 C^T b
    s <- 1 / x@col_scales
    c <- x@col_locations
    x_tb <- Matrix::Matrix(0, nrow = ncol(x@data), ncol = 1, sparse = FALSE)
    sum_y <- base::sum(y)
    for (j in seq_len(ncol(x@data))) {
      x_tb[j] <- s[j] * base::sum(x@data[, j] * y) - s[j] * c[j] * sum_y
    }
    x_tb
  }
})

# svd ####
#' @importFrom irlba irlba
#' @title Singular Value decomposition for LazyMatrix.
#'
#' @description Performs lazy SVD using irlba for svd on sparse matrices.
#' @param x A LazyMatrix object.
#' @param nu number of left singular vectors to estimate (defaults to nv).
#' @param nv  number of right singular vectors to estimate.
#' @returns A list with entries:
#'   \item{d}{max(nu, nv) approximate singular values}
#'   \item{u}{nu approximate left singular vectors (only when right_only=FALSE)}
#'   \item{v}{nv approximate right singular vectors}
#'   \item{iter}{The number of Lanczos iterations carried out}
#'   \item{mprod}{The total number of matrix vector products carried out}
#' @export
#'
#' @examples
#' set.seed(123)
#' mat_a <- matrix(rnorm(500), nrow = 50, ncol = 10)
#' lazy_a <-LazyMatrix(mat_a, scale = "sd", location = "mean")
#' S <- svd(lazy_a)
#' # Receive singular values with
#' S$d
setMethod("svd", "LazyMatrix", function(x, nu = min(n, p), nv = min(n, p)) {
  if (missing(nu)) {
    nu <- 5
  }
  if (missing(nv)) {
    nv <- 5
  }

  n <- nrow(x)
  p <- ncol(x)

  # Remove 1 dimension to be compatible with irlba
  max_k <- min(n, p) - 1
  nu <- min(nu, max_k)
  nv <- min(nv, max_k)

  # Adapter through S4 dispatch
  mult_func <- function(x, y) {
    result <- x %*% y
    as.vector(result) # Create correct type for irlba
  }
  irlba::irlba(x, nu = nu, nv = nv, mult = mult_func)
})

# prcomp ####
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

# norm ####
setGeneric("norm", function(x) standardGeneric("norm"))
setMethod("norm", "LazyMatrix", function(x) {
  s <- 1 / x@col_scales
  c <- x@col_locations
  x_i <- Matrix::colSums(x@data)
  x_i_squared <- Matrix::colSums(x@data^2)
  n <- nrow(x)
  sum <- 0
  norm_squared <-
    for (i in seq(s)) {
      sum <- sum +
        s[i]^2 * x_i_squared[i] +
        -1 * s[i]^2 * 2 * c[i] * x_i[i] +
        s[i]^2 * n * c[i]^2
    }
  sqrt(sum)
})

# LSQR ####
setGeneric("lsqr", function(x, y, ...) standardGeneric("lsqr"))
setMethod("lsqr", c("LazyMatrix", "ANY"), function(x, y) {
  A <- x
  b <- y
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
    if (sqrt(sum(residual^2)) < tolerance) {
      convergence <- TRUE
    }
  }
  x_0
})
