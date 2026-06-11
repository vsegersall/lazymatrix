#--------------------------------------------------
# LazyMatrix ####
#--------------------------------------------------

#--------------------------------------------------
## t() ####
# LazyMatrix ####
#--------------------------------------------------

#--------------------------------------------------
## t() ####
#--------------------------------------------------
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

#--------------------------------------------------
## Matrix Multiplication ####
## Matrix Multiplication ####
#--------------------------------------------------

#--------------------------------------------------
### LazyMatrix & Vector ####
### LazyMatrix & Vector ####
#--------------------------------------------------
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

#--------------------------------------------------
### Vector & LazyMatrix ####
### Vector & LazyMatrix ####
#--------------------------------------------------
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

#--------------------------------------------------
### LazyMatrix & base::matrix ####
### LazyMatrix & base::matrix ####
#--------------------------------------------------
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

#--------------------------------------------------
## crossprod() ####
## crossprod() ####
#--------------------------------------------------
#' Crossproduct for LazyMatrix
#'
#' Computes the crossproduct of a LazyMatrix object as it's Gram matrix or computes the transposed matrix-vector multiplication.
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
    s <- 1 / x@col_scales
    S_inv <- Matrix::Diagonal(length(x@col_scales), s)
    c <- x@col_locations
    n <- nrow(x@data)
    xt_x <- base::crossprod(x@data)
    first_term <- S_inv %*% xt_x %*% S_inv
    cc_t <- c %*% t(c)
    second_term <- n * S_inv %*% cc_t %*% S_inv
    result <- first_term - second_term
    return(result)
  }

  # t(X) %*% y
  s <- 1 / x@col_scales
  c <- x@col_locations
  y <- as.vector(y)

  if (inherits(x@data, "dgCMatrix")) {
    sparse_data <- x@data
    result <- lazy_crossprod_vec_sp(sparse_data, s, c, y)
    result
  } else {
    dense_data <- x@data
    result <- lazy_crossprod_vec(dense_data, s, c, y)
    result
  }
})

#--------------------------------------------------
## svd() ####
## svd() ####
#--------------------------------------------------
#' @importFrom irlba irlba
#' @title Singular Value decomposition for LazyMatrix.
#'
#' @description Performs lazy SVD using irlba for partial Singular value decomposition on sparse matrices.
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

#--------------------------------------------------
## norm() ####
## norm() ####
#--------------------------------------------------
#' Computes the Frobenius norm of a LazyMatrix object.
#'
#' @param x A LazyMatrix object.
#'
#' @returns A numeric scalar representing the Frobenius norm of the matrix.
#' @export
#'
#' @examples
#' mat_a <- base::matrix(rnorm(50), nrow = 10, ncol = 5)
#' lazy_a <- LazyMatrix(mat_a, "sd", "mean")
#' norm(lazy_a)
setGeneric("norm", function(x) standardGeneric("norm"))
#' @rdname norm
#' @export
setMethod("norm", "LazyMatrix", function(x) {
  s <- 1 / x@col_scales
  c <- x@col_locations
  x_i <- Matrix::colSums(x@data)
  x_i_squared <- Matrix::colSums(x@data^2)
  n <- nrow(x)
  norm_squared <- base::sum(
    s^2 * x_i_squared + -2 * s^2 * c * x_i + s^2 * n * c^2
  )
  base::sqrt(norm_squared)
})

#--------------------------------------------------
# LazyColumn ####
#--------------------------------------------------

#--------------------------------------------------
## Vector Addition ####
#--------------------------------------------------
setMethod(
  "+",
  signature(e1 = "LazyColumn", e2 = "ANY"),
  function(e1, e2) {
    s <- 1 / e1@scale
    c <- e1@location
    first_term <- e1@data * s
    second_term <- c * s
    first_term - second_term + e2
  }
)

setMethod(
  "+",
  signature(e1 = "ANY", e2 = "LazyColumn"),
  function(e1, e2) {
    s <- 1 / e2@scale
    c <- e2@location
    first_term <- e2@data * s
    second_term <- c * s
    e1 + first_term - second_term
  }
)

#--------------------------------------------------
## Vector Subtraction ####
#--------------------------------------------------
setMethod(
  "-",
  signature(e1 = "LazyColumn", e2 = "ANY"),
  function(e1, e2) {
    s <- 1 / e1@scale
    c <- e1@location
    first_term <- e1@data * s
    second_term <- c * s
    first_term - second_term - e2
  }
)

setMethod(
  "-",
  signature(e1 = "ANY", e2 = "LazyColumn"),
  function(e1, e2) {
    s <- 1 / e2@scale
    c <- e2@location
    first_term <- e2@data * s
    second_term <- c * s
    e1 - first_term + second_term
  }
)
