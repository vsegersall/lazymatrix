#--------------------------------------------------
# LazyMatrix ####
#--------------------------------------------------

#--------------------------------------------------
## t() ####
#--------------------------------------------------
#' Given a \code{LazyMatrix} x, t returns the transpose of x.
#'
#' @param x A \code{LazyMatrix} object.
#'
#' @returns A \code{LazyMatrix} object with the transposed data matrix.
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
#--------------------------------------------------

#--------------------------------------------------
### LazyMatrix & Vector ####
#--------------------------------------------------
#' Matrix multiplication for \code{LazyMatrix} and vector
#'
#' @description Multiplies a \code{LazyMatrix} object by a vector.
#'
#' @param x A \code{LazyMatrix} object.
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
#--------------------------------------------------
#' Matrix multiplication for vector and \code{LazyMatrix}
#'
#' @description Multiplies a \code{LazyMatrix} object by a vector.
#'
#' @param x A numeric vector.
#' @param y A \code{LazyMatrix} object.
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
#--------------------------------------------------
#' Matrix multiplication for \code{LazyMatrix} and matrix-object.
#'
#' @description Multiplies a \code{LazyMatrix} object by a matrix
#'
#' @param x A \code{LazyMatrix} object.
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
#--------------------------------------------------
#' Crossproduct for \code{LazyMatrix}
#'
#' Computes the crossproduct of a \code{LazyMatrix} object as it's Gram matrix or computes the transposed matrix-vector multiplication.
#'
#' @param x A \code{LazyMatrix} object.
#' @param y An optional numeric vector or matrix. If NULL, computes the Gram matrix of x.
#' @returns A matrix: the Gram matrix if y is NULL, otherwise the crossproduct result.
#' @export
#' @aliases crossprod,\code{LazyMatrix}-method
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
#--------------------------------------------------
#' @importFrom irlba irlba
#' @title Singular Value decomposition for \code{LazyMatrix}.
#'
#' @description Performs lazy SVD using irlba for partial Singular value decomposition on sparse matrices.
#' @param x A \code{LazyMatrix} object.
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
#--------------------------------------------------
#' Computes the Frobenius norm of a \code{LazyMatrix} object.
#'
#' @param x A \code{LazyMatrix} object.
#'
#' @returns A numeric scalar representing the Frobenius norm of the matrix.
#' @export
#'
#' @examples
#' mat_a <- base::matrix(rnorm(50), nrow = 10, ncol = 5)
#' lazy_a <- LazyMatrix(mat_a, "sd", "mean")
#' norm(lazy_a)
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
#' Vector Addition between \code{LazyColumn} and regular vector
#'
#' @description Sums a \code{LazyColumn} vector and a regular base vector.
#'
#' @param e1 A \code{LazyColumn} object.
#' @param e2 A numeric vector.
#' @returns A numeric vector.
#' @export
#' @examples
#' mat_a <- base::matrix(rnorm(12), nrow=3, ncol=4)
#' b <- rnorm(nrow(mat_a))
#' lazy_a <- LazyMatrix(mat_a, "sd", "mean")
#' lazy_col <- lazy_a[,2]
#' lazy_col + b
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

#' Vector Addition between regular vector and \code{LazyColumn}
#'
#' @description Sums a regular base vector and a \code{LazyColumn} vector.
#'
#' @param e1 A numeric vector.
#' @param e2 A \code{LazyColumn} object.
#' @returns A numeric vector.
#' @export
#' @examples
#' mat_a <- base::matrix(rnorm(12), nrow=3, ncol=4)
#' b <- rnorm(nrow(mat_a))
#' lazy_a <- LazyMatrix(mat_a, "sd", "mean")
#' lazy_col <- lazy_a[, 2]
#' b + lazy_col
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

#' Vector Addition between two \code{LazyColumn} vectors.
#'
#' @description Sums two \code{LazyColumn} vectors.
#'
#' @param e1 A \code{LazyColumn} object.
#' @param e2 A \code{LazyColumn} object.
#' @returns A numeric vector.
#' @export
#' @examples
#' mat_a <- base::matrix(rnorm(12), nrow=3, ncol=4)
#' lazy_a <- LazyMatrix(mat_a, "sd", "mean")
#' lazy_col <- lazy_a[,2]
#' lazy_col_2 <- lazy_a[, 3]
#' lazy_col_2 + lazy_col
setMethod(
  "+",
  signature(e1 = "LazyColumn", e2 = "LazyColumn"),
  function(e1, e2) {
    s_1 <- 1 / e1@scale
    c_1 <- e1@location
    s_2 <- 1 / e2@scale
    c_2 <- e2@location
    first_term <- e1@data * s_1 + e2@data * s_2
    second_term <- c_1 * s_1 + c_2 * s_2
    first_term - second_term
  }
)

#--------------------------------------------------
## Vector Subtraction ####
#--------------------------------------------------
#' Vector subtraction between a \code{LazyColumn} vector and a regular R vector.
#'
#' @description Subtracts a numeric R vector from a \code{LazyColumn} vector.
#'
#' @param e1 A \code{LazyColumn} object.
#' @param e2 A numeric vector.
#' @returns A numeric vector.
#' @export
#' @examples
#' mat_a <- base::matrix(rnorm(12), nrow=3, ncol=4)
#' b <- rnorm(nrow(mat_a))
#' lazy_a <- LazyMatrix(mat_a, "sd", "mean")
#' lazy_col <- lazy_a[,2]
#' lazy_col - b
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

#' Vector Subtraction between regular vector and \code{LazyColumn}
#'
#' @description Subtracts a \code{LazyColumn} vector from a base vector.
#'
#' @param e1 A \code{LazyColumn} object.
#' @param e2 A numeric vector.
#' @returns A numeric vector.
#' @export
#' @examples
#' mat_a <- base::matrix(rnorm(12), nrow=3, ncol=4)
#' b <- rnorm(nrow(mat_a))
#' lazy_a <- LazyMatrix(mat_a, "sd", "mean")
#' lazy_col <- lazy_a[, 2]
#' b - lazy_col
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

#' Vector Subtraction between two \code{LazyColumn} vectors.
#'
#' @description Subtracts a \code{LazyColumn} vector from another \code{LazyColumn} vector.
#'
#' @param e1 A \code{LazyColumn} object.
#' @param e2 A \code{LazyColumn} object.
#' @returns A numeric vector.
#' @export
#' @examples
#' mat_a <- base::matrix(rnorm(12), nrow = 3, ncol = 4)
#' lazy_a <- LazyMatrix(mat_a, "sd", "mean")
#' lazy_col <- lazy_a[, 2]
#' lazy_col_2 <- lazy_a[, 3]
#' lazy_col_2 - lazy_col
setMethod(
  "-",
  signature(e1 = "LazyColumn", e2 = "LazyColumn"),
  function(e1, e2) {
    s_1 <- 1 / e1@scale
    c_1 <- e1@location
    s_2 <- 1 / e2@scale
    c_2 <- e2@location
    first_term <- e1@data * s_1 - e2@data * s_2
    second_term <- c_1 * s_1 - c_2 * s_2
    first_term - second_term
  }
)

#--------------------------------------------------
## Scalar Multiplication and Element wise vector multiplication ####
#--------------------------------------------------
#' Multiply a LazyColumn by a numeric scalar or vector
#'
#' Computes the element-wise multiplication of a \code{LazyColumn} object
#' with a numeric value, preserving the lazy structure.
#' If \code{e2} is a scalar, scalar multiplication is performed.
#' If \code{e2} is a vector of the same length as the column, element-wise
#' multiplication is performed.
#'
#' @param e1 A \code{LazyColumn} object.
#' @param e2 A numeric scalar or vector.
#'
#' @return A numeric vector with the resulting vector.
#'
#' @examples
#' mat_a <- base::matrix(rnorm(12), nrow=3, ncol=4)
#' X <- LazyMatrix(mat_a, "sd", "mean")
#' lazy_col <- X[, 1]
#' lazy_col * 2
#' lazy_col * rnorm(nrow(X))
setMethod(
  "*",
  signature(e1 = "LazyColumn", e2 = "numeric"),
  function(e1, e2) {
    s <- 1 / e1@scale
    c <- e1@location
    if (length(e2) == 1) {
      # scalar multiplication
      first_term <- e1@data * s
      second_term <- c * s
      first_term * e2 - second_term * e2
    } else {
      # Element-wise multiplication
      first_term <- e1@data * s * e2
      second_term <- c * s * e2
      first_term - second_term
    }
  }
)

#' Multiply a numeric scalar or vector by a LazyColumn
#'
#' Computes the element-wise multiplication of a \code{LazyColumn} object
#' with a numeric value, preserving the lazy structure.
#' If \code{e1} is a scalar, scalar multiplication is performed.
#' If \code{e1} is a vector of the same length as the column, element-wise
#' multiplication is performed.
#'
#' @param e1 A numeric scalar or vector.
#' @param e2 A \code{LazyColumn} object.
#'
#' @return A numeric vector with the resulting vector.
#'
#' @examples
#' mat_a <- base::matrix(rnorm(12), nrow=3, ncol=4)
#' X <- LazyMatrix(mat_a, "sd", "mean")
#' lazy_col <- X[, 1]
#' 2* lazy_col
#' rnorm(nrow(X)) * lazy_col
setMethod(
  "*",
  signature(e1 = "numeric", e2 = "LazyColumn"),
  function(e1, e2) {
    s <- 1 / e2@scale
    c <- e2@location
    if (length(e1) == 1) {
      # scalar multiplication
      first_term <- e2@data * s
      second_term <- c * s
      e1 * first_term - e1 * second_term
    } else {
      # element-wise multiplication
      first_term <- e1 * e2@data * s
      second_term <- e1 * c * s
      first_term - second_term
    }
  }
)

#' Multiply element-wise two LazyColumn vectors
#'
#' Computes the element-wise multiplication of two \code{LazyColumn} objects
#' , preserving the lazy structure.
#'
#' @param e1 A \code{LazyColumn} object.
#' @param e2 A \code{LazyColumn} object.
#'
#' @return A numeric vector with the resulting vector.
#'
#' @examples
#' mat_a <- base::matrix(rnorm(12), nrow=3, ncol=4)
#' X <- LazyMatrix(mat_a, "sd", "mean")
#' lazy_col <- X[, 1]
#' lazy_col_2 <- X[, 2]
#' lazy_col_2 * lazy_col
setMethod(
  "*",
  signature(e1 = "LazyColumn", e2 = "LazyColumn"),
  function(e1, e2) {
    s_1 <- 1 / e1@scale
    c_1 <- e1@location
    s_2 <- 1 / e2@scale
    c_2 <- e2@location
    first_term <- e1@data * e2@data
    second_term <- c_1 * e2@data
    third_term <- e1@data * c_2
    fourth_term <- c_1 * c_2
    s_1 * s_2 * (first_term - second_term - third_term + fourth_term)
  }
)

#--------------------------------------------------
## Dot product ####
#--------------------------------------------------
#' Perform the dot product between a LazyColumn and a numeric vector
#'
#' Computes the inner product of a \code{LazyColumn} object
#' and a numeric vector.
#' If \code{y} is a scalar, scalar multiplication is performed.
#' If \code{y} is a vector of the same length as the column, the dot product
#' is performed.
#'
#' @param x A \code{LazyColumn} object.
#' @param y A numeric scalar or vector.
#'
#' @return A numeric value with the resulting scalar.
#'
#' @examples
#' mat_a <- base::matrix(rnorm(12), nrow=3, ncol=4)
#' X <- LazyMatrix(mat_a, "sd", "mean")
#' b <- rnorm(nrow(X))
#' lazy_col <- X[, 1]
#' lazy_col %*% b
setMethod(
  "%*%",
  signature(x = "LazyColumn", y = "numeric"),
  function(x, y) {
    s <- 1 / x@scale
    c <- x@location
    if (length(y) == 1) {
      x * y
    } else {
      first_term <- s * x@data %*% y
      second_term <- s * c * sum(y)
      first_term - second_term
    }
  }
)

#' Perform the dot product between a numeric vectorand a LazyColumn
#'
#' Computes the inner product of a numeric vector
#' and a \code{LazyColumn} object.
#' If \code{x} is a scalar, scalar multiplication is performed.
#' If \code{x} is a vector of the same length as the column, the dot product
#' is performed.
#'
#' @param x A numeric scalar or vector.
#' @param y A \code{LazyColumn} object.
#'
#' @return A numeric value with the resulting scalar.
#'
#' @examples
#' mat_a <- base::matrix(rnorm(12), nrow=3, ncol=4)
#' X <- LazyMatrix(mat_a, "sd", "mean")
#' b <- rnorm(nrow(X))
#' lazy_col <- X[, 1]
#' b %*% lazy_col
setMethod(
  "%*%",
  signature(x = "numeric", y = "LazyColumn"),
  function(x, y) {
    s <- 1 / y@scale
    c <- y@location
    if (length(x) == 1) {
      x * y
    } else {
      first_term <- s * x %*% y@data
      second_term <- sum(x) * s * c
      first_term - second_term
    }
  }
)

#' Perform the dot product between two LazyColumn vectors
#'
#' Computes the inner product between two \code{LazyColumn} objects.
#'
#' @param x A \code{LazyColumn} object.
#' @param y A \code{LazyColumn} object.
#'
#' @return A numeric value with the resulting scalar.
#'
#' @examples
#' mat_a <- base::matrix(rnorm(12), nrow=3, ncol=4)
#' X <- LazyMatrix(mat_a, "sd", "mean")
#' b <- rnorm(nrow(X))
#' lazy_col <- X[, 1]
#' lazy_col_2 <- X[, 2]
#' lazy_col %*% lazy_col_2
setMethod(
  "%*%",
  signature(x = "LazyColumn", y = "LazyColumn"),
  function(x, y) {
    s_1 <- 1 / x@scale
    c_1 <- x@location
    s_2 <- 1 / y@scale
    c_2 <- y@location
    n <- length(x@data)
    one_vector <- rep(1, length(x@data))
    first_term <- x@data %*% y@data
    second_term <- x@data %*% one_vector * c_2
    third_term <- y@data %*% one_vector * c_1
    fourth_term <- n * c_1 * c_2
    s_1 * s_2 * (first_term - second_term - third_term + fourth_term)
  }
)

#--------------------------------------------------
## Euclidean Norm ####
#--------------------------------------------------
#' Perform the norm of a LazyColumn vector
#'
#' Computes the norm of a \code{LazyColumn} object.
#'
#' @param x A \code{LazyColumn} object.
#' @param type A character value defining type of norm. Default is Euclidean norm.
#'
#' @return A numeric value with the resulting scalar.
#'
#' @examples
#' mat_a <- base::matrix(rnorm(12), nrow=3, ncol=4)
#' X <- LazyMatrix(mat_a, "sd", "mean")
#' b <- rnorm(nrow(X))
#' lazy_col <- X[, 1]
#' norm(lazy_col)
setMethod("norm", "LazyColumn", function(x, type = "2") {
  if (type != "2") {
    stop("Only Euclidean norm (type='2') is supported for LazyColumn.")
  }
  s <- 1 / x@scale
  c <- x@location
  n <- length(x)
  one_vector <- rep(1, n)
  first_term <- base::sum(x@data^2)
  second_term <- 2 * c * sum(x@data)
  third_term <- n * c^2
  base::sqrt(s^2 * (first_term - second_term + third_term))
})
