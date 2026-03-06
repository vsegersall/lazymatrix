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
setClass("LazyMatrix",
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
LazyMatrix <- function(data, scale = NULL,
                       location = NULL){
  # code for constructing helper
  col_scales <- if(is.null(scale)){
    numeric(0)
  } else if (scale == "sd"){
    base::apply(data, 2, stats::sd)
  }else {
    numeric(0)
  }
  row_scales <- if(is.null(scale)){
    numeric(0)
  } else if (scale == "sd"){
    base::apply(data, 1, stats::sd)
  }else {
    numeric(0)
  }
  col_locations <- if(is.null(location)){
    numeric(0)
  } else if (location == "mean"){
    Matrix::colMeans(data)
  } else {
    numeric(0)
  }
  row_locations <- if(is.null(location)){
    numeric(0)
  } else if (location == "mean"){
    Matrix::rowMeans(data)
  } else {
    numeric(0)
  }
  new("LazyMatrix", data = data,
      col_scales = col_scales, row_scales = row_scales,
      col_locations = col_locations, row_locations = row_locations)
}

# Validity checks on the arguments
setValidity("LazyMatrix", function(object){
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
setMethod("nrow", "LazyMatrix", function(x){
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
setMethod("ncol", "LazyMatrix", function(x){
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
setMethod("dim", "LazyMatrix", function(x){
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
setMethod("colnames", "LazyMatrix", function(x){
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
setMethod("as.matrix", "LazyMatrix", function(x){
  # X_tilde = X S^-1 - C S^-1
  s <- 1/x@col_scales
  S_inv <- Matrix::Diagonal(length(x@col_scales), s)
  c <- x@col_locations
  first_term <- x@data %*% S_inv
  second_term <- Matrix::Matrix(c * s, nrow=nrow(first_term),
                                ncol=length(s), byrow=TRUE)
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
setMethod("t", "LazyMatrix", function(x){
  x_transpose <- t(x@data)
  new("LazyMatrix", data = x_transpose,
      col_scales = x@row_scales, row_scales = x@col_scales,
      col_locations = x@row_locations, row_locations = x@col_locations)
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
setMethod("%*%", c("LazyMatrix", "ANY"), function(x, y){
  # X_tilde b = X S^-1 b - C S^-1 b
  s <- 1/x@col_scales
  c <- x@col_locations
  x@data %*% (s * y) - sum(c * s * y)
})

<<<<<<< HEAD
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
=======
## vector & LazyMatrix
>>>>>>> fea32ef (svd and pca on new branch.)
setMethod("%*%", c("ANY", "LazyMatrix"), function(x, y){
  t(crossprod(y, x))
})

## LazyMatrix & matrix
setMethod("%*%", c("LazyMatrix", "matrix"), function(x, y){
  # X_tilde M = X S^-1 M - C S^-1 M
  s <- 1/x@col_scales
  c <- x@col_locations
  first_term <- x@data %*% (s * y)
  centering_row <- as.vector((c * s) %*% y)
  centering_matrix <- base::matrix(centering_row,
                             nrow = nrow(x),
                             ncol = ncol(y),
                             byrow = TRUE)
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
setMethod("crossprod", c("LazyMatrix", "ANY"), function(x, y = NULL){
  if (is.null(y)){
    # gram matrix
    # X_tilde^T X_tilde = S^-1 X^T X S^-1 - n S^-1 c c^T S^-1
    s <- 1/x@col_scales
    S_inv <- Matrix::Diagonal(length(x@col_scales), s)
    c <- x@col_locations
    n <- nrow(x@data)
    xt_x <- base::crossprod(x@data)
    first_term <- S_inv %*% xt_x %*% S_inv
    cc_t <- c %*% t(c)
    second_term <- n * S_inv %*% cc_t %*% S_inv
    first_term - second_term
  }
  else{
    # t(X) %*% y
    # X_tilde^T b = S^1 X^T b - S^1 C^T b
    s <- 1/x@col_scales
    c <- x@col_locations
    x_tb <- Matrix::Matrix(0, nrow = ncol(x@data),
                           ncol = 1, sparse = FALSE)
    sum_y <- base::sum(y)
    for (j in 1:ncol(x@data)){
      x_tb[j] <- s[j]*base::sum(x@data[,j] * y) - s[j]*c[j]*sum_y
    }
    x_tb
  }
})

# svd ####
setMethod("svd", "LazyMatrix", function(x, nu = min(n, p), nv = min(n, p)){
  if (missing(nu)) nu <- 5
  if (missing(nv)) nv <- 5

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
setMethod("prcomp", "LazyMatrix", function(x, retx = TRUE, tol = NULL,
                                           rank. = NULL, ...){
  cen <- x@col_locations
  sc <- x@col_scales
  if (any(sc == 0))
    stop("cannot rescale a constant/zero column to unit variance")
  n <- nrow(x)
  p <- ncol(x)
  k <- if (!is.null(rank.)) {
    stopifnot(length(rank.) == 1, is.finite(rank.), as.integer(rank.) >
                0)
    min(as.integer(rank.), n, p)
  }
  else min(n, p)
  k <- min(k, min(n, p) - 1)
  s <- svd(x, nu = 0, nv= k)
  j <- base::seq_len(k)
  s$d <- s$d / base::sqrt(max(1, n-1))
  if (!is.null(tol)) {
    rank <- sum(s$d > (s$d[1L] * tol))
    if (rank < k) {
      j <- seq_len(k <- rank)
      s$v <- s$v[, j, drop = FALSE]
    }
  }
  center = if(length(cen) > 0) cen else FALSE
  scale = if(length(sc) > 0) sc else FALSE
  dimnames(s$v) <- list(colnames(x), paste0("PC", j))
  r <- list(sdev = s$d, rotation = s$v, center = center,
            scale = scale)
  if (retx)
    r$x <- x %*% s$v
  base::class(r) <- "prcomp"
  r
})
