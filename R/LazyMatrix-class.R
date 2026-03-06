# Class definition ####
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
## define location- and scale parameters
## Compute and store them
LazyMatrix <- function(data, scale = NULL,
                       location = NULL){
  # code for constructing helper
  col_scales <- if(is.null(scale)){
    numeric(0)
  } else if (scale == "sd"){
    base::apply(data, 2, sd)
  }else {
    numeric(0)
  }
  row_scales <- if(is.null(scale)){
    numeric(0)
  } else if (scale == "sd"){
    base::apply(data, 1, sd)
  }else {
    numeric(0)
  }
  col_locations <- if(is.null(location)){
    numeric(0)
  } else if (location == "mean"){
    Matrix::colMeans(data)
  }else {
    numeric(0)
  }
  row_locations <- if(is.null(location)){
    numeric(0)
  } else if (location == "mean"){
    Matrix::rowMeans(data)
  }else {
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
setMethod("nrow", "LazyMatrix", function(x){
  base::nrow(x@data)
})

# ncol ####
setGeneric("ncol")
setMethod("ncol", "LazyMatrix", function(x){
  base::ncol(x@data)
})

# dim ####
setGeneric("dim")
setMethod("dim", "LazyMatrix", function(x){
  base::dim(x@data)
})

# as.matrix ####
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

# colnames ####
setGeneric("colnames")
setMethod("colnames", "LazyMatrix", function(x){
  base::colnames(x@data)
})

# matrix multiplication ####
## LazyMatrix & vector
setMethod("%*%", c("LazyMatrix", "ANY"), function(x, y){
  # X_tilde b = X S^-1 b - C S^-1 b
  s <- 1/x@col_scales
  c <- x@col_locations
  x@data %*% (s * y) - sum(c * s * y)
})

## Vector & LazyMatrix
setMethod("%*%", c("ANY", "LazyMatrix"), function(x, y){
  t(crossprod(y, x))
})

# transpose ####
setMethod("t", "LazyMatrix", function(x){
  x_transpose <- t(x@data)
  new("LazyMatrix", data = x_transpose,
      col_scales = x@row_scales, row_scales = x@col_scales,
      col_locations = x@row_locations, row_locations = x@col_locations)
})

# crossprod ####
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
