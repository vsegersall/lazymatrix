# Class definition
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

# Helper function should:
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

# Multiplication between lazy object and non-lazy vector
setMethod("%*%", c("LazyMatrix", "ANY"), function(x, y){
  s <- 1/x@col_scales
  c <- x@col_locations
  x@data %*% s * y - c * s * y
})

# Transpose
setMethod("t", "LazyMatrix", function(x){
  x_transpose <- t(x@data)
  new("LazyMatrix", data = x_transpose,
      col_scales = x@row_scales, row_scales = x@col_scales,
      col_locations = x@row_locations, row_locations = x@col_locations)
})

# crossprod
setMethod("crossprod", c("LazyMatrix", "ANY"), function(x, y = NULL){
  if (is.null(y)){
    # gram matrix
  }
  else{
    # t(X) %*% y
    s <- 1/x@col_scales
    S_inv <- Matrix::Diagonal(length(x@col_scales), s)
    c <- x@col_locations
    x_tb <- numeric(ncol(x@data))
    for (j in 1:ncol(x@data)){
      x_tb[j] <- s[j]*sum((x@data[,j]-c[j]) * y)
    }
    x_tb
  }
})
