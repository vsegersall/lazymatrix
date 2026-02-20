# Class definition
setClass("LazyMatrix",
  slots = c(
    data = "ANY",
    scales = "ANY",
    locations = "ANY"
  ),
  prototype = list(
    data = matrix(0),
    scales = c(0),
    locations = c(0)
  )
)

# Helper function should:
## define location- and scale parameters
## Compute and store them
LazyMatrix <- function(data, scale = NULL,
                       location = NULL){
  # code for constructing helper
  scales <- if(is.null(scale)){
    c()
  } else if (scale == "sd"){
    scales <- apply(data, 2, sd)
  }else {
    c()
  }
  locations <- if(is.null(location)){
    c()
  } else if (location == "mean"){
    locations <- Matrix::colMeans(data)
  }else {
    c()
  }
  new("LazyMatrix", data=data, scales=scales,
      locations=locations)
}

# Validity checks on the arguments
setValidity("LazyMatrix", function(object){
  # 1. check that data is a matrix: this is crucial for how the methods are
  ## implemented
  # 2. Check for location and scale
})

# Multiplication between lazy object and non-lazy vector
setMethod("%*%", c("LazyMatrix", "ANY"), function(x, y){
  s <- 1/x@scales
  c <- x@locations
  x@data %*% s * y - c * s * y
})

# Transpose
setMethod("t", "LazyMatrix", function(x){
  s <- 1/x@scales
  c <- matrix(0, nrow=nrow(x@data),
              ncol=ncol(x@data))
  for (i in 1:nrow(c)){
    c[i,] <- x@locations
  }
  s %*% t(x@data) - s %*% t(c)
})

# crossprod

