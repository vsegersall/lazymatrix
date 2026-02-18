# Class definition
setClass("LazyMatrix",
  slots = c(
    data = "ANY",
    transformations = "list" # allows for chaining transformations and operations
  ),
  prototype = list(
    # Empty matrix of zeroes until filled with real data
    ## empty list of transformations
    data = matrix(0),
    transformations = list()
  )
)

# Some helper function allowing for
# users to only call LazyMatrix(X) without "operations"
LazyMatrix <- function(data, transform = NULL){
  # code for constructing helper
  transformations <- if(is.null(transform)){
    list()
  } else if (is.character(transform)){
    lapply(transform, function(t) list(type = t)) # if user only gives one or a vector of transforms(expected user behavior)
  } else {
    transform
  }
  new("LazyMatrix", data=data, transformations=transformations)
}

# Validity checks on the arguments
setValidity("LazyMatrix", function(object){
  # 1. check that data is a matrix of some sort - should be able to handle:
  ## multiple matrix objects such as data.frame, Matrix etc
  # 2. Check that transformations is a list
})

# Methods to the class
## Addition
### Generic
setGeneric("addition", function(x, y) standardGeneric("addition"))

### two lazy matrices
setMethod("addition", c("LazyMatrix", "LazyMatrix"), function(x, y){
  new("LazyMatrix",
      data = x@data,
      transformations = c(
        x@transformations,
        list(list(
          operation = "add",
          addent = y
        ))
      ))
})

#setMethod("addition", c("LazyMatrix", "LazyMatrix"), function(x, y){
#  newmat <- matrix(NA, ncol=ncol(x@data),
#                   nrow=nrow(x@data))
#  for (i in 1:nrow(x@data)){
#    for (j in 1:ncol(x@data)){
#      newmat[i, j] <- x@data[i, j] + y@data[i, j]
#    }
#  }
#  newLazy <- LazyMatrix(newmat)
#  })
