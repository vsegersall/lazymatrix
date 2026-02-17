# Class definition
setClass("LazyMatrix",
  slots = c(
    data = "ANY",
    operations = "list"
  ),
  prototype = list(
    # Empty matrix of zeroes until filled with real data
    ## empty list of operations
    data = matrix(0),
    operations = list()
  )
)

# Some helper function allowing for
# users to only call LazyMatrix(X) without "operations"
LazyMatrix <- function(data, operations=list()){
  # code for constructing helper
  # new("LazyMatrix", data=data, operations=operations)
}

# Validity checks on the arguments
setValidity("LazyMatrix", function(object){
  # 1. check that data is a matrix of some sort - should be able to handle:
  ## multiple matrix objects such as data.frame, Matrix etc
  # 2. Check that operations is a list
})

# Methods to the class
