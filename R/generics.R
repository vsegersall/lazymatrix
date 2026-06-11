#--------------------------------------------------
# LazyMatrix ####
#--------------------------------------------------

#--------------------------------------------------
## nrow() ####
#--------------------------------------------------
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

#--------------------------------------------------
## ncol() ####
#--------------------------------------------------
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

#--------------------------------------------------
## dim() ####
#--------------------------------------------------
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

#--------------------------------------------------
## colnames() ####
#--------------------------------------------------
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

#--------------------------------------------------
# LazyColumn ####
#--------------------------------------------------

#--------------------------------------------------
## length() ####
#--------------------------------------------------
setMethod("length", "LazyColumn", function(x) {
  base::length(x@data)
})
