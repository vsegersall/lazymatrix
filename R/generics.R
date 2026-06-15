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
## norm() ####
#--------------------------------------------------
#' Compute the norm of a LazyMatrix or LazyColumn
#'
#' Dispatches to the appropriate method based on the class of \code{x}.
#'
#' @param x A \code{LazyMatrix} or \code{LazyColumn} object.
#' @param ... Additional arguments passed to methods, such as \code{type}.
#'
#' @rdname norm
#' @export
setGeneric("norm", function(x, ...) standardGeneric("norm"))

#--------------------------------------------------
## length() ####
#--------------------------------------------------
#' Get the length of a LazyColumn
#'
#' @param x A LazyColumn object.
#'
#' @returns An integer value containing the number of elements within the vector.
#'
#' @examples
#' mat_a <- base::matrix(rep(1, 6), nrow=2, ncol=3)
#' lazy_a <- LazyMatrix(mat_a, "sd", "mean")
#' lazy_col <- lazy_a[, 2]
#' length(lazy_col)
#'
#' @rdname length-LazyColumn
#' @export
setMethod("length", "LazyColumn", function(x) {
  base::length(x@data)
})
