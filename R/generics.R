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

#--------------------------------------------------
# LazyColumn ####
#--------------------------------------------------
#--------------------------------------------------
## Logial Operators ####
#--------------------------------------------------
#' Comparison operators for LazyColumn
#'
#' Enables logical comparisons (`>`, `<`, `>=`, `<=`, `==`, `!=`) on a
#' \code{LazyColumn}, comparing against its scaled values.
#'
#' @param e1 A \code{LazyColumn} or numeric.
#' @param e2 A \code{LazyColumn} or numeric.
#' @returns A logical vector.
#' @export
setMethod("Compare", signature("LazyColumn", "numeric"), function(e1, e2) {
  callGeneric(e1@data, e2) # placeholder, fill in `scaled` properly
})

#' Comparison operators for LazyColumn
#'
#' Enables logical comparisons (`>`, `<`, `>=`, `<=`, `==`, `!=`) on a
#' \code{LazyColumn}, comparing against its scaled values.
#'
#' @param e1 A \code{LazyColumn} or numeric.
#' @param e2 A \code{LazyColumn} or numeric.
#' @returns A logical vector.
#' @export
setMethod("Compare", signature("numeric", "LazyColumn"), function(e1, e2) {
  callGeneric(e1, e2@data) # placeholder, fill in `scaled` properly
})

#--------------------------------------------------
## setNames ####
#--------------------------------------------------
#' Set names for a LazyColumn
#'
#' Assigns names to the underlying data of a \code{LazyColumn}, returning a
#' new \code{LazyColumn} with the same scale and location as the original.
#' This enables name-based subsetting via \code{[} on \code{LazyColumn}
#' objects, mirroring \code{stats::setNames()} for ordinary vectors.
#'
#' @param object A \code{LazyColumn} object.
#' @param nm A character vector of names, with length equal to
#'   \code{length(object@data)}.
#'
#' @returns A new \code{LazyColumn} with named data, preserving the original
#'   scale and location.
#'
#' @examples
#' set.seed(123)
#' mat_a <- matrix(rnorm(500), nrow = 50, ncol = 10)
#' lazy_a <- LazyMatrix(mat_a, "sd", "mean")
#' lazy_c <- lazy_a[, 2]
#' lazy_named <- setNames(lazy_c[1:26], letters[1:26])
#' lazy_named["a"]
#'
#' @rdname setNames-LazyColumn
#' @aliases setNames,LazyColumn,character-method
#' @export
setMethod(
  "setNames",
  signature("LazyColumn", "character"),
  function(object, nm) {
    named_data <- stats::setNames(object@data, nm)
    new(
      "LazyColumn",
      data = named_data,
      scale = object@scale,
      location = object@location
    )
  }
)
