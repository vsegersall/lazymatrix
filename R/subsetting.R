#--------------------------------------------------
# LazyMatrix --> LazyColumn ####
#--------------------------------------------------
#' Subset a LazyMatrix by columns
#'
#' Subsets a \code{LazyMatrix} object by columns, returning either a
#' \code{LazyColumn} or a new \code{LazyMatrix} depending on the number
#' of columns selected. Row subsetting is not yet supported.
#'
#' @param x A \code{LazyMatrix} object.
#' @param i Row index. Must be missing as row subsetting is not yet supported.
#' @param j Column index. Either a single integer returning a \code{LazyColumn},
#'   or a vector of integers returning a \code{LazyMatrix}.
#' @param ... Additional arguments (ignored).
#' @param drop Logical. Currently ignored.
#'
#' @returns A \code{LazyColumn} if a single column is selected, or a
#'   \code{LazyMatrix} if multiple columns are selected.
#'
#' @examples
#' A <- Matrix::sparseMatrix(i = c(1,2,3), j = c(1,2,3), x = c(1,2,3))
#' lazy_m <- LazyMatrix(A, "sd", "mean")
#'
#' # Single column → LazyColumn
#' lazy_col <- lazy_m[, 2]
#'
#' # Multiple columns → LazyMatrix
#' lazy_subset <- lazy_m[, 1:3]
#'
#' @rdname subset-LazyMatrix
#' @aliases [,LazyMatrix,ANY,ANY,ANY-method
#' @export
setMethod("[", "LazyMatrix", function(x, i, j, ..., drop = TRUE) {
  # Column subsetting: X[, j]
  if (missing(i) && !missing(j)) {
    j <- as.integer(j)
    if (length(j) == 1) {
      new(
        "LazyColumn",
        data = x@data[, j],
        scale = x@col_scales[j],
        location = x@col_locations[j]
      )
    } else {
      new(
        "LazyMatrix",
        data = x@data[, j],
        col_scales = x@col_scales[j],
        row_scales = x@row_scales[j],
        col_locations = x@col_locations[j],
        row_locations = x@row_locations[j]
      )
    }
  } else {
    # Row subsetting: X[i, ]
    stop(
      "This feature is under development. Only column subsetting is supported."
    )
  }
})
