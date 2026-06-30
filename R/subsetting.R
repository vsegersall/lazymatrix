#--------------------------------------------------
# LazyMatrix ####
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
#' @name subset-LazyMatrix
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
  } else if (!missing(i) && missing(j)) {
    # Row subsetting: X[i, ]
    stop(
      "This feature is under development. Only column subsetting is supported."
    )
  } else {
    if (length(i) == 1 && length(j) == 1) {
      x@data[i, j] / x@col_scales[j] - x@col_locations[j] / x@col_scales[j]
    } else if (length(i) > 1 && length(j) == 1) {
      new(
        "LazyColumn",
        data = x@data[i, j],
        scale = stats::sd(x@data[i, j]),
        location = base::mean(x@data[i, j])
      )
    } else if (length(i) == 1 && length(j) > 1) {
      stop(
        "This feature is under development. Only column subsetting is supported."
      )
    } else {
      new(
        "LazyMatrix",
        data = x@data[i, j],
        col_scales = base::apply(x@data[i, j], 2, stats::sd),
        row_scales = base::apply(x@data[i, j], 1, stats::sd),
        col_locations = Matrix::colMeans(x@data[i, j]),
        row_locations = Matrix::rowMeans(x@data[i, j])
      )
    }
  }
})

#--------------------------------------------------
# LazyColumn ####
#--------------------------------------------------
#' Subset a LazyColumn
#'
#' Subsets a \code{LazyColumn} object using standard R indexing rules:
#' positive integers, negative integers, logical vectors (with recycling),
#' character vectors (if named), zero, or missing (nothing).
#'
#' @param x A \code{LazyColumn} object.
#' @param i Index: positive integers, negative integers, logical vector,
#'   character vector (if named), zero, or missing (nothing).
#' @param j Not used for \code{LazyColumn} (1-dimensional). Must be missing.
#' @param ... Additional arguments (ignored).
#' @param drop Logical. Currently ignored — single element extraction always
#'   returns a plain scaled numeric, consistent with vector subsetting.
#'
#' @returns A \code{LazyColumn} when multiple elements are selected, or a
#'   scaled numeric value when a single element is selected.
#'
#' @examples
#' set.seed(123)
#' mat_a <- matrix(rnorm(500), nrow = 50, ncol = 10)
#' lazy_a <- LazyMatrix(mat_a, "sd", "mean")
#' lazy_c <- lazy_a[, 2]
#'
#' # Single element → plain scaled numeric
#' lazy_c[2]
#'
#' # Multiple elements → LazyColumn
#' lazy_c[c(1, 3, 5)]
#'
#' @name subset-LazyColumn
#' @rdname subset-LazyColumn
#' @aliases [,LazyColumn,ANY,ANY,ANY-method
#' @export
setMethod("[", "LazyColumn", function(x, i, ..., drop = TRUE) {
  # ---- Case: nothing (X[]) ----
  if (missing(i)) {
    return(x)
  }

  # ---- Case: zero-length result (X[0] or X[integer(0)]) ----
  if (length(i) == 0 || (length(i) == 1 && i == 0)) {
    return(new("LazyColumn", data = numeric(0), scale = 0, location = 0))
  }

  # ---- Case: single element ----
  if (length(i) == 1 && i > 0) {
    x_ij <- x@data[i] / x@scale - x@location / x@scale
    return(x_ij)
  }

  new(
    "LazyColumn",
    data = x@data[i],
    scale = stats::sd(x@data[i]),
    location = base::mean(x@data[i])
  )
})
