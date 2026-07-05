#--------------------------------------------------
# LazyBase: Virtual Class for all Lazy Objects ####
#--------------------------------------------------
setClass("LazyBase", contains = "VIRTUAL")

#--------------------------------------------------
# LazyMatrix: Class Definition ####
#--------------------------------------------------
#' @importFrom methods new

#' @name LazyMatrix-class
#' @title LazyMatrix S4 class
#'
#' @description An S4 class to represent a lazily transformed matrix with scaling and location parameters.
#'
#' @slot data The underlying matrix.
#' @slot col_scales Numeric vector of column scales.
#' @slot row_scales Numeric vector of row scales.
#' @slot col_locations Numeric vector of column locations.
#' @slot row_locations Numeric vector of row locations.
#' @export
#' @return An object of class \code{LazyMatrix} with slots \code{data} (matrix, possibly sparse), \code{col_scales}, \code{row_scales}, \code{col_locations}, \code{row_locations}.
#' Represents the original data matrix plus stored scaling/centering parameters used for lazy operations
#' @examples
#' mat <- matrix(1:6, nrow=2, ncol=3)
#' obj <- LazyMatrix(mat, "sd", "mean")
setClass(
  "LazyMatrix",
  contains = "LazyBase",
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

#--------------------------------------------------
## LazyMatrix: Helper ####
#--------------------------------------------------
#' Constructs a LazyMatrix object.
#'
#' @param data a matrix object.
#' @param scale optional scaling parameter.
#' @param location optional location parameter.
#'
#' @return A LazyMatrix object.
#' @export
#'
#' @examples
#' mat_a <- matrix(1:6, nrow=3, ncol=2)
#' lazy_a <- LazyMatrix(mat_a, scale="sd", location="mean")
#' lazy_a
LazyMatrix <- function(data, scale = NULL, location = NULL) {
  # code for constructing helper
  col_scales <- if (is.null(scale)) {
    numeric(0)
  } else if (scale == "sd") {
    base::apply(data, 2, stats::sd)
  } else {
    numeric(0)
  }
  row_scales <- if (is.null(scale)) {
    numeric(0)
  } else if (scale == "sd") {
    base::apply(data, 1, stats::sd)
  } else {
    numeric(0)
  }
  col_locations <- if (is.null(location)) {
    numeric(0)
  } else if (location == "mean") {
    Matrix::colMeans(data)
  } else {
    numeric(0)
  }
  row_locations <- if (is.null(location)) {
    numeric(0)
  } else if (location == "mean") {
    Matrix::rowMeans(data)
  } else {
    numeric(0)
  }
  new(
    "LazyMatrix",
    data = data,
    col_scales = col_scales,
    row_scales = row_scales,
    col_locations = col_locations,
    row_locations = row_locations
  )
}

#--------------------------------------------------
## LazyMatrix: Validity Check ####
#--------------------------------------------------
setValidity("LazyMatrix", function(object) {
  # 1. check that data is a matrix: this is crucial for how the methods are
  ## implemented
  # 2. Check for location and scale
})

#--------------------------------------------------
# LazyColumn: Class Definition ####
#--------------------------------------------------
#' @importFrom methods new

#' @name LazyColumn-class
#' @title LazyColumn S4 class
#'
#' @description An S4 class to represent a column vector as a subset of a LazyMatrix-object
#'
#' @slot data The underlying data column vector.
#' @slot scale Numeric scalar containing column-scale parameter.
#' @slot location Numeric scalar containing the column-location parameter.
#' @export
#' @return An object of class \code{LazyColumn} with slots \code{data} (numeric vector), \code{scale} (numeric scalar), and \code{location} (numeric scalar); represents a column of a \code{LazyMatrix} (scaled via scale and location).
#' @examples
#' mat <- matrix(1:6, nrow = 2, ncol = 3)
#' lazy_mat <- LazyMatrix(mat, "sd", "mean")
#' lazy_column <- lazy_mat[, 2]
setClass(
  "LazyColumn",
  contains = "LazyBase",
  slots = list(
    data = "numeric",
    scale = "numeric",
    location = "numeric"
  )
)
