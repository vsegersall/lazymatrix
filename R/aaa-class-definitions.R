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
#' @returns A LazyMatrix object.
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
setClass(
  "LazyColumn",
  contains = "LazyBase",
  slots = list(
    data = "numeric",
    scale = "numeric",
    location = "numeric"
  )
)
