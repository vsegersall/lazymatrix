#--------------------------------------------------
# LazyMatrix --> LazyVector ####
#--------------------------------------------------
setMethod("[", "LazyMatrix", function(x, i, j, ..., drop = TRUE) {
  # Column subsetting: X[, j]
  if (missing(i) && !missing(j)) {
    j <- as.integer(j)
    if (length(j) == 1) {
      new(
        "LazyVector",
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
