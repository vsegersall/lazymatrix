test_that("Subsetting works as base R.", {
  # 1. Define test matrix
  set.seed(123)
  mat_a <- matrix(rnorm(500), nrow = 50, ncol = 10)

  # 2. Define Regular LazyMatrix
  lazy_a <- LazyMatrix(mat_a, "sd", "mean")

  # 3. Single column case, all rows
  lazy_column <- lazy_a[, 2]
  dense_column <- mat_a[, 2]
  dense_mean <- base::mean(mat_a[, 2])
  dense_sd <- stats::sd(mat_a[, 2])
  expect_equal(lazy_column@data, dense_column)
  expect_equal(lazy_column@location, dense_mean)
  expect_equal(lazy_column@scale, dense_sd)

  # 4. Multiple Column Case, all rows
  lazy_subset <- lazy_a[, 5:10]
  dense_subset <- mat_a[, 5:10]
  dense_means <- base::apply(dense_subset, 2, base::mean)
  dense_sds <- base::apply(dense_subset, 2, stats::sd)
  expect_equal(lazy_subset@data, dense_subset)
  expect_equal(lazy_subset@col_locations, dense_means)
  expect_equal(lazy_subset@col_scales, dense_sds)

  # 5. One column, multiple rows
  lazy_col_2 <- lazy_a[2:4, 5]
  dense_col_2 <- scale(mat_a[2:4, 5])
  dense_mean_2 <- attr(dense_col_2, "scaled:center")
  dense_sd_2 <- attr(dense_col_2, "scaled:scale")
  expect_equal(lazy_col_2@data, mat_a[2:4, 5])
  expect_equal(lazy_col_2@location, dense_mean_2)
  expect_equal(lazy_col_2@scale, dense_sd_2)

  # 6. Multiple columns, multiple rows
  lazy_subset_2 <- lazy_a[2:7, 5:10]
  dense_subset_2 <- mat_a[2:7, 5:10]
  scaled_subset <- scale(dense_subset_2)
  expect_equal(lazy_subset_2@data, mat_a[2:7, 5:10])
  expect_equal(lazy_subset_2@col_scales, attr(scaled_subset, "scaled:scale"))
  expect_equal(
    lazy_subset_2@col_locations,
    attr(scaled_subset, "scaled:center")
  )

  # 7. Single element case
  lazy_element <- lazy_a[5, 2]
  dense_element <- base::scale(mat_a)[5, 2]
  expect_equal(lazy_element, dense_element)
})
