test_that("Subsetting works as base R.", {
  # 1. Define test matrix
  set.seed(123)
  mat_a <- matrix(rnorm(500), nrow = 50, ncol = 10)

  # 2. Define Regular LazyMatrix
  lazy_a <- LazyMatrix(mat_a, "sd", "mean")

  # 3. Single column case
  lazy_column <- lazy_a[, 2]
  dense_column <- mat_a[, 2]
  dense_mean <- base::mean(mat_a[, 2])
  dense_sd <- stats::sd(mat_a[, 2])
  expect_equal(lazy_column@data, dense_column)
  expect_equal(lazy_column@location, dense_mean)
  expect_equal(lazy_column@scale, dense_sd)

  # 4. Multiple Column Case
  lazy_subset <- lazy_a[, 5:10]
  dense_subset <- mat_a[, 5:10]
  dense_means <- base::apply(dense_subset, 2, base::mean)
  dense_sds <- base::apply(dense_subset, 2, stats::sd)
  expect_equal(lazy_subset@data, dense_subset)
  expect_equal(lazy_subset@col_locations, dense_means)
  expect_equal(lazy_subset@col_scales, dense_sds)
})
