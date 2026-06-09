#--------------------------------------------------
# LazyMatrix: Class Definition ####
#--------------------------------------------------
test_that("Class definition works", {
  mat_a <- base::matrix(1:4, 2, 2)
  a <- LazyMatrix(mat_a)
  b <- LazyMatrix(mat_a, scale = "sd")
  c <- LazyMatrix(mat_a, scale = "sd", location = "mean")

  # 1. Check that each type is a LazyMatrix object
  expect_s4_class(a, "LazyMatrix")
  expect_s4_class(b, "LazyMatrix")
  expect_s4_class(c, "LazyMatrix")

  # 2. Check that we stored column locations properly
  expected.col_location <- Matrix::colMeans(mat_a)
  observed.col_location <- c@col_locations
  expect_equal(expected.col_location, observed.col_location)

  # 3. Check that we stored row locations properly
  expected.row_location <- Matrix::rowMeans(mat_a)
  observed.row_location <- c@row_locations
  expect_equal(expected.row_location, observed.row_location)

  # 4. Check that we stored column scales properly
  expected.col_scale <- base::apply(mat_a, 2, sd)
  observed.col_scale.1arg <- b@col_scales
  expect_equal(expected.col_scale, observed.col_scale.1arg)
  observed.col_scale.2args <- c@col_scales
  expect_equal(expected.col_scale, observed.col_scale.2args)

  # 5. Check that we stored row scales properly
  expected.row_scale <- base::apply(mat_a, 1, sd)
  observed.row_scale.1arg <- b@row_scales
  expect_equal(expected.row_scale, observed.row_scale.1arg)
  observed.row_scale.2args <- c@row_scales
  expect_equal(expected.row_scale, observed.row_scale.2args)

  # 6. Check that empty LazyMatrix has numeric(0) for all params
  expect_length(a@col_scales, 0)
  expect_length(a@row_scales, 0)
  expect_length(a@col_locations, 0)
  expect_length(a@row_locations, 0)
})

#--------------------------------------------------
# LazyMatrix: Type Tests ####
#--------------------------------------------------

#--------------------------------------------------
# Type: sparseMatrix ####
#--------------------------------------------------
test_that("Methods handle sparseMatrix", {
  # 1. Define sparseMatrix
  set.seed(123)
  n_row <- 7
  n_col <- 5
  i <- c(1, 2, 3, 4, 5, 6, 7, 1, 3, 5)
  j <- c(1, 2, 3, 4, 5, 2, 3, 4, 5, 1)
  x <- rnorm(length(i))
  mat_a <- Matrix::sparseMatrix(i = i, j = j, x = x, dims = c(n_row, n_col))
  scaled_a <- base::scale(mat_a)

  # 2. Define LazyMatrix
  lazy_a <- LazyMatrix(mat_a, "sd", "mean")

  # 3. Define test objects
  b <- rnorm(ncol(mat_a))
  c <- rnorm(nrow(mat_a))

  # 4. Matrix multiplication
  prod_norm <- scaled_a %*% b
  prod_lazy <- lazy_a %*% b
  expect_equal(as.matrix(prod_lazy), prod_norm)

  # 5. Crossprod
  cp_norm <- base::crossprod(scaled_a, c)
  cp_lazy <- crossprod(lazy_a, c)
  expect_equal(as.matrix(cp_lazy), cp_norm)
})
