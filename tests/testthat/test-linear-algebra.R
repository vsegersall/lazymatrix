#--------------------------------------------------
# Matrix Multiplication ####
#--------------------------------------------------
test_that("Lazy multiplication computation works", {
  # 1. Define non-lazy object
  mat_a <- base::matrix(c(1, 2, 3, 1, 2, 3), nrow = 2, ncol = 3)
  scaled_a <- scale(mat_a)

  # 2. Define lazy object
  lazy_a <- LazyMatrix(mat_a, "sd", "mean")

  # 3. Define test objects
  b <- c(1, 2, 3)
  c <- c(1, 2)
  set.seed(123)
  m <- base::matrix(rnorm(6), nrow = 3, ncol = 2)

  # 4. Test: matrix %*% vector
  expected.product <- scaled_a %*% b
  observed.product <- lazy_a %*% b
  expect_equal(expected.product, observed.product)

  # 5. Test: matrix %*% matrix
  normal_m <- scaled_a %*% m
  lazy_m <- lazy_a %*% m
  expect_equal(normal_m, lazy_m)

  # 6. Test: vector %*% matrix
  #normal_v <- crossprod(c, scaled_a)
  normal_v <- c %*% scaled_a
  lazy_v <- c %*% lazy_a
  expect_equal(normal_v, lazy_v)
})

#--------------------------------------------------
# Matrix Transpose ####
#--------------------------------------------------
test_that("Lazy tranpose works", {
  mat.a <- base::matrix(c(1, 2, 3, 1, 2, 3), nrow = 2, ncol = 3)
  mat.at <- base::t(mat.a)
  lazy.a <- LazyMatrix(mat.a, "sd", "mean")
  lazy.at <- t(lazy.a)
  expect_s4_class(lazy.at, "LazyMatrix")
  expect_equal(mat.at, lazy.at@data)
})

#--------------------------------------------------
# Transposed Matrix-vector Multiplication ####
#--------------------------------------------------
test_that("Crossprod works", {
  # 1. Define sparseMatrix
  base::set.seed(123)
  n_row <- 50
  n_col <- 10
  i <- c(
    1:50,
    base::sample(1:50, 20, replace = TRUE),
    base::sample(1:50, 15, replace = TRUE)
  )
  j <- c(
    base::rep_len(1:10, 50),
    base::sample(1:10, 20, replace = TRUE),
    base::sample(1:10, 15, replace = TRUE)
  )
  pairs <- base::unique(data.frame(i = i, j = j))
  i <- pairs$i
  j <- pairs$j
  x <- stats::rnorm(length(i))
  A <- Matrix::sparseMatrix(i = i, j = j, x = x, dims = c(n_row, n_col))

  # 2. Define dense test matrix
  B <- matrix(rnorm(500), nrow = 50, ncol = 10)

  # 3. Scaled objects
  scaled_a <- base::scale(A)
  scaled_b <- base::scale(B)

  # 4. Test vector
  test_a <- rnorm(nrow(A))
  test_b <- rnorm(nrow(B))

  # 5. Define lazy matrices
  lazy_a <- LazyMatrix(A, "sd", "mean")
  lazy_b <- LazyMatrix(B, "sd", "mean")

  # 6. sparseMatrix: X^T %*% b
  sparse_crossprod <- crossprod(lazy_a, test_a)
  expected_a <- base::crossprod(scaled_a, test_a)
  expect_equal(as.matrix(sparse_crossprod), as.matrix(expected_a)) # ← Add as.matrix()

  # 7. dense matrix: X^T %*% b
  dense_crossprod <- crossprod(lazy_b, test_b)
  expected_b <- base::crossprod(scaled_b, test_b)
  expect_equal(as.matrix(dense_crossprod), as.matrix(expected_b)) # ← Add as.matrix()

  # 5. Test Gram matrix values, not object
  scaled_gram <- crossprod(scaled_a)
  lazy_gram <- crossprod(lazy_a)
  expect_equal(as.matrix(lazy_gram), as.matrix(scaled_gram))
})

#--------------------------------------------------
# Partial SVD with irlba() ####
#--------------------------------------------------
test_that("SVD works", {
  # 1. Define non lazy matrix
  set.seed(123)
  mat_a <- matrix(rnorm(500), nrow = 50, ncol = 10)
  scaled_a <- scale(mat_a)

  # 2. Define LazyMatrix
  lazy_a <- LazyMatrix(mat_a, scale = "sd", location = "mean")
  n <- nrow(lazy_a)
  p <- ncol(lazy_a)

  # 3. Set a smaller number of sing values
  k <- 3

  # 3. Perform SVD
  svd_norm <- base::svd(scaled_a, nu = k, nv = k)
  svd_lazy <- lazymatrix::svd(lazy_a, nu = k, nv = k)

  # 4. Test
  expect_equal(svd_norm$d[1:3], svd_lazy$d[1:3])
  expect_equal(dim(svd_lazy$u[1:9]), dim(svd_norm$u[1:9]))
})

#--------------------------------------------------
# Frobienius Norm ####
#--------------------------------------------------
test_that("Frobenius norm works", {
  # 1. Set test matrix
  set.seed(123)
  sparse_matrix <- Matrix::Matrix(0, 5, 3)
  sparse_matrix[sample(length(sparse_matrix), 5)] <- rnorm(5)

  # 2. Set lazy object
  lazy_s <- LazyMatrix(sparse_matrix, "sd", "mean")

  # 3. Compute norm
  frob_norm <- base::norm(scale(sparse_matrix), "F")
  lazy_frob <- lazymatrix::norm(lazy_s)

  # 4. Test
  expect_equal(lazy_frob, frob_norm)
})
