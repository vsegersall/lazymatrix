#--------------------------------------------------
# Algorithms: Helper Functions ####
#--------------------------------------------------

#--------------------------------------------------
## gradient_descent_helper() ####
#--------------------------------------------------
gradient_descent_helper <- function(
  x,
  y,
  w_init,
  b_init,
  learning_rate,
  n_epochs
) {
  w <- w_init
  b <- Matrix::Matrix(b_init, nrow = nrow(x))
  n <- nrow(x)
  for (epoch in 1:n_epochs) {
    y_pred <- x %*% w + b
    error <- y_pred - y
    w <- w - (learning_rate * crossprod(x, error)) / n
    b <- b - (learning_rate * sum(error)) / n
  }
  return(list(w = w, b = b))
}

#--------------------------------------------------
## cholesky_decomp() ####
#--------------------------------------------------
cholesky_decomp <- function(A) {
  L <- Matrix::chol(A)
  M <- t(L)
  A_new <- M %*% L
  return(base::list(A = A, "upper" = L, "lower" = M))
}

#--------------------------------------------------
# Gradient Descent ####
#--------------------------------------------------
test_that("Gradient descent algorithm works", {
  set.seed(2121)

  # 1. Set up design matrix, non-lazy
  n <- 50
  p <- 5
  mat_a <- base::matrix(stats::rnorm(n * p), nrow = n, ncol = p)
  scaled_a <- base::scale(mat_a)

  # 2. Set up lazy design matrix
  lazy_a <- LazyMatrix(mat_a, "sd", "mean")

  # 3. Parameter initialisation
  set.seed(4567)
  w_init <- stats::rnorm(p)
  b_init <- stats::rnorm(1)
  y_true <- stats::rnorm(nrow(scaled_a))
  learning_rate <- 0.01
  n_epochs <- 10

  # 4. Run gradient descent, lazy and not lazy
  pars_nonlazy <- gradient_descent_helper(
    x = scaled_a,
    y = y_true,
    w_init = w_init,
    b_init = b_init,
    learning_rate = learning_rate,
    n_epochs = n_epochs
  )
  pars_lazy <- gradient_descent_helper(
    x = lazy_a,
    y = y_true,
    w_init = w_init,
    b_init = b_init,
    learning_rate = learning_rate,
    n_epochs = n_epochs
  )
  preds_nonlazy <- scaled_a %*% pars_nonlazy$w + pars_nonlazy$b
  preds_lazy <- lazy_a %*% pars_lazy$w + pars_lazy$b

  # 5. Test
  expect_equal(as.vector(pars_nonlazy$w), as.vector(pars_lazy$w))
  expect_equal(pars_nonlazy$b, pars_lazy$b)
  expect_equal(preds_nonlazy, preds_lazy)
})

#--------------------------------------------------
# Cholesky Decomposition ####
#--------------------------------------------------
test_that("Cholesky decomposition works", {
  # 1. Define test objects
  set.seed(123)
  mat_a <- matrix(rnorm(500), nrow = 50, ncol = 10)
  scaled_a <- Matrix::Matrix(base::scale(mat_a))
  b <- rnorm(ncol(scaled_a))

  # 2. Define lazy object
  lazy_a <- LazyMatrix(mat_a, "sd", "mean")

  # 3. Construct symmetric positive definit matrices
  gram_manual <- t(scaled_a) %*% scaled_a
  gram_lazy <- crossprod(lazy_a)

  # 4. Test for positive definite
  expect_equal(gram_manual, gram_lazy)
  eigen_manual <- base::eigen(gram_lazy)$values
  expect_true(all(eigen_manual > 0))

  # 5. Cholesky decomp
  non_lazy <- cholesky_decomp(gram_manual)
  lazy <- cholesky_decomp(gram_lazy)

  # 6. Solve lower system
  ## Solve the lower triangular system Lx = b for x using forward substitution
  non_lazy_l_sol <- solve(non_lazy$lower, b)
  lazy_l_sol <- solve(lazy$lower, b)
  expect_equal(non_lazy_l_sol, lazy_l_sol)

  # 7. Solve upper system
  ## Solve the upper triangular system t(L)x = y for x using backward substitution
  non_lazy_u_sol <- solve(non_lazy$upper, b)
  lazy_u_sol <- solve(lazy$upper, b)
  expect_equal(non_lazy_u_sol, lazy_u_sol)
})

#--------------------------------------------------
# Linear Regression with LSQR ####
#--------------------------------------------------
test_that("Linear regression with LSQR works", {
  # 1. Define non lazy matrix
  set.seed(123)
  mat_a <- matrix(rnorm(500), nrow = 50, ncol = 10)
  scaled_a <- base::scale(mat_a)

  # 2. Define response y
  set.seed(456)
  y <- stats::rnorm(nrow(mat_a))

  # 3. Base using lm.fit
  base_coeff <- stats::lm.fit(scaled_a, y)$coefficients

  # 3. Define lazy object
  lazy_a <- LazyMatrix(mat_a, "sd", "mean")
  beta_lazy <- lsqr(lazy_a, y)

  # 6. Tests
  expect_equal(as.vector(beta_lazy), as.vector(base_coeff), tolerance = 1e-6)
})

# PCA ####
test_that("PCA works", {
  # 1. Define non lazy matrix
  set.seed(123)
  mat_a <- matrix(rnorm(500), nrow = 50, ncol = 10)

  # 2. Define LazyMatrix
  lazy_a <- LazyMatrix(mat_a, "sd", "mean")

  # 3. Fit pca
  k <- min(nrow(mat_a), ncol(mat_a)) - 1
  pca_normal <- stats::prcomp(mat_a, scale = TRUE, center = TRUE, rank. = 3)
  pca_lazy <- prcomp(lazy_a, rank = 3)

  # 4. Test with tolerance due to iterative nature of irlba
  expect_equal(
    abs(pca_normal$rotation),
    abs(pca_lazy$rotation),
    tolerance = 1e-5
  )
  expect_equal(pca_normal$sdev[1:3], pca_lazy$sdev[1:3], tolerance = 1e-5)
  expect_equal(abs(pca_normal$x), abs(pca_lazy$x), tolerance = 1e-5)
})
