# === Helper functions === ####
gradient_descent_helper <- function(x, y, w_init, b_init,
                                    learning_rate, n_epochs){
  w <- w_init
  b <- b_init
  n <- nrow(x)
  for (epoch in 1:n_epochs){
    y_pred <- as.vector(x %*% w) + b
    error <- y_pred - y
    w <- w - (learning_rate * crossprod(x, error))/n
    b <- b - (learning_rate * sum(error))/n
  }
  return(list(w=w, b=b))
}

cholesky_decomp <- function(A){
  L <- Matrix::chol(A)
  M <- t(L)
  A_new <- M %*% L
  return(base::list(A=A,  "upper" = L,
              "lower"=M))
}

linear_regression <- function(x, y){
  x <- as.matrix(x)
  return(stats::lm(y~x))
}

# === Function Tests === ####

# Class definition ####
test_that("Class definition works fine.", {
  mat.a <- matrix(1:4, 2, 2)
  a <- LazyMatrix(mat.a)
  b <- LazyMatrix(mat.a, scale = "sd")
  c <- LazyMatrix(mat.a, scale = "sd", location = "mean")

  # 1. Check that each type is a LazyMatrix object
  expect_s4_class(a, "LazyMatrix")
  expect_s4_class(b, "LazyMatrix")
  expect_s4_class(c, "LazyMatrix")

  # 2. Check that we stored column locations properly
  expected.col_location <- Matrix::colMeans(mat.a)
  observed.col_location <- c@col_locations
  expect_equal(expected.col_location, observed.col_location)

  # 3. Check that we stored row locations properly
  expected.row_location <- Matrix::rowMeans(mat.a)
  observed.row_location <- c@row_locations
  expect_equal(expected.row_location, observed.row_location)

  # 4. Check that we stored column scales properly
  expected.col_scale <- base::apply(mat.a, 2, sd)
  observed.col_scale.1arg <- b@col_scales
  expect_equal(expected.col_scale, observed.col_scale.1arg)
  observed.col_scale.2args <- c@col_scales
  expect_equal(expected.col_scale, observed.col_scale.2args)

  # 5. Check that we stored row scales properly
  expected.row_scale <- base::apply(mat.a, 1, sd)
  observed.row_scale.1arg <- b@row_scales
  expect_equal(expected.row_scale, observed.row_scale.1arg)
  observed.row_scale.2args <- c@row_scales
  expect_equal(expected.row_scale, observed.row_scale.2args)

  # 6. Check that empty LazyMatrix has numeric(0) for all params
  expect_equal(length(a@col_scales), 0)
  expect_equal(length(a@row_scales), 0)
  expect_equal(length(a@col_locations), 0)
  expect_equal(length(a@row_locations), 0)
})

# Matrix multiplication ####
test_that("Lazy multiplication computation works. ", {
  # Expected outcome
  mat.a <- base::matrix(c(1, 2, 3,
                          1, 2, 3), nrow=2, ncol=3)
  b <- c(1, 2, 3)
  expected.location <- Matrix::colMeans(mat.a)
  expected.sd <- base::apply(mat.a, 2, sd)
  expected.scale <- 1/expected.sd
  expected.product <- mat.a %*% (expected.scale * b) - sum(expected.location * expected.scale * b)

  # Observed outcome
  a <- LazyMatrix(mat.a, "sd", "mean")
  observed.product <- a %*% b

  # Test
  expect_equal(expected.product, observed.product)
})

# Transpose ####
test_that("Lazy tranpose works. ", {
  mat.a <- base::matrix(c(1, 2, 3,
                          1, 2, 3), nrow=2, ncol=3)
  mat.at <- base::t(mat.a)
  lazy.a <- LazyMatrix(mat.a, "sd", "mean")
  lazy.at <- t(lazy.a)
  expect_s4_class(lazy.at, "LazyMatrix")
  expect_equal(mat.at, lazy.at@data)
})

# Crossproduct ####
test_that("Crossprod works. ", {
  mat_a <- base::matrix(c(1, 2, 3,
                          1, 2, 3), nrow=2, ncol=3)
  mat_at <- base::t(mat_a)
  lazy_a <- LazyMatrix(mat_a, "sd", "mean")

  # crossprod() with vector
  b <- c(1, -1)
  expected.means <- Matrix::colMeans(mat_a)
  expected.locations <- matrix(0, nrow=nrow(mat_a),
                               ncol=ncol(mat_a))
  for (i in 1:nrow(expected.locations)){
    expected.locations[i,] <- expected.means
  }
  expected.sd <- base::apply(mat_a, 2, sd)
  expected.scale <- 1/expected.sd
  scale.mat <- Matrix::Diagonal(n = length(expected.scale),
                                x = expected.scale)

  exp.outcome <- t(scale.mat) %*% (mat_at - t(expected.locations)) %*% b
  #exp.outcome <- as.vector(exp.outcome)
  obs.outcome <- crossprod(lazy_a, b)
  expect_equal(exp.outcome, obs.outcome)

  # gram matrix
  exp.gram <- t(scale.mat) %*% (mat_at - t(expected.locations)) %*% (mat_a - expected.locations) %*% scale.mat
  obs.gram <- crossprod(lazy_a)
  expect_equal(exp.gram, obs.gram)
})

# Sparse SVD with irlba ####
test_that("SVD works. ", {
  # 1. Define non lazy matrix
  set.seed(123)
  mat_a <- matrix(rnorm(500), nrow=50, ncol=10)
  scaled_a <- scale(mat_a)

  # 2. Define LazyMatrix
  lazy_a <- LazyMatrix(mat_a, scale="sd",
                       location = "mean")

  # 3. Perform SVD
  svd_norm <- base::svd(scaled_a)
  svd_lazy <- svd(lazy_a)

  # 4. Test
  expect_equal(svd_norm$d[1:5], svd_lazy$d[1:5])
})

# === Algorithmic Tests === ####

# Gradient descent ####
test_that("Gradient descent algorithm works. ", {
  set.seed(2121)

  # 1. Set up design matrix, non-lazy
  n <- 50
  p <- 5
  mat_a <- base::matrix(stats::rnorm(n*p), nrow = n,
                  ncol = p)
  mat_at <- base::t(mat_a)
  expected.means <- Matrix::colMeans(mat_a)
  expected.locations <- matrix(0, nrow=nrow(mat_a),
                               ncol=ncol(mat_a))
  for (i in 1:nrow(expected.locations)){
    expected.locations[i,] <- expected.means
  }
  expected.sd <- base::apply(mat_a, 2, sd)
  expected.scale <- 1/expected.sd
  scale.mat <- Matrix::Diagonal(n = length(expected.scale),
                                x = expected.scale)
  scaled_a <- (mat_a - expected.locations) %*% scale.mat

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
  pars_nonlazy <- gradient_descent_helper(x=scaled_a, y=y_true,
                                          w_init=w_init, b_init=b_init,
                                          learning_rate=learning_rate,
                                          n_epochs=n_epochs)
  pars_lazy <- gradient_descent_helper(x=lazy_a, y=y_true,
                                          w_init=w_init, b_init=b_init,
                                          learning_rate=learning_rate,
                                          n_epochs=n_epochs)
  preds_nonlazy <- scaled_a %*% pars_nonlazy$w + pars_nonlazy$b
  preds_lazy <- lazy_a %*% pars_lazy$w + pars_lazy$b

  # 5. Test
  expect_equal(pars_nonlazy$w, pars_lazy$w)
  expect_equal(pars_nonlazy$b, pars_lazy$b)
  expect_equal(preds_nonlazy, preds_lazy)
})

# Cholesky decomposition ####
test_that("Cholesky decomposition works. ", {
  # 1. Define non lazy matrix
  set.seed(123)
  mat_a <- matrix(rnorm(30), nrow=10, ncol=3)  # 10×3 matris
  b <- c(1, 2, 3)
  expected.means <- Matrix::colMeans(mat_a)
  expected.locations <- matrix(0, nrow=nrow(mat_a),
                               ncol=ncol(mat_a))
  for (i in 1:nrow(expected.locations)){
    expected.locations[i,] <- expected.means
  }
  expected.sd <- base::apply(mat_a, 2, sd)
  expected.scale <- 1/expected.sd
  scale.mat <- Matrix::Diagonal(n = length(expected.scale),
                                x = expected.scale)
  scaled_a <- (mat_a - expected.locations) %*% scale.mat

  # 2. Define lazy object
  lazy_a <- LazyMatrix(mat_a, "sd", "mean")

  # 3. Construct symmetric positive definit matrices
  gram_manual <- t(scaled_a) %*% scaled_a
  gram_lazy <- crossprod(lazy_a)

  # 4. Test for positive definite
  expect_equal(gram_manual, gram_lazy)
  eigen_manual <- eigen(gram_lazy)$values
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

# Linear Regression ####
test_that("Linear regression works. ", {
  # 1. Define non lazy matrix
  set.seed(123)
  mat_a <- matrix(rnorm(30), nrow=10, ncol=3)
  expected.means <- Matrix::colMeans(mat_a)
  expected.locations <- matrix(0, nrow=nrow(mat_a),
                               ncol=ncol(mat_a))
  for (i in 1:nrow(expected.locations)){
    expected.locations[i,] <- expected.means
  }
  expected.sd <- base::apply(mat_a, 2, sd)
  expected.scale <- 1/expected.sd
  scale.mat <- Matrix::Diagonal(n = length(expected.scale),
                                x = expected.scale)
  scaled_a <- (mat_a - expected.locations) %*% scale.mat

  # 2. Define lazy object
  lazy_a <- LazyMatrix(mat_a, "sd", "mean")

  # 3. Define response
  set.seed(456)
  y <- rnorm(nrow(mat_a))

  # 4. Fit models
  mod_base <- linear_regression(scaled_a, y)
  mod_lazy <- linear_regression(lazy_a, y)

  # 5. Tests
  #expect_s3_class(mod_lazy, "lm")
  expect_equal(coef(mod_base), coef(mod_lazy))
  expect_equal(fitted(mod_base), fitted(mod_lazy))
  expect_equal(residuals(mod_base), residuals(mod_lazy))
})
