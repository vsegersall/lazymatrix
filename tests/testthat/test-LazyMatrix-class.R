# === Helper functions === ####
gradient_descent_helper <- function(x, y, w_init, b_init,
                                    learning_rate, n_epochs){
  w <- w_init
  b <- b_init
  n <- nrow(x)
  for (epoch in 1:n_epochs){
    y_pred <- base::as.vector(x %*% w) + b
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
  expect_equal(length(a@col_scales), 0)
  expect_equal(length(a@row_scales), 0)
  expect_equal(length(a@col_locations), 0)
  expect_equal(length(a@row_locations), 0)
})

# Matrix multiplication ####
test_that("Lazy multiplication computation works", {
  # 1. Define non-lazy object
  mat_a <- base::matrix(c(1, 2, 3,
                          1, 2, 3), nrow=2, ncol=3)
  scaled_a <- scale(mat_a)


  # 2. Define lazy object
  lazy_a <- LazyMatrix(mat_a, "sd", "mean")

  # 3. Define test objects
  b <- c(1, 2, 3)
  c <- c(1, 2)
  set.seed(123)
  m <- base::matrix(rnorm(6), nrow=3,
                    ncol=2)

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

# Transpose ####
test_that("Lazy tranpose works", {
  mat.a <- base::matrix(c(1, 2, 3,
                          1, 2, 3), nrow=2, ncol=3)
  mat.at <- base::t(mat.a)
  lazy.a <- LazyMatrix(mat.a, "sd", "mean")
  lazy.at <- t(lazy.a)
  expect_s4_class(lazy.at, "LazyMatrix")
  expect_equal(mat.at, lazy.at@data)
})

# Crossproduct ####
test_that("Crossprod works", {
  mat_a <- base::matrix(c(1, 2, 3,
                          1, 2, 3), nrow=2, ncol=3)
  mat_at <- base::t(mat_a)
  lazy_a <- LazyMatrix(mat_a, "sd", "mean")

  # crossprod() with vector
  b <- c(1, -1)
  expected.means <- Matrix::colMeans(mat_a)
  expected.locations <- base::matrix(0, nrow=nrow(mat_a),
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
test_that("SVD works", {
  # 1. Define non lazy matrix
  set.seed(123)
  mat_a <- matrix(rnorm(500), nrow=50, ncol=10)
  scaled_a <- scale(mat_a)

  # 2. Define LazyMatrix
  lazy_a <- LazyMatrix(mat_a, scale = "sd",
                    location = "mean")
  n <- nrow(lazy_a)
  p <- ncol(lazy_a)

  # 3. Perform SVD
  svd_norm <- base::svd(scaled_a)
  svd_lazy <- svd(lazy_a, nu=n, nv=p)

  # 4. Test
  expect_equal(svd_norm$d[1:5], svd_lazy$d[1:5])
  expect_equal(dim(svd_lazy$u), dim(svd_norm$u))
})

# === Type Tests === ####
# sparseMatrix ####
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


# === Algorithmic Tests === ####

# Gradient descent ####
test_that("Gradient descent algorithm works", {
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
test_that("Cholesky decomposition works", {
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

# Linear Regression ####
test_that("Linear regression works", {
  # 1. Define response y
  set.seed(456)
  y <- stats::rnorm(nrow(mat_a))

  # 2. Define non lazy matrix
  set.seed(123)
  mat_a <- matrix(rnorm(500), nrow=50, ncol=10)
  scaled_a <- base::scale(mat_a)
  mat_df <- as.data.frame(scaled_a)
  model_a <- Matrix::sparse.model.matrix(~ . - 1, data = mat_df)

  # 3. Base using lm.fit
  mod_base <- MatrixModels:::lm.fit.sparse(x=model_a, y = y, method = "qr")

  # 4. Base using svd
  svd_normal <- base::svd(scaled_a, nu= 9,
                          nv = 9)
  v_n <- svd_normal$v
  u_tn <- t(svd_normal$u)
  d_invn <- diag(1/svd_normal$d[1:9])
  beta_norm <- v_n %*% d_invn %*% u_tn %*% y
  expect_equal(as.vector(beta_norm), mod_base)

  # 3. Define lazy object
  #mat_a <- cbind(1, mat_a)
  lazy_a <- LazyMatrix(mat_a, "sd", "mean")
  n <- nrow(lazy_a)
  p <- ncol(lazy_a)
  svd_lazy <- svd(lazy_a, nu=n, nv=p)
  v <- svd_lazy$v
  u_t <- t(svd_lazy$u)
  d_inv <- Matrix::diag(1/svd_lazy$d)
  beta_lazy <- v %*% d_inv %*%  u_t %*% y

  # 6. Tests
  expect_equal(v, v_n)
  expect_equal(u_t, u_tn)
  expect_equal(beta_lazy, beta_norm)

  # 3. Define response
  set.seed(456)
  y <- stats::rnorm(nrow(mat_a))

  # 4. Compute coefficients
  mod_base <- stats::lm.fit(x=model_a, y = y, method = "qr")
  beta_base <- mod_base$coefficients
  beta_lazy <- v %*% d_inv %*%  u_t %*% y


  # 4. Fit models
  #mod_base <- stats::lm.fit(x=scaled_a, y = y, method = "qr")
  #mod_lazy <- MatrixModels:::lm.fit.sparse(x=lazy_a, y = y, method = "qr")

  # 5. Tests
  #expect_equal(mod_base$coefficients, mod_lazy$coefficients)
  #expect_equal(mod_base$residuals, mod_lazy$residuals)
  #expect_equal(mod_base$fitted.values, mod_lazy$fitted.values)
})

# PCA ####
test_that("PCA works", {
  # 1. Define non lazy matrix
  set.seed(123)
  mat_a <- matrix(rnorm(500), nrow=50, ncol=10)

  # 2. Define LazyMatrix
  lazy_a <- LazyMatrix(mat_a, "sd", "mean")

  # 3. Fit pca
  k <- min(nrow(mat_a), ncol(mat_a)) - 1
  pca_normal <- stats::prcomp(mat_a, scale=TRUE,
                              center=TRUE, rank. = k)
  pca_lazy <- prcomp(lazy_a)

  # 4. Test with tolerance due to iterative nature of irlba
  expect_equal(abs(pca_normal$rotation), abs(pca_lazy$rotation), tolerance = 1e-5)
  expect_equal(pca_normal$sdev[1:9], pca_lazy$sdev[1:9], tolerance = 1e-5)
  expect_equal(abs(pca_normal$x), abs(pca_lazy$x), tolerance = 1e-5)
})
