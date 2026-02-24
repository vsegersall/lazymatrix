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

test_that("Lazy multiplication computation works. ", {
  # Expected outcome
  mat.a <- matrix(1:4, 2, 2)
  b <- c(1, 2)
  expected.location <- Matrix::colMeans(mat.a)
  expected.sd <- base::apply(mat.a, 2, sd)
  expected.scale <- 1/expected.sd
  expected.product <- mat.a %*% expected.scale * b - expected.location * expected.scale * b

  # Observed outcome
  a <- LazyMatrix(mat.a, "sd", "mean")
  observed.product <- a %*% b

  # Test
  expect_equal(expected.product, observed.product)
})

test_that("Lazy tranpose works. ", {
  mat.a <- base::matrix(c(1, 2, 3,
                          1, 2, 3), nrow=2, ncol=3)
  mat.at <- base::t(mat.a)
  lazy.a <- LazyMatrix(mat.a, "sd", "mean")
  lazy.at <- t(lazy.a)
  expect_s4_class(lazy.at, "LazyMatrix")
  expect_equal(mat.at, lazy.at@data)
})

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
  exp.outcome <- as.vector(exp.outcome)
  obs.outcome <- crossprod(lazy_a, b)
  expect_equal(exp.outcome, obs.outcome)

  # gram matrix
  #exp.gram <- t(scale.mat) %*% (mat.at - t(expected.locations)) %*% (mat.a - expected.locations) %*% scale.mat
  #obs.gram <- lazy.at %*% lazy.a
  #expect_equal(exp.gram, obs.gram)
})

