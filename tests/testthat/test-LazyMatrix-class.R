test_that("Class definition works fine. ", {
  mat.a <- matrix(1:4, 2, 2)
  a <- LazyMatrix(mat.a)
  b <- LazyMatrix(mat.a, "sd")
  c <- LazyMatrix(mat.a, "sd",
                  "mean")
  # 1. Check that each type is a LazyMatrix object
  expect_s4_class(a, "LazyMatrix")
  expect_s4_class(b, "LazyMatrix")
  expect_s4_class(c, "LazyMatrix")

  # 2. Check that we stored location properly
  expected.location <- Matrix::colMeans(mat.a)
  observed.location <- c@locations
  expect_equal(expected.location, observed.location)

  # 3. Check that we stored scale properly
  expected.scale <- apply(mat.a, 2, sd)
  observed.scale.1arg <- b@scales
  expect_equal(expected.scale, observed.scale.1arg)
  observed.scale.2args <- c@scales
  expect_equal(expected.scale, observed.scale.2args)
})

test_that("Lazy multiplication computation works. ", {
  # Expected outcome
  mat.a <- matrix(1:4, 2, 2)
  b <- c(1, 2)
  expected.location <- Matrix::colMeans(mat.a)
  expected.sd <- apply(mat.a, 2, sd)
  expected.scale <- 1/expected.sd
  expected.product <- mat.a %*% expected.scale * b - expected.location * expected.scale * b

  # Observed outcome
  a <- LazyMatrix(mat.a, "sd", "mean")
  observed.product <- a %*% b

  # Test
  expect_equal(expected.product, observed.product)
})

test_that("Lazy transpose computation works. ", {
  # Expected outcome
  mat.a <- matrix(c(1, 2, 3,
                    1, 2, 3), nrow=2, ncol=3)
  expected.means <- Matrix::colMeans(mat.a)
  expected.locations <- matrix(0, nrow=nrow(mat.a),
         ncol=ncol(mat.a))
  for (i in 1:nrow(expected.locations)){
    expected.locations[i,] <- expected.means
  }
  expected.sd <- apply(mat.a, 2, sd)
  expected.scale <- 1/expected.sd
  mat.a.t <- t(mat.a)
  expected.transpose <- expected.scale %*% mat.a.t  - expected.scale %*% t(expected.locations)

  # Observed outcome
  a <- LazyMatrix(mat.a, "sd", "mean")
  observed.transpose <- t(a)

  # Test
  expect_equal(expected.transpose, observed.transpose)
})
