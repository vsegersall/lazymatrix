test_that("Lazy matrix stores addition properly without transforms. ", {
  # 1. Create 2 regular matrices
  mat.a <- matrix(1:4, 2, 2)
  mat.b <- matrix(5:8, 2, 2)

  # 2. Create 2 lazy matrices
  a <- LazyMatrix(mat.a)
  b <- LazyMatrix(mat.b)

  # 3. Addition
  ## should test only the lazy representation
  c <- addition(a, b)
  expect_s4_class(c, "LazyMatrix")
  expect_equal(c@transformations[[1]]$operation, "add")
  expect_identical(c@transformations[[1]]$addent, b)
  # add a line for checking that "addition" is stored

  # 4. Computation
  ## here we test the actual computation which is being done in a
  ## another method
  #expect_equal(c@data, mat.a + mat.b)
  #expect_equal(lazy.subtraction(a, b), mat.a - mat.b)
})

test_that("Lazy matrix stores addition properly with transforms. ", {
  # 1. Create 2 regular matrices
  mat.a <- matrix(1:4, 2, 2)
  mat.b <- matrix(5:8, 2, 2)

  # 2. Create 2 lazy matrices
  a <- LazyMatrix(mat.a, "scale")
  b <- LazyMatrix(mat.b, "interaction")

  # 3. Addition
  ## should test only the lazy representation
  c <- addition(a, b)
  expect_s4_class(c, "LazyMatrix")
  expect_equal(c@transformations[[2]]$operation, "add")
  expect_identical(c@transformations[[2]]$addent, b)
  # add a line for checking that "addition" is stored

  # 4. Computation
  ## here we test the actual computation which is being done in a
  ## another method
  #expect_equal(c@data, mat.a + mat.b)
  #expect_equal(lazy.subtraction(a, b), mat.a - mat.b)
})

test_that("Lazy matrix stores addition properly with multiple transforms. ", {
  # 1. Create 2 regular matrices
  mat.a <- matrix(1:4, 2, 2)
  mat.b <- matrix(5:8, 2, 2)

  # 2. Create 2 lazy matrices
  a <- LazyMatrix(mat.a, c("scale",
                           "interaction"))
  b <- LazyMatrix(mat.b, "interaction")

  # 3. Addition
  ## should test only the lazy representation
  c <- addition(a, b)
  expect_s4_class(c, "LazyMatrix")
  expect_equal(c@transformations[[3]]$operation, "add")
  expect_identical(c@transformations[[3]]$addent, b)
  # add a line for checking that "addition" is stored

  # 4. Computation
  ## here we test the actual computation which is being done in a
  ## another method
  #expect_equal(c@data, mat.a + mat.b)
  #expect_equal(lazy.subtraction(a, b), mat.a - mat.b)
})
