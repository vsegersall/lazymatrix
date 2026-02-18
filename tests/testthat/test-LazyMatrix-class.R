test_that("Basic lazy matrix operation work! ", {
  # 1. Create 2 regular matrices
  mat.a <- matrix(1:4, 2, 2)
  mat.b <- matrix(5:8, 2, 2)

  # 2. Create 2 lazy matrices
  a <- LazyMatrix(mat.a)
  b <- LazyMatrix(mat.b)

  # 3. Addition
  ## should trigger only the lazy representation
  c <- addition(a, b)
  expect_s4_class(c, "LazyMatrix")
  # add a line for checking that "addition" is stored

  # 4. Computation
  ## here we test the actual computation which is being done in a
  ## another method
  #expect_equal(c@data, mat.a + mat.b)
  #expect_equal(lazy.subtraction(a, b), mat.a - mat.b)
})
