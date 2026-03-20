# Matrix Multiplication ####
benchmark_lazy_multiplication <- function(){
  bench::press(
    n = c(100, 500, 1000),  # Matrix sizes
    sparsity = c(0.01, 0.05, 0.1),  # Sparsity levels
    {
      # 1. Define test matrix
      set.seed(123)
      num_nonzero <- n * n * sparsity
      i <- sample(1:n, num_nonzero, replace = TRUE)  # Row indices
      j <- sample(1:n, num_nonzero, replace = TRUE)  # Column indices
      x <- runif(num_nonzero)  # Values for non-zero elements
      test_data <- Matrix::sparseMatrix(i = i, j = j, x = x, dims = c(n, n))

      # 2. Define test vector
      test_vector <- runif(n)

      # 3. Define LazyMatrix
      lazy_test <- LazyMatrix(test_data, "sd", "mean")

      # 4. Compute scaled matrix
      scaled_test <- scale(test_data)

      # 5. Benchmark
      bench::mark(
        lazy <- lazy_test %*% test_vector,
        dense <- Matrix::Matrix(scaled_test, sparse = FALSE) %*% test_vector
      )
    }
  )
}
result <- benchmark_lazy_multiplication()
print(result)
plot(result)
