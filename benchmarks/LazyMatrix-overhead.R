# Benchmark for showing overhead of %*% ####
# 1. Define test matrix
n_row <- 50
n_col <- 10

n_nonzero <- round(0.05 * n_row * n_col)

i <- sample(1:n_row, n_nonzero, replace = TRUE)
j <- sample(1:n_col, n_nonzero, replace = TRUE)

pairs <- unique(data.frame(i = i, j = j))
i <- pairs$i
j <- pairs$j

x <- rnorm(length(i))
A <- Matrix::sparseMatrix(i = i, j = j, x = x, dims = c(n_row, n_col))

# 2. Define test vector
b <- rnorm(ncol(A))

# 3. Define LazyMatrix
X <- LazyMatrix(A, "sd", "mean")

# 5. Benchmarking
bm <- bench::mark(
  dense_naive = {
    A_dense <- scale(as.matrix(A), center = TRUE, scale = TRUE)
    A_dense %*% c
  },

  direct_sparse = {
    A %*% (1 / X@col_scales * c) - sum(X@col_locations * 1 / X@col_scales * c)
  },

  lazy = X %*% c,

  check = FALSE,
  min_iterations = 20
)
