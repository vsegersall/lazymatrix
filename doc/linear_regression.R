## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(lazymatrix)
 # 1. Define sparseMatrix
set.seed(123)
n_row <- 50
n_col <- 10
i <- c(
  1:50,
  sample(1:50, 20, replace = TRUE),
  sample(1:50, 15, replace = TRUE)
)
j <- c(
  rep_len(1:10, 50),
  sample(1:10, 20, replace = TRUE),
  sample(1:10, 15, replace = TRUE)
)
pairs <- unique(data.frame(i = i, j = j))
i <- pairs$i
j <- pairs$j
x <- rnorm(length(i))
A <- Matrix::sparseMatrix(i = i, j = j, x = x, dims = c(n_row, n_col))

# 2. Define response vector
y <- rnorm(nrow(A))

# 3. Define LazyMatrix
X <- LazyMatrix(A, "sd", "mean")

# 4. Perform lsqr
lazy_beta <- lsqr(X, y)

## ----non lazy approach--------------------------------------------------------
scaled_a <- base::scale(A)
non_lazy_beta <- stats::lm.fit(scaled_a, y)$coefficients

## ----result-------------------------------------------------------------------
isTRUE(all.equal(as.vector(lazy_beta), as.vector(non_lazy_beta)))

