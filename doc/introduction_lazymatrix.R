## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(lazymatrix)
set.seed(123)
sparse_matrix <- Matrix::Matrix(0, 5, 3)
sparse_matrix[sample(length(sparse_matrix), 5)] <- rnorm(5)
b <- rnorm(3)
lazy_matrix <- LazyMatrix(sparse_matrix, scale = "sd", location = "mean")
lazy_matrix

## ----transpose----------------------------------------------------------------
lazy_transpose <- t(lazy_matrix)
lazy_transpose

## ----matrix operations--------------------------------------------------------
set.seed(123)
b <- rnorm(ncol(lazy_matrix))
c <- rnorm(nrow(lazy_matrix))
product <- lazy_matrix %*% b
cross_product <- crossprod(lazy_matrix, c)
product
cross_product

