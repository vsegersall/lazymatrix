#--------------------------------------------------
# LazyMatrix ####
#--------------------------------------------------

#--------------------------------------------------
## Regular subsetting ####
#--------------------------------------------------
test_that("Subsetting works as base R.", {
  # 1. Define test matrix
  set.seed(123)
  mat_a <- matrix(rnorm(500), nrow = 50, ncol = 10)

  # 2. Define Regular LazyMatrix
  lazy_a <- LazyMatrix(mat_a, "sd", "mean")

  # 3. Single column case, all rows
  lazy_column <- lazy_a[, 2]
  dense_column <- mat_a[, 2]
  dense_mean <- base::mean(mat_a[, 2])
  dense_sd <- stats::sd(mat_a[, 2])
  expect_equal(lazy_column@data, dense_column)
  expect_equal(lazy_column@location, dense_mean)
  expect_equal(lazy_column@scale, dense_sd)

  # 4. Multiple Column Case, all rows
  lazy_subset <- lazy_a[, 5:10]
  dense_subset <- mat_a[, 5:10]
  dense_means <- base::apply(dense_subset, 2, base::mean)
  dense_sds <- base::apply(dense_subset, 2, stats::sd)
  expect_equal(lazy_subset@data, dense_subset)
  expect_equal(lazy_subset@col_locations, dense_means)
  expect_equal(lazy_subset@col_scales, dense_sds)

  # 5. One column, multiple rows
  lazy_col_2 <- lazy_a[2:4, 5]
  dense_col_2 <- scale(mat_a[2:4, 5])
  dense_mean_2 <- attr(dense_col_2, "scaled:center")
  dense_sd_2 <- attr(dense_col_2, "scaled:scale")
  expect_equal(lazy_col_2@data, mat_a[2:4, 5])
  expect_equal(lazy_col_2@location, dense_mean_2)
  expect_equal(lazy_col_2@scale, dense_sd_2)

  # 6. Multiple columns, multiple rows
  lazy_subset_2 <- lazy_a[2:7, 5:10]
  dense_subset_2 <- mat_a[2:7, 5:10]
  scaled_subset <- scale(dense_subset_2)
  expect_equal(lazy_subset_2@data, mat_a[2:7, 5:10])
  expect_equal(lazy_subset_2@col_scales, attr(scaled_subset, "scaled:scale"))
  expect_equal(
    lazy_subset_2@col_locations,
    attr(scaled_subset, "scaled:center")
  )

  # 7. Single element case
  lazy_element <- lazy_a[5, 2]
  dense_element <- base::scale(mat_a)[5, 2]
  expect_equal(lazy_element, dense_element)

  # 8. Logical subsetting
})

#--------------------------------------------------
## Logical subsetting ####
#--------------------------------------------------
test_that("Logical column subsetting works", {
  skip("Logical subsetting not yet implemented")
  set.seed(123)
  mat_a <- matrix(rnorm(500), nrow = 50, ncol = 10)
  lazy_a <- LazyMatrix(mat_a, "sd", "mean")
  scaled_a <- scale(mat_a)

  # 1. Single TRUE — recycled across all columns → LazyMatrix with all 10 columns
  result <- lazy_a[, TRUE]
  expect_s4_class(result, "LazyMatrix")
  expect_equal(ncol(result), 10)

  # 2. Single FALSE — recycled across all columns → selects nothing
  expect_error(lazy_a[, FALSE])

  # 3. Exact length logical — selects specific columns → LazyMatrix
  col_mask <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
  result <- lazy_a[, col_mask]
  expect_s4_class(result, "LazyMatrix")
  expect_equal(ncol(result), 5)

  # 4. Exact length logical selecting single column → LazyColumn
  col_mask_single <- c(
    FALSE,
    TRUE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE
  )
  result <- lazy_a[, col_mask_single]
  expect_s4_class(result, "LazyColumn")

  # 5. Recycled length-2 vector → selects odd columns
  result <- lazy_a[, c(TRUE, FALSE)]
  expect_s4_class(result, "LazyMatrix")
  expect_equal(ncol(result), 5)

  # 6. Recycled length-2 vector → selects even columns
  result <- lazy_a[, c(FALSE, TRUE)]
  expect_s4_class(result, "LazyMatrix")
  expect_equal(ncol(result), 5)

  # 7. All TRUE vector → all columns selected
  result <- lazy_a[, rep(TRUE, 10)]
  expect_s4_class(result, "LazyMatrix")
  expect_equal(ncol(result), 10)

  # 8. All FALSE vector → selects nothing
  expect_error(lazy_a[, rep(FALSE, 10)])

  # 9. Values are correct after logical subsetting
  col_mask <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
  result <- lazy_a[, col_mask]
  expect_equal(
    as.matrix(result),
    scaled_a[, col_mask],
    tolerance = 1e-6
  )

  # 10. Non-multiple length logical → warning about recycling
  expect_warning(lazy_a[, c(TRUE, FALSE, TRUE)])
})

#--------------------------------------------------
## Negative Column subsetting ####
#--------------------------------------------------
test_that("Negative column subsetting works", {
  skip("Negative subsetting not yet implemented")

  set.seed(123)
  mat_a <- matrix(rnorm(500), nrow = 50, ncol = 10)
  lazy_a <- LazyMatrix(mat_a, "sd", "mean")
  scaled_a <- scale(mat_a)

  # 1. Remove single column → LazyMatrix with 9 columns
  result <- lazy_a[, -1]
  expect_s4_class(result, "LazyMatrix")
  expect_equal(ncol(result), 9)

  # 2. Remove last column
  result <- lazy_a[, -10]
  expect_s4_class(result, "LazyMatrix")
  expect_equal(ncol(result), 9)

  # 3. Remove multiple columns → LazyMatrix
  result <- lazy_a[, -c(1, 3, 5)]
  expect_s4_class(result, "LazyMatrix")
  expect_equal(ncol(result), 7)

  # 4. Remove all but one column → LazyColumn
  result <- lazy_a[, -c(1, 2, 3, 4, 5, 6, 7, 8, 9)]
  expect_s4_class(result, "LazyColumn")

  # 5. Remove all columns → error
  expect_error(lazy_a[, -c(1:10)])

  # 6. Values are correct after negative subsetting
  result <- lazy_a[, -c(1, 3, 5)]
  expect_equal(
    as.matrix(result),
    scaled_a[, -c(1, 3, 5)],
    tolerance = 1e-6
  )

  # 7. Out of bounds negative index → error
  expect_error(lazy_a[, -11])
})

#--------------------------------------------------
## Character Column subsetting ####
#--------------------------------------------------
test_that("Character column subsetting works", {
  skip("Character subsetting not yet implemented")

  set.seed(123)
  mat_a <- matrix(rnorm(500), nrow = 50, ncol = 10)
  colnames(mat_a) <- paste0("V", 1:10)
  lazy_a <- LazyMatrix(mat_a, "sd", "mean")
  scaled_a <- scale(mat_a)

  # 1. Single column name → LazyColumn
  result <- lazy_a[, "V1"]
  expect_s4_class(result, "LazyColumn")

  # 2. Multiple column names → LazyMatrix
  result <- lazy_a[, c("V1", "V3", "V5")]
  expect_s4_class(result, "LazyMatrix")
  expect_equal(ncol(result), 3)

  # 3. All column names → LazyMatrix with all columns
  result <- lazy_a[, paste0("V", 1:10)]
  expect_s4_class(result, "LazyMatrix")
  expect_equal(ncol(result), 10)

  # 4. Non-existent column name → error
  expect_error(lazy_a[, "nonexistent"])

  # 5. Mix of valid and invalid names → error
  expect_error(lazy_a[, c("V1", "nonexistent")])

  # 6. Values are correct after character subsetting
  result <- lazy_a[, c("V1", "V3", "V5")]
  expect_equal(
    as.matrix(result),
    scaled_a[, c("V1", "V3", "V5")],
    tolerance = 1e-6
  )

  # 7. LazyMatrix without colnames → error
  mat_no_names <- matrix(rnorm(500), nrow = 50, ncol = 10)
  lazy_no_names <- LazyMatrix(mat_no_names, "sd", "mean")
  expect_error(lazy_no_names[, "V1"])
})

#--------------------------------------------------
# LazyColumn ####
#--------------------------------------------------
#--------------------------------------------------
## Positive integers ####
#--------------------------------------------------
test_that("Postive integers return elements at the specified positions", {
  # 1. Define test objects
  set.seed(123)
  mat_a <- matrix(rnorm(500), nrow = 50, ncol = 10)
  lazy_a <- LazyMatrix(mat_a, "sd", "mean")
  lazy_c <- lazy_a[, 2]
  dense_c <- mat_a[, 2]

  # 2. Single element → returns a scaled numeric value, not a LazyColumn
  dense_1 <- scaled_c[2]
  lazy_1 <- lazy_c[2]
  expect_type(lazy_1, "double")
  expect_equal(lazy_1, dense_1)

  # 3. Positive integer vector
  dense_1 <- dense_c[c(3, 1, 5)]
  lazy_1 <- lazy_c[c(3, 1, 5)]
  expect_equal(lazy_1@data, dense_1)
  expect_equal(lazy_1@scale, attr(scale(dense_1), "scaled:scale"))
  expect_equal(lazy_1@location, attr(scale(dense_1), "scaled:center"))

  # 4. Duplicate indices will duplicate values
  dense_1 <- dense_c[c(3, 3, 3)]
  lazy_1 <- lazy_c[c(3, 3, 3)]
  expect_equal(lazy_1@data, dense_1)
  expect_equal(lazy_1@scale, attr(scale(dense_1), "scaled:scale"))
  expect_equal(lazy_1@location, attr(scale(dense_1), "scaled:center"))

  # 5. Real numbers are silently truncated to integers
  dense_1 <- dense_c[c(3.4, 3.67, 3.9)]
  lazy_1 <- lazy_c[c(3.4, 3.67, 3.9)]
  expect_equal(lazy_1@data, dense_1)
  expect_equal(lazy_1@scale, attr(scale(dense_1), "scaled:scale"))
  expect_equal(lazy_1@location, attr(scale(dense_1), "scaled:center"))
})

#--------------------------------------------------
## Negative integers ####
#--------------------------------------------------
test_that("Negative integers exclude elements at the specified positions", {
  # 1. Define test objects
  set.seed(123)
  mat_a <- matrix(rnorm(500), nrow = 50, ncol = 10)
  lazy_a <- LazyMatrix(mat_a, "sd", "mean")
  lazy_c <- lazy_a[, 2]
  dense_c <- mat_a[, 2]

  # 2. Single excluded element
  dense_1 <- dense_c[-2]
  lazy_1 <- lazy_c[-2]
  expect_equal(lazy_1@data, dense_1)
  expect_equal(lazy_1@scale, attr(scale(dense_1), "scaled:scale"))
  expect_equal(lazy_1@location, attr(scale(dense_1), "scaled:center"))

  # 3. Multiple excluded elements
  dense_1 <- dense_c[-c(3, 1)]
  lazy_1 <- lazy_c[-c(3, 1)]
  expect_equal(lazy_1@data, dense_1)
  expect_equal(lazy_1@scale, attr(scale(dense_1), "scaled:scale"))
  expect_equal(lazy_1@location, attr(scale(dense_1), "scaled:center"))

  # 4. Mixing positive and negative indices should error
  expect_error(lazy_c[c(-1, 2)])
})

#--------------------------------------------------
## Logical Vectors ####
#--------------------------------------------------
test_that("Logical vectors select elements where the corresponding logical value is TRUE", {
  # 1. Define test objects
  set.seed(123)
  mat_a <- matrix(rnorm(500), nrow = 50, ncol = 10)
  lazy_a <- LazyMatrix(mat_a, "sd", "mean")
  lazy_c <- lazy_a[, 2]
  dense_c <- mat_a[, 2]

  # 2. TRUE TRUEE FALSE FALSE
  dense_1 <- dense_c[c(TRUE, TRUE, FALSE, FALSE)]
  lazy_1 <- lazy_c[c(TRUE, TRUE, FALSE, FALSE)]
  expect_equal(lazy_1@data, dense_1)
  expect_equal(lazy_1@scale, attr(scale(dense_1), "scaled:scale"))
  expect_equal(lazy_1@location, attr(scale(dense_1), "scaled:center"))

  # 3. TRUE FALSE is the same as TRUE FALSE TRUE FALSE
  dense_1 <- dense_c[c(TRUE, FALSE)]
  lazy_1 <- lazy_c[c(TRUE, FALSE, TRUE, FALSE)]
  expect_equal(lazy_1@data, dense_1)
  expect_equal(lazy_1@scale, attr(scale(dense_1), "scaled:scale"))
  expect_equal(lazy_1@location, attr(scale(dense_1), "scaled:center"))

  # 6. Logical statements
  dense_1 <- dense_c[dense_c > 0.2]
  lazy_1 <- lazy_c[lazy_c > 0.2]
  expect_equal(lazy_1@data, dense_1)
  expect_equal(lazy_1@scale, attr(scale(dense_1), "scaled:scale"))
  expect_equal(lazy_1@location, attr(scale(dense_1), "scaled:center"))

  # 5. Missing values
  dense_1 <- dense_c[c(TRUE, TRUE, NA, FALSE)]
  lazy_1 <- lazy_c[c(TRUE, TRUE, NA, FALSE)]
  skip("No current method for NA's exist. ")
  expect_equal(lazy_1@data, dense_1)
  expect_equal(lazy_1@scale, attr(scale(dense_1), "scaled:scale"))
  expect_equal(lazy_1@location, attr(scale(dense_1), "scaled:center"))
})

#--------------------------------------------------
## Nothing ####
#--------------------------------------------------
test_that("Nothing returns the original vector. ", {
  # 1. Define test objects
  set.seed(123)
  mat_a <- matrix(rnorm(500), nrow = 50, ncol = 10)
  lazy_a <- LazyMatrix(mat_a, "sd", "mean")
  lazy_c <- lazy_a[, 2]
  dense_c <- mat_a[, 2]

  # 2. The empty [] should return the original LazyColumn
  dense_1 <- dense_c[]
  lazy_1 <- lazy_c[]
  expect_equal(lazy_1@data, dense_1)
})

#--------------------------------------------------
## Zero ####
#--------------------------------------------------
test_that("Zero returns a zero-length vector ", {
  # 1. Define test objects
  set.seed(123)
  mat_a <- matrix(rnorm(500), nrow = 50, ncol = 10)
  lazy_a <- LazyMatrix(mat_a, "sd", "mean")
  lazy_c <- lazy_a[, 2]
  dense_c <- mat_a[, 2]

  # 2. Zero-vector
  dense_1 <- dense_c[0]
  lazy_1 <- lazy_c[0]
  expect_equal(lazy_1@data, dense_1)
  expect_equal(lazy_1@scale, 0)
  expect_equal(lazy_1@location, 0)
})

#--------------------------------------------------
## Character vectors ####
#--------------------------------------------------
test_that("Zero returns a zero-length vector ", {
  # 1. Define test objects
  set.seed(123)
  mat_a <- matrix(rnorm(500), nrow = 50, ncol = 10)
  lazy_a <- LazyMatrix(mat_a, "sd", "mean")
  lazy_c <- lazy_a[, 2]
  dense_c <- mat_a[, 2]

  # 2. setNames
  dense_data <- stats::setNames(dense_c[1:26], letters[1:length(dense_c[1:26])])
  lazy_data <- setNames(lazy_c[1:26], letters[1:length(lazy_c[1:26])])

  # 3. Subsetting using names
  dense_1 <- dense_data[c("d", "c", "a")]
  lazy_1 <- lazy_data[c("d", "c", "a")]
  expect_equal(lazy_1@data, dense_1)
  expect_equal(lazy_1@scale, attr(scale(dense_1), "scaled:scale"))
  expect_equal(lazy_1@location, attr(scale(dense_1), "scaled:center"))

  # 4. Repeating names
  dense_1 <- dense_data[c("a", "a", "a")]
  lazy_1 <- lazy_data[c("a", "a", "a")]
  expect_equal(lazy_1@data, dense_1)
  expect_equal(lazy_1@scale, attr(scale(dense_1), "scaled:scale"))
  expect_equal(lazy_1@location, attr(scale(dense_1), "scaled:center"))
})
