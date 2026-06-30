# Subset a LazyColumn

Subsets a `LazyColumn` object using standard R indexing rules.

## Usage

``` r
# S4 method for class 'LazyColumn,ANY,ANY,ANY'
x[i, j, ..., drop = TRUE]
```

## Arguments

- x:

  A `LazyColumn` object.

- i:

  Index: positive integers, negative integers, logical vector, character
  vector (if named), zero, or missing (nothing).

- ...:

  Additional arguments (ignored).

- drop:

  Logical. Currently ignored — single element extraction always returns
  a plain scaled numeric, consistent with vector subsetting.

## Value

A `LazyColumn` when multiple elements are selected, or a scaled numeric
value when a single element is selected.
