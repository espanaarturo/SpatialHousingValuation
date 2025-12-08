#' Null coalescing operator
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

#' Select numeric columns safely
numeric_cols <- function(df) {
  names(Filter(is.numeric, df))
}

