#' Coercion between a matrix and a tibble
#'
#' Convenience function for coercing a \eqn{d * n} matrix into a tibble with \eqn{n} rows and
#' \eqn{d} columns, where \eqn{n} is the number of observations and \eqn{d} is the number of
#' variables, and vice-versa.
#'
#' @param x A matrix or a tibble.
#' @name reshape
NULL

#' @export
#' @rdname reshape
#' @importFrom tibble as_tibble
matrix_to_tibble <- function(x) {
  as_tibble(t(x))
}

#' @export
#' @rdname reshape
tibble_to_matrix <- function(x) {
  t(as.matrix(x))
}
