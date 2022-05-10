#' Coerce a matrix to a tibble
#'
#' Convenience function for coercing a \eqn{d \times n} matrix into a tibble with \eqn{n} rows and
#' \eqn{d} columns, where \eqn{n} is the number of observations and \eqn{d} is the number of
#' variables.
#'
#' @param x A matrix.
#' @export
#' @importFrom tibble as_tibble
matrix_to_tibble <- function(x) {
  as_tibble(t(x))
}
