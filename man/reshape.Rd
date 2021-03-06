% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reshape.R
\name{reshape}
\alias{reshape}
\alias{matrix_to_tibble}
\alias{tibble_to_matrix}
\title{Coercion between a matrix and a tibble}
\usage{
matrix_to_tibble(x)

tibble_to_matrix(x)
}
\arguments{
\item{x}{A matrix or a tibble.}
}
\description{
Convenience function for coercing a \eqn{d * n} matrix into a tibble with \eqn{n} rows and
\eqn{d} columns, where \eqn{n} is the number of observations and \eqn{d} is the number of
variables, and vice-versa.
}
