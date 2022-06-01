#' Compute maximum likelihood estimate of population mean
#'
#' For data from a multivariate normal distribution, the maximum likelihood estimate of the
#' population mean is computed as the sample means of the individual dimensions.
#'
#' @param x A matrix of dimension \eqn{d} by \eqn{n}, where \eqn{d} is the number of dimensions and
#' \eqn{n} is the sample size.
#' @return A matrix of dimension \eqn{d} by \eqn{1}.
#' @export
mean_mle <- function(x) {
  n <- ncol(x)

  (1/n) * as.matrix(rowSums(x))
}
