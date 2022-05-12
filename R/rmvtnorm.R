#' Sample from the (approximate) multivariate normal distribution
#'
#' @details Consider a random sample of size 12 of Uniform(0,1) random variables.
#' Let \Sexpr[results=rd, stage=build]{
#' katex::math_to_rd("X", displayMode=FALSE)
#' } be the sum of the sample elements. Then \Sexpr[results=rd, stage=build]{
#' katex::math_to_rd("X - 6", displayMode=FALSE, include_css=FALSE)
#' } has an approximate \Sexpr[results=rd, stage=build]{
#' katex::math_to_rd("N(0,1)", displayMode=FALSE, include_css=FALSE)
#' } distribution.
#'
#' Let \Sexpr[results=rd, stage=build]{
#' katex::math_to_rd("\\\mathbf{Z}", displayMode=FALSE, include_css=FALSE)
#' } be a \Sexpr[results=rd, stage=build]{
#' katex::math_to_rd("d \\\times n", displayMode=FALSE, include_css=FALSE)
#' } matrix with distribution \Sexpr[results=rd, stage=build]{
#' katex::math_to_rd(
#' "N_{d}(\\\mathbf{0},\\\,\\\mathbb{I}_{d})",
#' displayMode=FALSE, include_css=FALSE)
#' }. Let \Sexpr[results=rd, stage=build]{
#' katex::math_to_rd("X", displayMode=FALSE, include_css=FALSE)
#' } be a \Sexpr[results=rd, stage=build]{
#' katex::math_to_rd("d \\\times n", displayMode=FALSE, include_css=FALSE)
#' } random matrix with distribution \Sexpr[results=rd, stage=build]{
#' katex::math_to_rd(
#' "N_{d}(\\\bm{\\\mu},\\\,\\\bm{\\\Sigma})",
#' displayMode=FALSE, include_css=FALSE)
#' }. If \Sexpr[results=rd, stage=build]{
#' katex::math_to_rd("\\\mathbf{P}", displayMode=FALSE, include_css=FALSE)
#' } and \Sexpr[results=rd, stage=build]{
#' katex::math_to_rd("\\\mathbf{D}", displayMode=FALSE, include_css=FALSE)
#' } are the eigenvector and eigenvalue matrices of \Sexpr[results=rd, stage=build]{
#' katex::math_to_rd("\\\bm{\\\Sigma}", displayMode=FALSE, include_css=FALSE)
#' }, respectively, then
#' \Sexpr[results=rd, stage=build]{
#' katex::math_to_rd(
#' "\\\mathbf{X} \\\,=\\\, \\\\mathbf{PD}^{1/2}\\\mathbf{Z} \\\,+\\\, \\\bm{\\\mu}\\\mathbf{1}'",
#' include_css=FALSE)
#' }
#' has a \Sexpr[results=rd, stage=build]{
#' katex::math_to_rd(
#' "N_{d}(\\\bm{\\\mu},\\\,\\\bm{\\\Sigma})",
#' displayMode=FALSE, include_css=FALSE)
#' } distribution, where \Sexpr[results=rd, stage=build]{
#' katex::math_to_rd("\\\mathbf{1}", displayMode=FALSE, include_css=FALSE)
#' } is a column vector of ones.
#'
#' @param n The sample size.
#' @param mean A \eqn{d} by \eqn{1} matrix containing the means of the variables.
#' @param cov A \eqn{d} by \eqn{d} positive-definite, symmetric matrix specifying the covariance matrix
#' of the variables.
#' @return A matrix with \code{d} rows and \code{n} columns.
#' @export
#' @examples
#' mu <- matrix(c(1, 3))
#' sigma <- matrix(c(3, 1, 1, 2), 2, 2)
#' rmvtnorm(n = 5, mean = mu, cov = sigma)
rmvtnorm <- function(n, mean, cov) {
  d <- nrow(cov)

  stdnorm_sample <- matrix(
    replicate(n=n*d, rnorm2()),
    nrow=d, ncol=n, byrow=TRUE
  )

  eig <- eigen(cov)
  P <- eig$vectors
  D <- diag(eig$values)

  ones <- matrix(rep(1, n))

  X <- (P %*% sqrt(D) %*% stdnorm_sample) + (mean %*% t(ones))
  rownames(X) <- paste0("x", 1:d)

  X
}

rnorm2 <- function(uniform_sample_size=12) {
  uniform_sample <- runif(n=uniform_sample_size)

  x <- sum(uniform_sample)
  mu_x <- uniform_sample_size/2
  sigma_x <- sqrt(uniform_sample_size * (1/12))

  (x - mu_x) / sigma_x
}
