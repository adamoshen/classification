#' Sample from the (approximate) multivariate normal distribution
#'
#' @details Consider a random sample of size 12 of Uniform(0,1) random variables. Let \eqn{X} be the
#' sum of the sample elements. Then \eqn{X} has an approximate \eqn{N(0,1)} distribution.
#'
#' Let \eqn{\mathbf{Z}} be a \eqn{d \times n} matrix with distribution
#' \eqn{N_{d}(\mathbf{0},\,\mathbb{I})}. Let \eqn{\mathbf{X}} be a \eqn{d \times n} random matrix
#' with distribution \eqn{N_{d}(\bm{\mu},\,\mathbf{\Sigma})}. If \eqn{\mathbf{P}} and
#' \eqn{\mathbf{D}} are the eigenvector and eigenvalue matrices, respectively, then
#'
#' \deqn{\mathbf{X} \,=\, \mathbf{PD}^{\frac{1}{2}}\mathbf{Z} \,+\, \bm{\mu}\mathbf{1}'
#' \quad\sim\quad N_{d}(\bm{\mu},\, \mathbf{\Sigma}),}
#'
#' where \eqn{\mathbf{1}} is a column vector of ones.
#'
#' @param n The sample size.
#' @param mean A \eqn{d \times 1} matrix containing the means of the variables.
#' @param cov A \eqn{d \times d} positive-definite, symmetric matrix specifying the covariance
#' matrix of the variables.
#' @return A matrix with `d` rows and `n` columns.
#' @export
#' @examples
#' mu <- matrix(c(1, 3))
#' sigma <- matrix(c(3, 1, 1, 2), 2, 2)
#' rmvtnorm(n = 5, mu, sigma)
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
