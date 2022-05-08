cov_mle <- function(x) {
  n <- ncol(x)

  mean_x <- mean_mle(x) %*% matrix(rep(1, n), nrow=1, ncol=n)

  (1/n) * tcrossprod(x - mean_x)
}
