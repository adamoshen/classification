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
