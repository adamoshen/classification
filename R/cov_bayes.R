cov_bayes <- function(x, cov_x, cov_prior) {
  n <- ncol(x)

  (1/n) * cov_prior %*% solve(cov_prior + (1/n)*cov_x) %*% cov_x
}
