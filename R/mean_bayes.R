mean_bayes <- function(x, cov_x, mean_prior, cov_prior) {
  n <- ncol(x)

  cov_prior %*% solve((cov_prior + (1/n)*cov_x)) %*% mean_mle(x) +
    (1/n) * cov_x %*% solve(cov_prior + (1/n)*cov_x) %*% mean_prior
}
