rnorm2 <- function(uniform_sample_size=12) {
  uniform_sample <- runif(n=uniform_sample_size)

  x <- sum(uniform_sample)
  mu_x <- uniform_sample_size/2
  sigma_x <- sqrt(uniform_sample_size * (1/12))

  (x - mu_x) / sigma_x
}
