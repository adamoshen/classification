mean_mle <- function(x) {
  n <- ncol(x)

  (1/n) * as.matrix(rowSums(x))
}
