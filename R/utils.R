coefs <- function(x, A, b, c) {
  coef_a <- A[2, 2]
  coef_b <- 2*A[1, 2]*x + b[2]
  coef_c <- c + b[1]*x + A[1, 1]*x^2

  c(coef_c, coef_b, coef_a)
}

eval_quadratic <- function(x, A, b, c) {
  x <- as.matrix(x)

  drop(t(x) %*% A %*% x + b %*% x + c)
}

gsolve <- function(x) {
  solve(t(x) %*% x) %*% t(x)
}

left_mult <- function(x, a) {
  x <- as.matrix(x)

  drop(t(a) %*% x)
}

utils::globalVariables("where")
