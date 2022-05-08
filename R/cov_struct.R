cov_struct1 <- function(a, b, c, alpha, beta) {
  matrix(
    c(
      a^2, alpha*a*b, beta*a*c,
      alpha*a*b, b^2, alpha*b*c,
      beta*a*c, alpha*b*c, c^2
    ),
    nrow=3, ncol=3, byrow=TRUE
  )
}

cov_struct2 <- function(a, b, c, alpha, beta) {
  matrix(
    c(
      c^2, beta*b*c, alpha*a*c,
      beta*b*c, b^2, alpha*a*b,
      alpha*a*c, alpha*a*b, a^2
    ),
    nrow=3, ncol=3, byrow=TRUE
  )
}
