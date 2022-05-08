simultdiag <- function(data1, data2, mean1, mean2, cov1, cov2, verbose=TRUE) {
  d <- nrow(cov1)

  # Step 1: Y = t(P1) %*% X
  P1 <- eigen(cov1)$vectors
  D <- diag(eigen(cov1)$values)
  data1 <- t(P1) %*% data1
  data2 <- t(P1) %*% data2
  mean1 <- t(P1) %*% mean1
  mean2 <- t(P1) %*% mean2
  cov1 <- t(P1) %*% cov1 %*% P1
  cov2 <- t(P1) %*% cov2 %*% P1

  if (verbose) {
    cat("\nStep 1\n")
    cat("Left-multiplying matrix: P1'\n")
    print(t(P1))
    cat("Covariance matrix of class 1:\n")
    print(zapsmall(cov1))
    cat("Covariance matrix of class 2:\n")
    print(zapsmall(cov2))
  }

  # Step 2: Z = D^(-1/2) %*% Y
  data1 <- solve(sqrt(D)) %*% data1
  data2 <- solve(sqrt(D)) %*% data2
  mean1 <- solve(sqrt(D)) %*% mean1
  mean2 <- solve(sqrt(D)) %*% mean2
  cov1 <- solve(sqrt(D)) %*% cov1 %*% solve(sqrt(D))
  cov2 <- solve(sqrt(D)) %*% cov2 %*% solve(sqrt(D))

  if (verbose) {
    cat("\nStep 2\n")
    cat("Left-multiplying matrix: D^(-1/2)\n")
    print(solve(sqrt(D)))
    cat("Covariance matrix of class 1:\n")
    print(zapsmall(cov1))
    cat("Covariance matrix of class 2:\n")
    print(zapsmall(cov2))
  }

  # Step 3: A = t(P2) %*% Z
  P2 <- eigen(cov2)$vectors
  data1 <- t(P2) %*% data1
  data2 <- t(P2) %*% data2
  mean1 <- t(P2) %*% mean1
  mean2 <- t(P2) %*% mean2
  cov1 <- t(P2) %*% cov1 %*% P2
  cov2 <- t(P2) %*% cov2 %*% P2

  if (verbose) {
    cat("\nStep 3\n")
    cat("Left-multiplying matrix: P2'\n")
    print(t(P2))
    cat("Covariance matrix of class 1:\n")
    print(zapsmall(cov1))
    cat("Covariance matrix of class 2:\n")
    print(zapsmall(cov2))
  }

  rownames(data1) <- paste0("x", 1:d)
  rownames(data2) <- paste0("x", 1:d)

  invisible(
    list(
      class1_matrix = data1, class2_matrix = data2,
      mean1 = mean1, mean2 = mean2,
      cov1 = zapsmall(cov1), cov2 = zapsmall(cov2)
    )
  )
}
