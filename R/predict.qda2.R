predict.qda2 <- function(newdata, ..., mean1, mean2, cov1, cov2,
                         prior1, prior2) {
  cov1_inv <- solve(cov1)
  cov2_inv <- solve(cov2)

  A <- (cov2_inv - cov1_inv) / 2
  b <- (t(mean1) %*% cov1_inv) - (t(mean2) %*% cov2_inv)
  c <- log(sqrt(det(cov2) / det(cov1))) +
    log(prior1 / prior2) +
    0.5 * t(mean2) %*% cov2_inv %*% mean2 -
    0.5 * t(mean1) %*% cov1_inv %*% mean1

  newdata %>%
    rowwise() %>%
    mutate(.pred = if_else(eval_quadratic(c_across(...), A, b, c) > 0, "class1", "class2")) %>%
    ungroup()
}
