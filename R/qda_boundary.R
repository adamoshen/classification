qda_boundary <- function(mean1, mean2, cov1, cov2, prior1, prior2, dims, from, to, by) {
  dims <- sort(dims)
  missing_dims <- setdiff(1:nrow(mean1), dims)
  cov1_inv <- solve(cov1)
  cov2_inv <- solve(cov2)

  # Calculate quadratic coefficients
  A <- (cov2_inv - cov1_inv) / 2
  b <- (t(mean1) %*% cov1_inv) - (t(mean2) %*% cov2_inv)
  c <- log(sqrt(det(cov2) / det(cov1))) +
    log(prior1 / prior2) +
    0.5 * t(mean2) %*% cov2_inv %*% mean2 -
    0.5 * t(mean1) %*% cov1_inv %*% mean1

  # Remove unused dimensions (if any)
  if (length(missing_dims) != 0) {
    A <- A[-missing_dims, -missing_dims]
    b <- b[ , -missing_dims]
  }

  # Drop `b` and `c` from matrices to vectors
  b <- drop(b)
  c <- drop(c)

  # Find roots
  vals <- seq(from=from, to=to, by=by)

  vals %>%
    set_names(., nm=.) %>%
    map(~coefs(.x, A, b, c)) %>%
    map(polyroot) %>%
    map_dfr(as_tibble_col, column_name="root", .id=paste0("x", dims[1])) %>%
    mutate(
      across(contains("x"), as.numeric),
      root = zapsmall(root)
    ) %>%
    filter(Im(root) == 0) %>%
    mutate(root = Re(root)) %>%
    relocate(root, .after=everything())
}
