hokashyap_boundary <- function(data, truth, eta=0.9, tol=1e-3, maxiter=500) {
  Y <- pattern_matrix(data, {{ truth }})

  b <- matrix(rep(1, nrow(Y)))
  a <- gsolve(Y) %*% b
  k <- 1

  while (k < maxiter) {
    e <- Y %*% a - b
    e_plus <- (1/2) * (e + abs(e))
    b <- b + 2 * eta * e_plus
    a <- gsolve(Y) %*% b

    if (min(abs(e)) < tol) {
      # Not returning `b` because it is clutter
      return(list(a=a, k=k))
    }

    k <- k + 1
  }
  "Maximum iterations reached. Consider increasing `maxiter`."
}

predict.hokashyap <- function(hokashyap_boundary, newdata) {
  tmp <- newdata %>%
    select(!where(is.numeric))

  a <- hokashyap_boundary$a

  newdata %>%
    select(where(is.numeric)) %>%
    mutate(intercept = 1) %>%
    relocate(intercept, .before=everything()) %>%
    rowwise() %>%
    mutate(.value = left_mult(c_across(everything()), a)) %>%
    ungroup() %>%
    mutate(.pred = if_else(.value >= 0, "class1", "class2")) %>%
    bind_cols(tmp) %>%
    select(-.value) %>%
    relocate(.pred, .after=everything())
}
