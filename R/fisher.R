fisher_projection <- function(data, labels) {
  sample_sizes <- data %>%
    count({{ labels }}) %>%
    pull(n)

  Y <- pattern_matrix(data, {{ labels }})
  b <- matrix(c(
    rep(sum(sample_sizes) / sample_sizes[1], sample_sizes[1]),
    rep(sum(sample_sizes) / sample_sizes[2], sample_sizes[2])),
    nrow=sum(sample_sizes), ncol=1
  )

  a <- gsolve(Y) %*% b
  a <- drop(a)

  list(
    threshold = set_names(a[1], "threshold"),
    coefs = set_names(a[-1], colnames(Y)[-1])
  )
}

predict.fisher <- function(fisher_projection, newdata) {
  tmp <- newdata %>%
    select(!where(is.numeric))

  w <- fisher_projection %>%
    pluck("coefs") %>%
    as.matrix()
  w0 <- fisher_projection %>%
    pluck("threshold")

  newdata %>%
    select(where(is.numeric)) %>%
    rowwise() %>%
    mutate(.value = left_mult(c_across(everything()), w) + w0) %>%
    ungroup() %>%
    mutate(.pred = if_else(.value >= 0, "class1", "class2")) %>%
    bind_cols(tmp) %>%
    select(-.value) %>%
    relocate(.pred, .after=everything())
}
