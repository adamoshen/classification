k_nearest <- function(x, train, truth, k) {
  n <- nrow(train)

  tmp <- train %>%
    select(!where(is.numeric))

  train <- train %>%
    select(where(is.numeric)) %>%
    as.matrix()

  x <- matrix(x, nrow=1, ncol=length(x))
  x <- matrix(rep(1, n), nrow=n, ncol=1) %*% x

  apply(train - x, MARGIN=1, FUN=norm, type="2") %>%
    as_tibble_col(column_name = "distance") %>%
    bind_cols(tmp) %>%
    slice_min(order_by=distance, n=k) %>%
    count({{ truth }}) %>%
    slice_max(order_by=n, n=1, with_ties=FALSE) %>%
    pull({{ truth }})
}

predict.knn <- function(data, newdata, truth, k=1) {
  tmp <- newdata %>%
    select(!where(is.numeric))

  newdata %>%
    select(where(is.numeric)) %>%
    rowwise() %>%
    mutate(.pred = k_nearest(c_across(everything()), data, {{ truth }}, k)) %>%
    ungroup() %>%
    bind_cols(tmp) %>%
    relocate(.pred, .after=everything())
}
