pattern_matrix <- function(x, labels) {
  second_class <- x %>%
    distinct({{ labels }}) %>%
    pull({{ labels }}) %>%
    factor() %>%
    levels() %>%
    pluck(2)

  x %>%
    mutate(
      intercept = 1,
      across(where(is.numeric), ~if_else({{ labels }} == second_class, -.x, .x))
    ) %>%
    relocate(intercept, .before=everything()) %>%
    select(where(is.numeric)) %>%
    as.matrix()
}
