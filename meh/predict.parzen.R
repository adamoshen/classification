predict.parzen <- function(newdata, x1_i, x2_i, h1, h2, dim) {
  # Ensure `xi_i` and `newdata` has been reduced to a vector
  x1_i <- drop(x1_i)
  x2_i <- drop(x2_i)
  newdata <- drop(newdata)

  newdata %>%
    as_tibble_col(column_name = "x") %>%
    mutate(
      .fitted_class1 = map_dbl(x, ~uni_parzen_density(.x, x1_i, h1, length(x1_i))),
      .fitted_class2 = map_dbl(x, ~uni_parzen_density(.x, x2_i, h2, length(x2_i))),
      ".pred_x{dim}" := if_else(.fitted_class1 > .fitted_class2, "class1", "class2")
    ) %>%
    rename("x_{dim}" := x) %>%
    select(starts_with("x"), starts_with(".pred"))
}
