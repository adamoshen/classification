uni_parzen <- function(range_x, x_i, h) {
  # Ensure x_i has been reduced to a vector
  x_i <- drop(x_i)
  n <- length(x_i)

  # Assume equal bin widths
  bin_width <- abs(range_x[2] - range_x[1])

  out_data <- range_x %>%
    as_tibble_col(column_name="x") %>%
    mutate(
      .fitted = map_dbl(x, ~uni_parzen_density(.x, x_i, h, n)),
      bin_width = bin_width
    )

  mean_parzen <- out_data %>%
    summarise(mean_parzen = sum(x * .fitted * bin_width)) %>%
    pull(mean_parzen)

  var_parzen <- out_data %>%
    summarise(var_parzen = sum((x - mean_parzen)^2 * .fitted * bin_width)) %>%
    pull(var_parzen)

  out_data <- out_data %>%
    select(-bin_width)

  list(out_data=out_data, mean_parzen=mean_parzen, var_parzen=var_parzen)
}
