uni_parzen_density <- function(range_x, x_i, h, n) {
  (1/n) * (1/(sqrt(2*pi)*h)) * sum(exp((-1/2) * ((range_x - x_i) / h)^2))
}
