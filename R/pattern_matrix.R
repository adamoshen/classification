#' Create pattern matrix from two-class data
#'
#' Suppose we have a two-class data matrix \Sexpr[results=rd, stage=build]{
#' katex::math_to_rd("\\\mathbf{X}", displayMode=FALSE)
#' }, of size \eqn{n} by \eqn{d}, where \eqn{n} is the number of observations, and \eqn{d} is the
#' number of dimensions (features). Without loss of generality, if we assume that the first \eqn{n1}
#' observations belong to the first class and the second \eqn{n2} observations belong to the second
#' class, then the pattern matrix \Sexpr[results=rd, stage=build]{
#' katex::math_to_rd("\\\mathbf{Y}", displayMode=FALSE, include_css=FALSE)
#' } can be represented as:
#' \Sexpr[results=rd, stage=build]{
#' katex::math_to_rd("
#' \\\mathbf{Y} \\\,=\\\, \\\begin{bmatrix*}[r]
#' \\\mathbf{1}_{1} &\\\mathbf{X}_{1}\\\\\\\\
#' -\\\mathbf{1}_{2} &-\\\mathbf{X}_{2}
#' \\\end{bmatrix*},", include_css=FALSE)
#' }
#' where \Sexpr[results=rd, stage=build]{
#' katex::math_to_rd("\\\mathbf{1}_{i}", displayMode=FALSE, include_css=FALSE)
#' } is a column vector of \Sexpr[results=rd, stage=build]{
#' katex::math_to_rd("n_{i}", displayMode=FALSE, include_css=FALSE)
#' } ones and \Sexpr[results=rd, stage=build]{
#' katex::math_to_rd("\\\mathbf{X}_{i}", displayMode=FALSE, include_css=FALSE)
#' } is a \Sexpr[results=rd, stage=build]{
#' katex::math_to_rd("n_{i} \\\times d", displayMode=FALSE, include_css=FALSE)
#' } matrix whose rows are observations of class \eqn{i}.
#' @param x A data frame of dimension \eqn{n} by \eqn{d}, where \eqn{n} is the number of rows and
#' \eqn{d} is the number of dimensions (features).
#' @param labels The name of the column containing the true class labels. Can be supplied as a
#' string or symbol.
#' @export
#' @references Duda, R. O., Hart, P. E., Stork, D. G. (2001) Pattern Classification. Second edition.
#' Wiley. Section 5.8.2: Relation to Fisher's Linear Discriminant.
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
