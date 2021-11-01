#' Height to weight linear regression test
#'
#' @description This function performs a linear regression test of height against weight on a dataset
#'
#' @param data This is the input data to perform the test on
#'
#' @return List with a summary of the linear regression test and a graph
#' @export
#'
#' @examples
#' hw_test(project)
hw_test <- function(data) {
  weight <- height <- NULL
  lm <- stats::lm(height~weight, data)
  list(
    summary = summary(lm),
    graph = ggplot2::ggplot(data, mapping = ggplot2::aes(x = weight, y = height)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm")
  )
}
