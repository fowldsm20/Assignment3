#' Height against gender t test
#'
#' @description This function performs a t test of height against gender on a dataset
#'
#' @param data This is the input data to perform the test on
#'
#' @return List with a summary of the t test and a histogram
#' @export
#'
#' @examples
#' hg_test(project)
hg_test <- function(data) {
  height <- gender <- NULL
  list(
    summary = stats::t.test(height~gender, var.equal = TRUE, data = data),
    graph = ggplot2::ggplot(data = data, ggplot2::aes(x = height, fill = gender)) +
      ggplot2::geom_histogram(alpha = 0.8,position = "identity", binwidth = 1.5)
  )
}
