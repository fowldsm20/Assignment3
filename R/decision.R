#' Make decision
#'
#' @description This function gives a decision as to whether there is enough evidence to reject the Null Hypothesis
#'
#' @param p_value This is the p-value of the statistical test
#'
#' @return Decision in words as to whether the Null Hypothesis can be rejected
#' @export
#'
#' @examples
#' decision(p_value = 0.03)
decision <- function(p_value) {
  if (p_value < 0.05) {
    glue::glue("From the statistical analysis, the p value was found to be {100*p_value}%. Since this is less than 5%, there is sufficient evidence to reject the Null Hypothesis.")
  } else {
    glue::glue("From the statistical analysis, the p value was found to be {100*p_value}%. Since this is greater than 5%, there is insufficient evidence to reject the Null Hypothesis.")
  }
}
