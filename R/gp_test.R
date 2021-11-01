#' Gender against physical activity chi squared test
#'
#' @description This function performs a chi squared test of gender against physical activity on a dataset
#'
#' @param data This is the input data to perform the test on
#'
#' @return List with a summary of the chi squared test and a table
#' @export
#'
#' @examples
#' gp_test(project)
gp_test <- function(data) {
  gender <- phys <- n <- Male <- Female <- NULL
  data1 <- data %>%
    dplyr::count(gender, phys) %>%
    tidyr::pivot_wider(names_from = gender, values_from = n)
  list(
    summary = stats::chisq.test(
      dplyr::summarise(data1, Male, Female)),
    table = data1 %>%
      as.data.frame() %>%
      tibble::column_to_rownames(var = "phys") %>%
      kableExtra::kbl()
  )
}
