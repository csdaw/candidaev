#' Title
#'
#' @param data Description
#'
#' @return Returns x
#'
#' @export
#'
#' @examples
#' # example
#'
summarise_lfq <- function(data) {
  # convert data to tibble and apply summarise_na to each column
  sum_list <- data %>%
    tibble::as_tibble() %>%
    lapply(., summarise_na)

  # bind the summary for each column together in a tibble
  result <- tibble::as_tibble(do.call(rbind, sum_list), rownames = "label")

  # unnest the tibble columns
  tidyr::unnest(result)
}
