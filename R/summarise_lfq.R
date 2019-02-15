#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
summarise_lfq <- function(data) {
  # convert data to tibble and apply summarise_na to each column
  sum_list <- data %>%
    as_tibble() %>%
    lapply(., summarise_na)

  # bind the summary for each column together in a tibble
  result <- as_tibble(do.call(rbind, sum_list), rownames = "label")

  # unnest the tibble columns
  tidyr::unnest(result)
}
