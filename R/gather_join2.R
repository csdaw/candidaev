#' Title
#'
#' Function to get a long data frame of the assay data
#' annotated with sample information.
#'
#' @param mat Description
#'
#' @return Description
#'
#' @examples
#' #example
#'
gather_join2 <- function(mat) {
  data.frame(mat) %>%
    tidyr::gather(ID, val) %>%
    left_join(., data.frame(exd), by = "ID")

  # Function to get a long data.frame of the assay data
  # annotated with sample info
}
