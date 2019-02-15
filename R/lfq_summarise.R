#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
lfq_summarise <- function(data) {
  # convert data to data frame
  df <- as.data.frame(data, stringsAsFactors = FALSE)

  # apply internal lfq_summary function to each column
  sum_list <- lapply(df, na_summarise)

  # bind the lfq_summary for each column together
  result <- as.data.frame(do.call(rbind, sum_list))
  return(result)
}
