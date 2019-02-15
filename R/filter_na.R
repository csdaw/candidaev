#' Title
#'
#' @param data
#' @param op
#' @param pat
#' @param val
#'
#' @return
#' @export
#'
#' @examples
filter_na <- function(data, op = c("==", "<=", ">="), pat, val) {
  # refer to operator by name
  op <- as.name(op)

  # get number of NA in column group
  n_na <- apply(is.na(data[, c(grep(pat, colnames(data)))]), 1, sum)

  # subset data based on number of NA in column group
  subset(data, sapply(n_na, op, val))
}
