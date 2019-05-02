#' Title
#'
#' Description.
#'
#' @param data Description.
#' @param op Description.
#' @param pat Description.
#' @param val Description.
#'
#' @return Description.
#'
#' @examples
#' # example
#'
#' @export
filter_zero <- function(data, op = c("==", "<=", ">="), pat, val) {
  # refer to operator by name
  op <- as.name(op)

  # get sum of values in column group
  col_sum <- apply(data[, c(grep(pat, colnames(data)))], 1, function(x) sum(x == 0))

  # subset data based on sum of values in column group
  subset(data, sapply(col_sum, op, val))
}
