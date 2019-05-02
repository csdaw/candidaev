#' Title
#'
#' Description.
#'
#' @param data Description.
#' @param logic Description.
#' @param op Description.
#' @param pat1 Description.
#' @param val1 Description.
#' @param pat2 Description.
#' @param val2 Description.
#'
#' @return Description.
#'
#' @examples
#' # example
#'
#' @export
filter_zero2 <- function(data, logic = c("and", "or"), op = c("==", "<=", ">="),
                         pat1, val1, pat2, val2) {
  # refer to operator by name
  op <- as.name(op)

  # get sum of values in first group of columns
  sum1 <- apply(data[, c(grep(pat1, colnames(data)))], 1, function(x) sum(x == 0))

  # get sum of values in second group of columns
  sum2 <- apply(data[, c(grep(pat2, colnames(data)))], 1, function(x) sum (x == 0))

  # subset data based on sum of values in two column groups
  if(logic == "and") {
    subset(data, sapply(sum1, op, val1) & sapply(sum2, op, val2))
  } else if (logic == "or") {
    subset(data, sapply(sum1, op, val1) | sapply(sum2, op, val2))
  }
}
