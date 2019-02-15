#' Title
#'
#' @param data
#' @param logic
#' @param op
#' @param pat1
#' @param val1
#' @param pat2
#' @param val2
#'
#' @return
#' @export
#'
#' @examples
filter_val2 <- function(data, logic = c("and", "or"), op = c("==", "<=", ">="),
                        pat1, val1, pat2, val2) {
  # refer to operator by name
  op <- as.name(op)

  # get sum of values in first group of columns
  sum1 <- apply(data[, c(grep(pat1, colnames(data)))], 1, sum)

  # get sum of values in second group of columns
  sum2 <- apply(data[, c(grep(pat2, colnames(data)))], 1, sum)

  # subset data based on sum of values in two column groups
  if(logic == "and") {
    subset(data, sapply(sum1, op, val1) & sapply(sum2, op, val2))
  } else if (logic == "or") {
    subset(data, sapply(sum1, op, val1) | sapply(sum2, op, val2))
  }
}
