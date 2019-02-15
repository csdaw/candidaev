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
filter_na2 <- function(data, logic = c("and", "or"), op = c("==", ">=", "<="),
                       pat1, val1, pat2, val2) {
  # refer to operator by name
  op <- as.name(op)

  # get number of NA in first group of columns
  n_na1 <- apply(is.na(data[, c(grep(pat1, colnames(data)))]), 1, sum)

  # get number of NA in second group of columns
  n_na2 <- apply(is.na(data[, c(grep(pat2, colnames(data)))]), 1, sum)

  # subset data based on number of NA in two column groups
  if(logic == "and") {
    subset(data, sapply(n_na1, op, val1) & sapply(n_na2, op, val2))
  } else if (logic == "or") {
    subset(data, sapply(n_na1, op, val1) | sapply(n_na2, op, val2))
  }
}
