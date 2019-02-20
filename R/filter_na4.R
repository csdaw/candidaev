#' Title
#'
#' @param data Description
#'
#' @param logic Description
#'
#' @param op Description
#'
#' @param pat1 Description
#'
#' @param val1 Description
#'
#' @param pat2 Description
#'
#' @param val2 Description
#'
#' @param pat3 Description
#'
#' @param val3 Description
#'
#' @param pat4 Description
#'
#' @param val4 Description
#'
#' @return Description
#'
#' @examples
#' # example
#'
#' @export
#'
filter_na4 <- function(data, logic = c("and", "or"), op = c("==", ">=", "<="),
                       pat1, val1, pat2, val2, pat3, val3, pat4, val4) {
  # refer to operator by name
  op <- as.name(op)

  # get number of NA in first group of columns
  n_na1 <- apply(is.na(data[, c(grep(pat1, colnames(data)))]), 1, sum)

  # get number of NA in second group of columns
  n_na2 <- apply(is.na(data[, c(grep(pat2, colnames(data)))]), 1, sum)

  # get number of NA in third group of columns
  n_na3 <- apply(is.na(data[, c(grep(pat3, colnames(data)))]), 1, sum)

  # get number of NA in fourth group of columns
  n_na4 <- apply(is.na(data[, c(grep(pat4, colnames(data)))]), 1, sum)

  # subset data based on number of NA in two column groups
  if(logic == "and") {
    subset(data, sapply(n_na1, op, val1) &
             sapply(n_na2, op, val2) &
             sapply(n_na3, op, val3) &
             sapply(n_na4, op, val4))
  } else if (logic == "or") {
    subset(data, sapply(n_na1, op, val1) |
             sapply(n_na2, op, val2) |
             sapply(n_na3, op, val3) |
             sapply(n_na4, op, val4))
  }
}
