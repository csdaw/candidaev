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
#' @param pat3 Description.
#' @param val3 Description.
#' @param pat4 Description.
#' @param val4 Description.
#'
#' @return Description.
#'
#' @examples
#' # example
#'
#' @export
filter_val4 <- function(data, logic = c("and", "or"), op = c("==", ">=", "<="),
                        pat1, val1, pat2, val2, pat3, val3, pat4, val4) {
  # refer to operator by name
  op <- as.name(op)

  # get sum of values in first group of columns
  sum1 <- apply(data[, c(grep(pat1, colnames(data)))], 1, sum)

  # get sum of values in second group of columns
  sum2 <- apply(data[, c(grep(pat2, colnames(data)))], 1, sum)

  # get sum of values in first group of columns
  sum3 <- apply(data[, c(grep(pat3, colnames(data)))], 1, sum)

  # get sum of values in second group of columns
  sum4 <- apply(data[, c(grep(pat4, colnames(data)))], 1, sum)

  # subset data based on sum of values in two column groups
  if(logic == "and") {
    subset(data,
           sapply(sum1, op, val1) &
             sapply(sum2, op, val2) &
             sapply(sum3, op, val3) &
             sapply(sum4, op, val4))
  } else if (logic == "or") {
    subset(data, sapply(sum1, op, val1) |
             sapply(sum2, op, val2) |
             sapply(sum3, op, val3) |
             sapply(sum4, op, val4))
  }
}
