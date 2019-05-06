#' Filter rows based on 4 sets of columns which contain \code{NA} values
#'
#' @description This function uses 4 regular expressions to define 4
#' sets of columns in a data.frame/matrix of your choosing (based
#' on the column names).
#' Then it will filter out the data.frame/matrix rows that don't have a certain
#' number of \code{NA} values in  the 4 sets of columns.
#'
#' An example of the basic usage is as follows:
#'
#' \code{filter_na4(my_df, logic = "and", op = "<=",
#'                  pat1 = "LFQ.intensity.*A1_EV", val1 = 1,
#'                  pat2 = "LFQ.intensity.*A1_W", val2 = 2,
#'                  pat3 = "LFQ.intensity.*A9_EV", val3 = 1,
#'                  pat4 = "LFQ.intensity.*A9_W", val4 = 2)}
#'
#' This you would interpret as: "I want to keep the rows in \code{my_df} that
#' have \code{<= 1 NA} values in the columns whose names match the
#' regular expressions \code{LFQ.intensity.*A1_EV} \strong{and}
#' \code{LFQ.intensity.*A9_EV},
#' \strong{and} \code{<= 2 NA} values in
#' the columns whose names match the regular expressions \code{LFQ.intensity.*A1_W}
#' \strong{and} \code{LFQ.intensity.*A9_W}."
#'
#' @param data data.frame or matrix: has rows you want to filter out
#'
#' @param logic logical operation: can be "and" or "or", defines whether to keep rows
#' that fit all conditions (and) or fit any of the 4 conditions (or)
#'
#' @param op operator: can be "==", "<=", or ">=", defines the logic for the rows
#' you want to keep, i.e. keep rows with more than or equal to some number
#' of \code{NA} values
#'
#' @param pat1 regular expression: used for selecting first column set, e.g. "LFQ.intensity.*A1_EV"
#'
#' @param val1 integer: defines the number (sum) of \code{NA} values for the first column set
#'
#' @param pat2 regular expression: used for selecting second column set, e.g. "LFQ.intensity.*A1_W"
#'
#' @param val2 integer: defines the number (sum) of \code{NA} values for the second column set
#'
#' @param pat3 regular expression: used for selecting third column set, e.g. "LFQ.intensity.*A9_EV"
#'
#' @param val3 integer: defines the number (sum) of \code{NA} values for the third column set
#'
#' @param pat4 regular expression: used for selecting fourth column set, e.g. "LFQ.intensity.*A9_W"
#'
#' @param val4 integer: defines the number (sum) of \code{NA} values for the fourth column set
#'
#' @return Returns a \code{data.frame} or \code{matrix} where the rows that
#' don't have a certain number of \code{NA} values in certain columns have
#' been removed.
#'
#' @seealso Some related \pkg{candidaev} package functions.
#'
#' \itemize{
#' \item \code{\link{filter_na}} and \code{\link{filter_na2}}:
#' for filtering based on the number of \code{NA} values in 1 or 2 sets of columns.
#' \item \code{\link{filter_zero}}, \code{\link{filter_zero2}}, and
#' \code{\link{filter_zero4}}: for filtering based on the number of \code{zero}
#' values in 1, 2, or 4 sets of columns.
#' item \code{\link{filter_val}}, \code{\link{filter_val2}}, and
#' \code{\link{filter_val4}}: for filtering based on the sum of the values in
#' 1, 2, or 4 sets of columns.
#' }
#'
#' @examples
#' ## example for a matrix input
#'
#' # define a 6x13 numeric matrix
#' my_mat <- matrix(c(NA, NA, 34, 22, 26, NA,
#'                    NA, 12, 35, 22, 27, NA,
#'                    NA, NA, 34, NA, 28, NA,
#'                    NA, NA, 32, 20, 20, 25,
#'                    NA, NA, 35, NA, NA, 26,
#'                    NA, NA, 30, 21, NA, 27,
#'                    NA, NA, 30, NA, 25, NA,
#'                    NA, 15, 31, 23, 24, NA,
#'                    NA, NA, 32, 24, 23, NA,
#'                    23, NA, 29, 22, NA, NA,
#'                    24, NA, 30, NA, NA, NA,
#'                    21, 14, 31, 24, NA, 12,
#'                    432, 555, 666, 765, 890, 988),
#'                  nrow = 6,
#'                  ncol = 13)
#' colnames(my_mat) <- c(paste0("sample_", seq(1, 3), "a"),
#'                       paste0("sample_", seq(1, 3), "b"),
#'                       paste0("sample_", seq(1, 3), "c"),
#'                       paste0("sample_", seq(1, 3), "d"),
#'                       "total")
#'
#' # keep rows with 0/3 NA values in at least
#' # one of the 4 column sets
#' my_mat2 <- filter_na4(my_mat,
#'                       logic = "or",
#'                       op = "==",
#'                       pat1 = "sample.*a", val1 = 0,
#'                       pat2 = "sample.*b", val2 = 0,
#'                       pat3 = "sample.*c", val3 = 0,
#'                       pat4 = "sample.*d", val4 = 0)
#'
#' ## example for a data.frame input
#'
#' # define a 6x14 data.frame
#' my_df <- my_mat %>%
#'   as.data.frame() %>%
#'   mutate(colour = c("green", "blue", "orange",
#'                     "grey", "magenta", "cyan"))
#'
#' # keep row with exactly 0/3 NA values in sample d columns
#' # and 3/3 NA values in the other sample column sets
#' my_df2 <- filter_na4(my_df, logic = "and", op = "==",
#'                      pat1 = "sample.*a", val1 = 3,
#'                      pat2 = "sample.*b", val2 = 3,
#'                      pat3 = "sample.*c", val3 = 3,
#'                      pat4 = "sample.*d", val4 = 0)
#'
#' ## note you can also use this function with
#' ## dplyr functions and magrittr pipes
#' ## (and other tidyverse functions probably)
#'
#' # drop the 'total' column
#' # keep rows with 0/3 NA values in at least
#' # one of the 4 column sets, then pull the 'colour'
#' # column as a character vector
#' my_colour <- my_df %>%
#'   select(-total) %>%
#'   filter_na4(., logic = "or", op = "==",
#'              pat1 = "sample.*a", val1 = 0,
#'              pat2 = "sample.*b", val2 = 0,
#'              pat3 = "sample.*c", val3 = 0,
#'              pat4 = "sample.*d", val4 = 0) %>%
#'   pull(colour)
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
