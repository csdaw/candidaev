#' Filter rows based on the sums of values across 2 sets of columns
#'
#' @description This function uses 2 regular expressions to define 2
#' sets of columns in a data.frame/matrix of your choosing (based on
#' the columns names).
#' Then it will filter out the data.frame/matrix rows that have sums
#' across the 2 selected columns sets that do not equal 2 specific
#' values or thresholds.
#'
#' An example of the basic usage is as follows:
#'
#' \code{filter_val2(my_df, logic = "and", op = ">=",
#'                   pat1 = "Unique.peptides.*EV", val1 = 5,
#'                   pat2 = "Unique.peptides.*W", val2 = 4)}
#'
#' This you would interpret as: "I want to keep the rows in my \code{df}
#' that have a sum \code{>= 5} when you add up the values in the columns
#' whose names match the regular expression \code{Unique.peptides.*EV}
#' \strong{and} that have a sum \code{>= 4} when you add up the values in
#' the columns whose names match the regular expression
#' \code{Unique.peptides.*W}."
#'
#' @param data data.frame or matrix: has rows you want to filter out
#'
#' @param logic logical operation: can be "and" or "or", defines whether to keep rows
#' that fit both conditions (and) or fit either conditions (or)
#'
#' @param op operator: can be "==", "<=", or ">=", defines the logic for the rows
#' you want to keep, i.e. keep rows with a sum of more than or equal to some value
#'
#' @param pat1 regular expression: used for selecting columns, e.g. "Unique.peptides.*EV"
#'
#' @param val1 integer: defines the sum of the values from the first column set
#'
#' @param pat2 regular expression: used for selecting columns, e.g. "Unique.peptides.*W"
#'
#' @param val2 integer: defines the sum of the values from the second column set
#'
#' @return Returns a \code{data.frame} or \code{matrix} where the rows that
#' don't add up to a certain value in certain columns have
#' been removed.
#'
#' @seealso Some related \pkg{candidaev} package functions.
#'
#' \itemize{
#' \item \code{\link{filter_val}} and \code{\link{filter_val4}}:
#' for filtering based on the sum of the values in 1 or 4 sets of columns
#' \item \code{\link{filter_zero}}, \code{\link{filter_zero2}}, and
#' \code{\link{filter_zero4}}: for filtering based on the number of \code{zero}
#' values in 1, 2, or 4 sets of columns.
#' item \code{\link{filter_na}}, \code{\link{filter_na2}}, and
#' \code{\link{filter_na4}}: for filtering based on the number of \code{NA} values in
#' 1, 2 or 4 sets of columns.
#' }
#'
#' @examples
#' ## example for a matrix input
#'
#' # define a 6x7 numeric matrix
#' my_mat <- matrix(c(NA, NA, 30, NA, 25, NA,
#'                    NA, 15, 31, 23, 24, NA,
#'                    NA, NA, 32, 24, 23, NA,
#'                    23, NA, 29, 22, NA, NA,
#'                    24, NA, 30, NA, NA, NA,
#'                    21, 14, 31, 24, NA, 12,
#'                    432, 555, 666, 765, 890, 988),
#'                  nrow = 6,
#'                  ncol = 7)
#' colnames(my_mat) <- c("sample_1a",
#'                       "sample_2a",
#'                       "sample_3a",
#'                       "sample_1b",
#'                       "sample_2b",
#'                       "sample_3b",
#'                       "total")
#'
#' # keep rows where the sample a columns
#' # add up to at least 20 and the sample b columns
#' # add up to at least 20
#' my_mat2 <- filter_val2(my_mat,
#'                        logic = "and",
#'                        op = ">=",
#'                        pat1 = "sample.*a",
#'                        val1 = 20,
#'                        pat2 = "sample.*b",
#'                        val2 = 20)
#'
#' ## example for a data.frame input
#'
#' # define a 6x8 data.frame
#' my_df <- data.frame(sample_1a = c(NA, NA, 30, NA, 25, NA),
#'                     sample_2a = c(NA, 15, 31, 23, 24, NA),
#'                     sample_3a = c(NA, NA, 32, 24, 23, NA),
#'                     sample_1b = c(23, NA, 29, 22, NA, NA),
#'                     sample_2b = c(24, NA, 30, NA, NA, NA),
#'                     sample_3b = c(21, 14, 31, 24, NA, 12),
#'                     total = c(432, 555, 666, 765, 890, 988),
#'                     colour = c("orange", "brown", "red",
#'                                "pink", "black", "grey"))
#'
#' # keep the row where the sample a columns add up
#' # to exactly 47 or the sample b columns add up
#' # to exactly 12
#' my_df2 <- filter_val2(my_df,
#'                       logic = "or",
#'                       op = "==",
#'                       pat1 = "sample.*a",
#'                       val1 = 47,
#'                       pat2 = "sample.*b",
#'                       val2 = 12)
#'
#' ## note you can also use this function with
#' ## dplyr functions and magrittr pipes
#' ## (and other tidyverse functions probably)
#' # load dplyr
#' library(dplyr)
#'
#' # drop the 'total' column
#' # keep rows where the sample a columns add up
#' # to at least 1 and the sample b columns add
#' # up to at least 1, then pull the 'colour'
#' # column as a character vector
#' my_colour <- my_df %>%
#'   select(-total) %>%
#'   filter_val2(.,
#'               logic = "and",
#'               op = ">=",
#'               pat1 = "sample.*a",
#'               val1 = 1,
#'               pat2 = "sample.*b",
#'               val2 = 1) %>%
#'   pull(colour)
#'
#'
#' @export
filter_val2 <- function(data, logic = c("and", "or"), op = c("==", "<=", ">="),
                        pat1, val1, pat2, val2) {
  # refer to operator by name
  op <- as.name(op)

  # get sum of values in first group of columns
  sum1 <- apply(data[, c(grep(pat1, colnames(data)))], 1, function(x) sum(x, na.rm = TRUE))

  # get sum of values in second group of columns
  sum2 <- apply(data[, c(grep(pat2, colnames(data)))], 1, function(x) sum(x, na.rm = TRUE))

  # subset data based on sum of values in two column groups
  if(logic == "and") {
    subset(data, sapply(sum1, op, val1) & sapply(sum2, op, val2))
  } else if (logic == "or") {
    subset(data, sapply(sum1, op, val1) | sapply(sum2, op, val2))
  }
}
