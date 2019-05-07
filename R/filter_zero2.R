#' Filter rows based on 2 sets of columns which contain \code{zero} values
#'
#' @description This function uses 2 regular expressions to define 2
#' sets of columns in a data.frame/matrix of your choosing (based
#' on the column names).
#' Then it will filter out the data.frame/matrix rows that don't have a certain
#' number of \code{zero} values in  the 2 sets of columns.
#'
#' An example of the basic usage is as follows:
#'
#' \code{filter_zero2(my_df, logic = "and", op = "<=",
#'                    pat1 = "LFQ.intensity.*EV", val1 = 2,
#'                    pat2 = "LFQ.intensity.*W", val2 = 1)}
#'
#' This you would interpret as: "I want to keep the rows in \code{my_df} that
#' have \code{<= 2 zero} values in the columns whose names match the
#' regular expression \code{LFQ.intensity.*EV}, \strong{and} \code{<= 3 zero} values in
#' the columns whose names match the regular expression \code{LFQ.intensity.*W}."
#'
#' @param data data.frame or matrix: has rows you want to filter out
#'
#' @param logic logical operation: can be "and" or "or", defines whether to keep rows
#' that fit both conditions (and) or fit either conditions (or)
#'
#' @param op operator: can be "==", "<=", or ">=", defines the logic for the rows
#' you want to keep, i.e. keep rows with more than or equal to some number
#' of \code{zero} values
#'
#' @param pat1 regular expression: used for selecting first column set, e.g. "LFQ.intensity.*EV"
#'
#' @param val1 integer: defines the number (sum) of \code{zero} values for the first column set
#'
#' @param pat2 regular expression: used for selecting second column set, e.g. "LFQ.intensity.*W"
#'
#' @param val2 integer: defines the number (sum) of \code{zero} values for the second column set
#'
#' @return Returns a \code{data.frame} or \code{matrix} where the rows that
#' don't have a certain number of \code{zero} values in certain columns have
#' been removed.
#'
#' @seealso Some related \pkg{candidaev} package functions.
#'
#' \itemize{
#' \item \code{\link{filter_zero}} and \code{\link{filter_zero4}}:
#' for filtering based on the number of \code{zero} values in 1 or 4 sets of columns.
#' for filtering based on the number of NA values in 2 or 4 sets of columns.
#' \item \code{\link{filter_na}}, \code{\link{filter_na2}}, and
#' \code{\link{filter_na4}}: for filtering based on the number of \code{NA} values in
#' 1, 2, or 4 sets of columns.
#' item \code{\link{filter_val}}, \code{\link{filter_val2}}, and
#' \code{\link{filter_val4}}: for filtering based on the sum of the values in
#' 1, 2, or 4 sets of columns.
#' }
#'
#' @examples
#' ## example for a matrix input
#'
#' # define a 6x7 numeric matrix
#' my_mat <- matrix(c(0, 0, 30, 0, 25, 0,
#'                    0, NA, 31, 23, 24, 0,
#'                    0, 0, 32, 24, 23, 0,
#'                    23, 0, 29, 22, 0, 0,
#'                    24, 0, 30, 0, 0, 0,
#'                    21, 14, 31, 24, 0, 12,
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
#' # keep rows with maximum 1/3 zero values
#' in the sample a columns and 1/3 zero values
#' in the sample b columns
#' my_mat2 <- filter_zero2(my_mat,
#'                         logic = "or",
#'                         op = "<=",
#'                         pat1 = "sample.*a",
#'                         val1 = 1,
#'                         pat2 = "sample.*b",
#'                         val2 = 1)
#'
#' ## example for a data.frame input
#'
#' # define a 6x8 data.frame
#' my_df <- data.frame(sample_1a = c(0, 0, 30, 0, 25, 0),
#'                     sample_2a = c(0, NA, 31, 23, 24, 0),
#'                     sample_3a = c(0, 0, 32, 24, 23, 0),
#'                     sample_1b = c(23, 0, 29, 22, 0, 0),
#'                     sample_2b = c(24, 0, 30, 0, 0, 0),
#'                     sample_3b = c(21, 14, 31, 24, 0, 12),
#'                     total = c(432, 555, 666, 765, 890, 988),
#'                     colour = c("orange", "brown", "red",
#'                                "pink", "black", "grey"))
#'
#' # keep the row with exactly 0/3 zero values
#' # in the sample a columns and 3/3 zero values
#' # in the sample b columns
#' my_df2 <- filter_zero2(my_df,
#'                        logic = "and",
#'                        op = "==",
#'                        pat1 = "sample.*a",
#'                        val1 = 0,
#'                        pat2 = "sample.*b",
#'                        val2 = 3)
#'
#' ## note you can also use this function with
#' ## dplyr functions and magrittr pipes
#' ## (and other tidyverse functions probably)
#'
#' # drop the 'total' column
#' # keep rows with at least 2/3 zero values in the
#' # sample a columns or 2/3 zero values in the
#' # sample b columns, then pull the 'colour'
#' # column as a character vector
#' my_colour <- my_df %>%
#'   select(-total) %>%
#'   filter_zero2(.,
#'                logic = "or",
#'                op = ">=",
#'                pat1 = "sample.*a",
#'                val1 = 2,
#'                pat2 = "sample.*b",
#'                val2 = 2) %>%
#'   pull(colour)
#'
#' @export
filter_zero2 <- function(data, logic = c("and", "or"), op = c("==", "<=", ">="),
                         pat1, val1, pat2, val2) {
  # refer to operator by name
  op <- as.name(op)

  # get sum of values in first group of columns
  sum1 <- apply(data[, c(grep(pat1, colnames(data)))], 1, function(x) sum(x == 0, na.rm = TRUE))

  # get sum of values in second group of columns
  sum2 <- apply(data[, c(grep(pat2, colnames(data)))], 1, function(x) sum (x == 0, na.rm = TRUE))

  # subset data based on sum of values in two column groups
  if(logic == "and") {
    subset(data, sapply(sum1, op, val1) & sapply(sum2, op, val2))
  } else if (logic == "or") {
    subset(data, sapply(sum1, op, val1) | sapply(sum2, op, val2))
  }
}
