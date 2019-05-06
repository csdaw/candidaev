#' Filter rows based on 1 set of columns which contain \code{zero} values
#'
#' @description This function uses a regular expression to define 1
#' set of columns in a data.frame/matrix of your choosing (based
#' on the column names).
#' Then it will filter out the data.frame/matrix rows that don't have a certain
#' number of \code{zero} values in  the set of columns.
#'
#' An example of the basic usage is as follows:
#'
#' \code{filter_zero(my_df, op = "<=", pat = "LFQ.intensity.*", val = 2)}
#'
#' This you would interpret as: "I want to keep the rows in \code{my_df} that
#' have \code{<= 2 zero} values in the columns whose names match the
#' regular expression \code{LFQ.intensity.*}."
#'
#' @param data data.frame or matrix: has rows you want to filter out
#'
#' @param op operator: can be "==", "<=", or ">=", defines the logic for the rows
#' you want to keep, i.e. keep rows with more than or equal to some number
#' of \code{zero} values
#'
#' @param pat regular expression: used for selecting columns, e.g. "LFQ.intensity.*"
#'
#' @param val integer: defines the number (sum) of \code{zero} values
#'
#' @return Returns a \code{data.frame} or \code{matrix} where the rows that
#' don't have a certain number of \code{zero} values in certain columns have
#' been removed.
#'
#' @seealso Some related \pkg{candidaev} package functions.
#'
#' \itemize{
#' \item \code{\link{filter_zero2}} and \code{\link{filter_zero4}}:
#' for filtering based on the number of \code{zero} values in 2 or 4 sets of columns.
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
#' # define a 4x4 numeric matrix
#' my_mat <- matrix(c(2, 7, 0, 0,
#'                    3, NA, 0, 0,
#'                    1, 0, 8, 0,
#'                    432, 564, 789, 555),
#'                  nrow = 4,
#'                  ncol = 4)
#' colnames(my_mat) <- c("sample_1",
#'                       "sample_2",
#'                       "sample_3",
#'                       "total")
#'
#' # keep rows with maximum 1/3 zero values
#' # in the 3 sample columns
#' my_mat2 <- filter_zero(my_mat,
#'                        op = "<=",
#'                        pat = "sample_",
#'                        val = 1)
#'
#' ## example for a data.frame input
#'
#' # define a 4x5 data.frame
#' my_df <- data.frame(sample_1 = c(2, 7, 0, 0),
#'                     sample_2 = c(3, NA, 0, 0),
#'                     sample_3 = c(1, 0, 8, 0),
#'                     total = c(432, 564, 789, 555),
#'                     colour = c("red", "green", "blue", "purple"),
#'                     stringsAsFactors = FALSE)
#'
#' # keep the row with exactly 3/3 zero values
#' # in the 3 sample columns
#' my_df2 <- filter_zero(my_df,
#'                       op = "==",
#'                       pat = "sample_",
#'                       val = 3)
#'
#' ## note you can also use this function with
#' ## dplyr functions and magrittr pipes
#' ## (and other tidyverse functions perhaps)
#'
#' # drop the 'total' column
#' # keep rows with at least 2/3 zero values
#' # in the 3 sample columns
#' # pull the 'colour' column as a character vector
#' my_colour <- my_df %>%
#'   select(-total) %>%
#'   filter_zero(.,
#'               op = ">=",
#'               pat = "sample_",
#'               val = 2) %>%
#'   pull(colour)
#'
#'
#'
#' @export
#'
filter_zero <- function(data, op = c("==", "<=", ">="), pat, val) {
  # refer to operator by name
  op <- as.name(op)

  # get sum of values in column group
  col_sum <- apply(data[, c(grep(pat, colnames(data)))], 1, function(x) sum(x == 0, na.rm = TRUE))

  # subset data based on sum of values in column group
  subset(data, sapply(col_sum, op, val))
}
