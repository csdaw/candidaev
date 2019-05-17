#' Filter rows based on 1 set of columns which contain \code{NA} values
#'
#' @description This function uses a regular expression to define 1
#' set of columns in a data.frame/matrix of your choosing (based
#' on the column names).
#' Then it will filter out the data.frame/matrix rows that don't have a certain
#' number of \code{NA} values in  the set of columns.
#'
#' An example of the basic usage is as follows:
#'
#' \code{filter_na(my_df, op = "<=", pat = "LFQ.intensity.*", val = 2)}
#'
#' This you would interpret as: "I want to keep the rows in \code{my_df} that
#' have \code{<= 2 NA} values in the columns whose names match the
#' regular expression \code{LFQ.intensity.*}."
#'
#' @param data data.frame or matrix: has rows you want to filter out
#'
#' @param op operator: can be "==", "<=", or ">=", defines the logic for the rows
#' you want to keep, i.e. keep rows with more than or equal to some number
#' of \code{NA} values
#'
#' @param pat regular expression: used for selecting columns, e.g. "LFQ.intensity.*"
#'
#' @param val integer: defines the number (sum) of \code{NA} values
#'
#' @return Returns a \code{data.frame} or \code{matrix} where the rows that
#' don't have a certain number of \code{NA} values in certain columns have
#' been removed.
#'
#' @seealso Some related \pkg{candidaev} package functions.
#'
#' \itemize{
#' \item \code{\link{filter_na2}} and \code{\link{filter_na4}}:
#' for filtering based on the number of \code{NA} values in 2 or 4 sets of columns.
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
#' # define a 4x4 numeric matrix
#' my_mat <- matrix(c(2, 7, NA, NA,
#'                    3, 6, NA, NA,
#'                    1, NA, 8, NA,
#'                    432, 564, 789, 555),
#'                  nrow = 4,
#'                  ncol = 4)
#' colnames(my_mat) <- c("sample_1",
#'                       "sample_2",
#'                       "sample_3",
#'                       "total")
#'
#' # keep rows with maximum 1/3 NA values in sample columns
#' my_mat2 <- filter_na(my_mat,
#'                      op = "<=",
#'                      pat = "sample_",
#'                      val = 1)
#'
#' ## example for a data.frame input
#'
#' # define a 4x5 data.frame
#' my_df <- data.frame(sample_1 = c(2, 7, NA, NA),
#'                     sample_2 = c(3, 6, NA, NA),
#'                     sample_3 = c(1, NA, 8, NA),
#'                     total = c(432, 564, 789, 555),
#'                     colour = c("red", "green", "blue", "purple"),
#'                     stringsAsFactors = FALSE)
#'
#' # keep the row with exactly 3/3 NA values
#' # in the 3 sample columns
#' my_df2 <- filter_na(my_df,
#'                     op = "==",
#'                     pat = "sample_",
#'                     val = 3)
#'
#' ## note you can also use this function with
#' ## dplyr functions and magrittr pipes
#' ## (and other tidyverse functions perhaps)
#' # load dplyr
#' library(dplyr)
#'
#' # drop the 'total' column
#' # keep rows with at least 2/3 NA values
#' # in the 3 sample columns
#' # pull the 'colour' column as a character vector
#' my_colour <- my_df %>%
#'   select(-total) %>%
#'   filter_na(.,
#'             op = ">=",
#'             pat = "sample_",
#'             val = 2) %>%
#'   pull(colour)
#'
#'
#'
#' @export
filter_na <- function(data, op = c("==", "<=", ">="), pat, val) {
  # refer to operator by name
  op <- as.name(op)

  # get number of NA in column group
  n_na <- apply(is.na(data[, c(grep(pat, colnames(data)))]), 1, sum)

  # subset data based on number of NA in column group
  subset(data, sapply(n_na, op, val))
}
