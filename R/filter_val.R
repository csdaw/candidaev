#' Filter rows based on the sum of values across 1 set of columns
#'
#' @description This function uses a regular expression to define 1
#' set of columns in a data.frame/matrix of your choosing (based
#' on the column names).
#' Then it will filter out the data.frame/matrix rows that have
#' a sum across the selected columns that does not equal some specific
#' value or threshold.
#'
#' An example of the basic usage is as follows:
#'
#' \code{filter_val(my_df, op = ">=", pat = "Unique.peptides.*EV", val = 5)}
#'
#' This you would interpret as: "I want to keep the rows in \code{my_df} that
#' have have a sum \code{>= 5} when you add up the values in the
#' columns whose names match the regular expression \code{Unique.peptides.*EV}."
#'
#' @param data data.frame or matrix: has rows you want to filter out
#'
#' @param op operator: can be "==", "<=", or ">=", defines the logic for the rows
#' you want to keep, i.e. keep rows with a sum of more than or equal to some value
#'
#' @param pat regular expression: used for selecting columns, e.g. "Unique.peptides.*EV"
#'
#' @param val integer: defines the sum of column values
#'
#' @return Returns a \code{data.frame} or \code{matrix} where the rows that
#' don't add up to a certain value in certain columns have
#' been removed.
#'
#' @seealso Some related \pkg{candidaev} package functions.
#'
#' \itemize{
#' \item \code{\link{filter_val2}} and \code{\link{filter_val4}}:
#' for filtering based on the sum of the values in 2 or 4 sets of columns
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
#' # keep rows where the sample columns
#' # add up to at least 7
#' my_mat2 <- filter_val(my_mat,
#'                       op = ">=",
#'                       pat = "sample_",
#'                       val = 7)
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
#' # keep the row where the sample columns
#' # add up to exactly 6
#' my_df2 <- filter_val(my_df,
#'                      op = "==",
#'                      pat = "sample_",
#'                      val = 6)
#'
#' ## note you can also use this function with
#' ## dplyr functions and magrittr pipes
#' ## (and other tidyverse functions perhaps)
#' # load dplyr
#' library(dplyr)
#'
#' # drop the 'total' column
#' # keep rows where sample columns add up to
#' # 8 or less
#' # pull the 'colour' column as a character vector
#' my_colour <- my_df %>%
#'   select(-total) %>%
#'   filter_val(.,
#'              op = "<=",
#'              pat = "sample_",
#'              val = 8) %>%
#'   pull(colour)
#'
#'
#' @export
filter_val <- function(data, op = c("==", "<=", ">="), pat, val) {
  # refer to operator by name
  op <- as.name(op)

  # get sum of values in column group
  col_sum <- apply(data[, c(grep(pat, colnames(data)))], 1, function(x) sum(x, na.rm = TRUE))

  # subset data based on sum of values in column group
  subset(data, sapply(col_sum, op, val))
}
