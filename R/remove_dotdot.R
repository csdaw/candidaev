#' Remove duplicate . in column names
#'
#' @description \code{remove_dotdot} is a function that removes
#' duplicated periods in column names and replaces them with a single period.
#'
#' @param data data.frame or matrix: has column names contain duplicate periods
#' like .. or ... or .... etc.
#'
#' @return Returns a data.frame or matrix with column names that only
#' contain single periods.
#'
#' @examples
#' # make a data.frame
#' my_df <- data.frame(name..1 = c("Bob", "Jane", "Henry", "Sue"),
#'                     name..2 = c("Mary", "", "Tim", ""),
#'                     stringsAsFactors = FALSE)
#'
#' # remove duplicate periods from column names
#' new_df <- remove_dotdot(my_df)
#'
#' # make a matrix
#' my_mat <- matrix(c(2, 7, NA, NA,
#'                    3, 6, NA, NA,
#'                    1, NA, 8, NA,
#'                    432, 564, 789, 555),
#'                    nrow = 4,
#'                    ncol = 4)
#' colnames(my_mat) <- c("sample....1",
#'                       "sample..2",
#'                       "sample...3",
#'                       "total")
#'
#' # remove duplicate periods from column names
#' new_mat <- remove_dotdot(my_mat)
#'
#' @export
remove_dotdot <- function(data) {
  colnames(data) <- gsub("\\.\\.+", ".", colnames(data))
  colnames(data) <- gsub("\\.$", "", colnames(data))
  return(data)
}
