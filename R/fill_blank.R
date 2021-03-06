#' Fill blanks in character column
#'
#' @description \code{fill_blank} fills any empty "" strings in a data.frame character
#' column with the corresponding strings from another character column in
#' the same data.frame.
#'
#' @param df data.frame
#' @param blank_col character column in \code{df}: contains empty strings
#' @param fill_col character column in \code{df}: ccontains strings to replace empty strings
#'
#' @return \code{df} with empty strings in a specific character column filled
#' with strings from another column.
#'
#' @examples
#' # make data.frame with blanks
#' # columns must be data type = character
#' df <- data.frame(name1 = c("Bob", "Jane", "Henry", "Sue"),
#'                  name2 = c("Mary", "", "Tim", ""),
#'                  stringsAsFactors = FALSE)
#'
#' # fill blanks in names2
#' df <- fill_blank(df, "name2", "name1")
#'
#' @export
fill_blank <- function(df, blank_col, fill_col) {
  df[[blank_col]] <- as.character(ifelse(df[[blank_col]] == "",
                                         df[[fill_col]],
                                         df[[blank_col]]))
  return(df)
}
