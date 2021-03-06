#' Fill \code{NA} in character column
#'
#' @description \code{fill_na} fills in any \code{NA} rows in a data.frame character
#' column with the corresponding strings from another character column in
#' the same data.frame.
#'
#' Essentially \code{fill_na} converts all \code{NA} in a character column to ""
#' then uses \code{\link{fill_blank}} to fill in with values from
#' another character column in the same data frame.
#'
#' @param df data.frame
#' @param na_col character column in \code{df}: contains \code{NA} values
#' @param fill_col character column in \code{df}: ccontains strings to
#' replace \code{NA} values
#'
#' @return Returns a \code{data.frame} with \code{NA} in a specific character column filled
#' with strings from another column.
#'
#' @examples
#' # make data frame with NA
#' # columns must be class character
#' df <- data.frame(name1 = c("Bob", "Jane", "Henry", "Sue"),
#'                  name2 = c("Mary", NA, "Tim", NA),
#'                  stringsAsFactors = FALSE)
#'
#' # fill na in names2
#' df <- fill_na(df, "name2", "name1")
#'
#' @export
fill_na <- function(df, na_col, fill_col) {
  df[[na_col]][is.na(df[[na_col]])] <- ""
  df[[na_col]] <- as.character(ifelse(df[[na_col]] == "",
                                      df[[fill_col]],
                                      df[[na_col]]))
  return(df)
}
