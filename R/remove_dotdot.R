#' Remove duplicate . in column names
#'
#' Internal function to remove duplicated periods in column names with a
#' single period.
#'
#' @param df data frame: column names contain duplicate .. or ... etc.
#'
#' @return data frame: column names contain only single .
#'
remove_dotdot <- function(df) {
  colnames(df) <- gsub("\\.\\.+", ".", colnames(df))
  colnames(df) <- gsub("\\.$", "", colnames(df))
  return(df)
}
