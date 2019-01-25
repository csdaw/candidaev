#' Assert that a data.frame contains specified column names
#'
#' Given a data.frame or data.table object, assert that all columns in the colnames argument exist as columns.

#' @param data A data.frame or data.table
#' @param colnames Character vector with column names corresponding to columns in \emph{data}
#' @param only_colnames Assert that the only columns in the data object should be those in \emph{colnames}. Default = T.

#' @return Throws error if test is violated.
#' @export

#' @examples
#' assert_colnames(CO2, c("Plant","Type","Treatment","conc","uptake"))
#' assert_colnames(CO2, c("Plant","Type"), only_colnames=FALSE)
assert_colnames <- function(data, colnames, only_colnames=TRUE) {
  # Do all specified colnames exist in dataframe?
  non_df_cols <- colnames[!colnames %in% colnames(data)]

  # Do all columns of the data exist in the specified colnames?
  non_colname_cols <- colnames(data)[!colnames(data) %in% colnames]

  if(length(non_df_cols) > 0 & length(non_colname_cols) > 0) {
    stop(paste0("These columns exist in colnames but not in your dataframe: ",
                paste(non_df_cols, collapse=" "),
                " and these exist in your dataframe but not in colnames: ",
                paste(non_colname_cols, collapse=" ")))
  } else if(length(non_df_cols) > 0) {
    stop(paste0("These columns exist in colnames but not in your dataframe: ",
                paste(non_df_cols, collapse=" ")))
  } else if(length(non_colname_cols) > 0 & only_colnames == TRUE) {
    stop(paste0("These columns exist in your dataframe but not in colnames: ",
                paste(non_colname_cols, collapse=" ")))
  }
  print("All column names present")
}

assert_colnames2 <- function(df, colnames, only_colnames = TRUE) {
  # Define specified columns that are not in dataframe
  non_df_cols <- colnames[!colnames %in% colnames(df)]

  # Define dataframe columns that are not in required columns
  non_colnames_cols <- colnames(df)[!colnames(df) %in% colnames]

  if(length(non_df_cols) > 0 & length(non_colname_cols) > 0) {
    stop(sprintf(paste0("These specified colnames don't exist in df:  ",
                paste(non_df_cols, collapse = ", "),
                "\n\nThese df columns don't exist in specified colnames:  ",
                paste(non_colnames_cols, collapse = ", "))))
  } else if(length(non_df_cols) > 0) {
    stop(paste0("These specified colnames don't exist in df:  ",
                paste(non_df_cols, collapse = ", ")))
  } else if(length(non_colnames_cols) > 0 & only_colnames == TRUE) {
    stop(paste0("These df columns don't exist in specified colnames:  ",
                paste(non_colname_cols, collapse = ", ")))
  }
  return(TRUE)
}

assert_exd <- function(df) {
  # Required columns in experimental design dataframe
  exd_cols <- c("ID", "label", "condition", "replicate")

  # Define required columns that are not in dataframe
  non_df_cols <- exd_cols[!exd_cols %in% colnames(df)]

  # Define dataframe columns that are not in required columns
  non_exd_cols <- colnames(df)[!colnames(df) %in% exd_cols]

  if(length(non_df_cols) > 0 & length(non_exd_cols) > 0) {
    stop(sprintf(paste0("Add these required columns to experimental design:  ",
                paste(non_df_cols, collapse = ", "),
                "\n\nRemove these unecessary columns from experimental design:  ",
                paste(non_exd_cols, collapse = ", "))))
  } else if(length(non_df_cols) > 0) {
    stop(paste0("Add these required columns to experimental design:  ",
                paste(non_df_cols, collapse = ", ")))
  } else if(length(non_exd_cols) > 0) {
    stop(paste0("Remove these unecessary columns from experimental design:  ",
                paste(non_exd_cols, collapse = ", ")))
  }
  return(TRUE)
}
