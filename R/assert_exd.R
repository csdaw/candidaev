#' Assert that an experimental design has the correct columns
#'
#' Certain plot functions adapted from the \href{doi.org/10.18129/B9.bioc.DEP}{DEP}
#' package require an experimental design to define data groups and labels.
#' This internal function ensures that only the exact, correct columns are
#' present in the experimental design data frame.
#'
#' Extraneous columns are named so they can be removed and missing
#' columns are also named so they can be added to the experimental
#' design.
#'
#' @param df data frame: experimental design for proteomics experiment
#'
#' @return \code{TRUE} if experimental design has correct columns.
#'
#' Prints an error messages if there are extra and/or missing columns, with
#' the names of those extra and/or missing columns.
#'
#' @examples
#' # example of a correct experimental design
#' exp <- c(ID = c("A", "B", "C"),
#'          label = c("LFQ.intensity.A", "LFQ.intensity.B", "LFQ.intensity.C"),
#'          condition = c("sample", "sample", "control"),
#'          replicate = c(1, 2, 1))
#'
#' @seealso
#' This function is used in \code{\link(plot_heatmap2)}, etc.
#'
assert_exd <- function(df) {
  # assert that input is data frame
  assertthat::assert_that(is.data.frame(df))

  # required columns in experimental design data frame
  exd_cols <- c("ID", "label", "condition", "replicate")

  # define required columns that are not in data frame
  non_df_cols <- exd_cols[!exd_cols %in% colnames(df)]

  # define data frame columns that are not in required columns
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
