#' Assert that an experimental design has the correct columns
#'
#' @description Certain plot functions in this package which are adapted from the
#' \href{http://doi.org/10.18129/B9.bioc.DEP}{DEP}
#' package require an experimental design to define data groups and labels.
#' This internal function ensures that only the exact, correct columns are
#' present in the experimental design data frame.
#'
#' Extraneous columns are named so they can be removed and missing
#' columns are also named so they can be added to the experimental
#' design.
#'
#' This function a slightly modified version of the \code{assert_colnames} function
#' from the \href{https://cran.r-project.org/web/packages/assertable/index.html}{assertable}
#' package. \code{assert_exd} is meant to be used with the \code{assert_that} function from the
#' \href{https://cran.r-project.org/web/packages/assertthat/index.html}{assertthat}
#' package.
#'
#' @param df data frame: the experimental design for proteomics experiment. See
#' examples section for an example of a correct experimental design.
#'
#' @return Return \code{TRUE} if experimental design has correct columns.
#'
#' Print an error message if there are extra and/or missing columns, with
#' the names of those extra and/or missing columns.
#'
#' @seealso This function is used in the following
#' \pkg{candidaev} package functions:
#' \itemize{
#' \item \code{\link{convert_lfq}}
#' \item \code{\link{get_annotation2}}
#' \item \code{\link{plot_heatmap2}}
#' \item \code{\link{plot_imputation2}}
#' \item \code{\link{plot_normalization2}}
#' \item \code{\link{plot_numbers2}}
#' \item \code{\link{plot_volcano2}}
#' }
#'
#' @examples
#' # example of a correct experimental design for
#' # a MaxQuant proteinGroups.txt file
#' exp <- data.frame(ID = c("A", "B", "C"),
#'                   label = c("LFQ.intensity.A",
#'                             "LFQ.intensity.B",
#'                             "LFQ.intensity.C"),
#'                   condition = c("sample",
#'                                 "sample",
#'                                 "control"),
#'                   replicate = c(1, 2, 1))
#'
#' # use assert_exd within a function definition
#' my_fun <- function(x) {
#'   assertthat::assert_that(assert_exd(x))
#'
#'   # rest of function goes here
#' }
#'
#' # function will stop if exp is not correct
#' my_fun(exp)
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
    stop(sprintf(paste0("Add the following column(s) to the experimental design data frame:  ",
                paste(non_df_cols, collapse = ", "),
                "\n\nRemove the following unecessary columns from experimental design data frame:  ",
                paste(non_exd_cols, collapse = ", "))))
  } else if(length(non_df_cols) > 0) {
    stop(paste0("Add the following column(s) to the experimental design data frame:  ",
                paste(non_df_cols, collapse = ", ")))
  } else if(length(non_exd_cols) > 0) {
    stop(paste0("Remove the following unecessary columns from experimental design data frame:  ",
                paste(non_exd_cols, collapse = ", ")))
  }
  return(TRUE)
}
