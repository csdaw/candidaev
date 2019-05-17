#' Convert proteinGroups table into LFQ intensity matrix
#'
#' @description This function extracts the \code{LFQ.intensity} columns defined in
#' an experimental design from a MaxQuant proteinGroups data.frame, and converts
#' these into a numeric matrix.
#'
#' It also changes the row names of the numeric matrix to the UniProt
#' accession(s) found in the \code{Majority.protein.IDs} column of the
#' proteinGroups data.frame.
#'
#' A numeric matrix generally makes it easier to perform
#' normalisation, imputation, and statistical analyses in later steps.
#'
#' @param df data.frame: this should at a minimum contain a \code{Majority.protein.IDs}
#' column as well as the \code{LFQ.intensity} columns defined in the experimental design.
#'
#' @param exd data.frame: see the examples section of \code{\link{assert_exd}} or
#' \code{\link{atcc_exp}} for examples of appropriately structured experimental designs.
#'
#' @param log_2 logical: if \code{TRUE} then the numeric matrix will be log2
#' transformed before being returned. If \code{FALSE} the output numeric
#' matrix will not be log2 transformed before being returned.
#'
#' @return Returns a numeric \code{matrix} with the same number of rows as \code{df}
#' and the \code{LFQ.intensity} columns defined in \code{exd}. The row names are the
#' \code{Majority.protein.IDs} column in \code{df}.
#'
#'
#' @examples
#' # load dplyr
#' library(dplyr)
#'
#' # load a proteinGroups data.frame supplied with this package
#' my_proteinGroups <- atcc
#'
#' # load its corresponding experimental design
#' my_expDesign <- atcc_exp
#'
#' # filter for proteins identified with minimum 3 unique peptides
#' # and convert to numeric matrix
#' my_lfq <- my_proteinGroups %>%
#'   filter(Unique.peptides >= 3) %>%
#'   convert_lfq(., my_expDesign)
#'
#' @import dplyr
#' @export
convert_lfq <- function(df, exd, log_2 = TRUE) {
  # define vector of labels from experimental design
  lfq_cols <- exd[["label"]]

  # show error if inputs are not correct data structure
  # or don't have correct columns
  assertthat::assert_that(is.data.frame(df),
                          is.data.frame(exd),
                          assert_exd(exd))
  # show error if LFQ columns in experimental design don't exactly match
  # LFQ intensity columns in proteinGroups data.frame
  assertthat::assert_that(identical(lfq_cols, colnames(select(df, one_of(lfq_cols)))))

  # select LFQ intensity data and convert to numeric matrix with
  # UniProt accessions as rownames
  mat <- df %>%
    select(Majority.protein.IDs, dplyr::one_of(lfq_cols)) %>%
    tibble::remove_rownames() %>%
    tibble::column_to_rownames(var = "Majority.protein.IDs") %>%
    as.matrix()

  # give columns shorter names
  colnames(mat) <- exd[["ID"]]
  mat[mat == 0] <- NA

  # log2 transform LFQ intensities if required
  if(log_2 == TRUE) {
    mat <- log2(mat)
    return(mat)
  } else {return(mat)}
}
