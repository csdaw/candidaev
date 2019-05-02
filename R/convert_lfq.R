#' Title
#'
#' @param df Description
#'
#' @param exd Description
#'
#' @return Returns x
#'
#'
#' @examples
#' # example
#'
#' @import dplyr
#'
#' @export
#'
convert_lfq <- function(df, exd, log_2 = TRUE) {
  # define vector of labels from experimental design
  lfq_cols <- exd[["label"]]

  # show error if inputs are not correct class or structure
  assertthat::assert_that(is.data.frame(df),
                          is.data.frame(exd),
                          assert_exd(exd))
  # show error if LFQ columns in experimental design don't exactly match LFQ
  # columns in proteinGroups data frame
  assertthat::assert_that(identical(lfq_cols, colnames(select(df, one_of(lfq_cols)))))

  # select LFQ data and convert to matrix with UniProt accessions as rownames
  mat <- df %>%
    select(Majority.protein.IDs, dplyr::one_of(lfq_cols)) %>%
    tibble::remove_rownames() %>%
    tibble::column_to_rownames(var = "Majority.protein.IDs") %>%
    as.matrix()

  # give columns shorter names and log2 transform LFQ intensities
  colnames(mat) <- exd[["ID"]]
  mat[mat == 0] <- NA

  if(log_2 == TRUE) {
    mat <- log2(mat)
    return(mat)
  } else {return(mat)}
}
