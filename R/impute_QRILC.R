#' Imputation of left-censored missing data using QRILC method
#'
#' @description This function is a simple wrapper around the
#' \code{\link[imputeLCMD]{impute.QRILC}}
#' function from \pkg{imputeLCMD}. See \code{\link[imputeLCMD]{impute.QRILC}} for
#' more information.
#'
#' @param mat numeric matrix: contains \code{NA} values
#'
#' @return Returns a numeric \code{matrix} with missing values imputed.
#'
#' @examples
#' # create a numeric matrix with missing data
#' my_mat <- matrix(c(NA, NA, 30, NA, 25, NA,
#'                    NA, 15, 31, 23, 24, NA,
#'                    NA, NA, 32, 24, 23, NA,
#'                    23, NA, 29, 22, NA, NA,
#'                    24, NA, 30, NA, NA, NA,
#'                    21, 14, 31, 24, NA, 12),
#'                  nrow = 6,
#'                  ncol = 6)
#' colnames(my_mat) <- c("sample_1a",
#'                       "sample_2a",
#'                       "sample_3a",
#'                       "sample_1b",
#'                       "sample_2b",
#'                       "sample_3b")
#'
#' # impute missing data
#' my_mat2 <- impute_QRILC(my_mat)
#'
#' @export
impute_QRILC <- function(mat) {
  imp <- imputeLCMD::impute.QRILC(mat)
  imp <- imp[[1]]
}
