#' Title
#'
#' @param mat
#'
#' @return
#' @export
#'
#' @examples
QRILC_impute <- function(mat) {
  imp <- imputeLCMD::impute.QRILC(mat)
  imp <- imp[[1]]
}
