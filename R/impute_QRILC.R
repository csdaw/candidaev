#' Title
#'
#' @param mat Description
#'
#' @return Returns x
#'
#' @export
#'
#' @examples
#' # example
#'
impute_QRILC <- function(mat) {
  imp <- imputeLCMD::impute.QRILC(mat)
  imp <- imp[[1]]
}
