#' Title
#'
#' @param mat Description
#'
#' @param toptab Description
#'
#' @return Description
#' @export
#'
#' @examples
#' # example
combine_result <- function(mat, toptab) {
  result <- cbind(mat,
                  toptab[match(rownames(mat), rownames(toptab)), ],
                  significant = toptab[match(rownames(mat), rownames(toptab)), "adj.P.Val"] < 0.01)
  return(result)
}
