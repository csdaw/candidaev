combine_result <- function(mat, toptab) {
  result <- cbind(mat,
                  toptab[match(rownames(mat), rownames(toptab)), ],
                  significant = toptab[, "adj.P.Val"] < 0.01)
  return(result)
}
