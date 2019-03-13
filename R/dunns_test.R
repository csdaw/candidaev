#' Title
#'
#' Description.
#'
#' @param x Description.
#' @param g Description.
#' @param method Description.
#' @param ... Description.
#'
#' @return Description.
#'
#' @examples
#' # example
#'
#' @export
dunns_test <- function(x, g, method, ...) {
  # capture console output before it prints
  dtres <- utils::capture.output(res <- dunn.test::dunn.test(x, g, method, ...))

  # convert returned list `res` to data.frame
  res <- res %>%
    as.data.frame() %>%
    rename(p.adj = P.adjusted, p.val = P, comparison = comparisons) %>%
    tidyr::separate(comparison, into = c("group2", "group1"), sep = " - ", remove= FALSE) %>%
    mutate(p.adj = signif(p.adj, 2)) %>%
    select(group1, group2, comparison, everything())
  res
}
