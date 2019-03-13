#' Title
#'
#' Description.
#'
#' @param x Description.
#' @param ... Description.
#'
#' @return Description.
#'
#' @examples
#' # example
#'
#' @export
tukey_hsd <- function(x, ...) {
  res <- TukeyHSD(x, ...) %>%
    broom::tidy() %>%
    mutate(comparison2 = comparison) %>%
    tidyr::separate(comparison2, into= c("group2", "group1"), sep = "-") %>%
    rename(p.adj = adj.p.value) %>%
    mutate(p.adj = signif(p.adj, 2)) %>%
    select(term, group1, group2, everything(), -term)
  res
}
