#' Title
#'
#' Description.
#'
#' @param mat Description.
#'
#' @return Description.
#'
#' @examples
#' # example
#'
#' @export
#'
plot_mds <- function(mat) {
  mds <- limma::plotMDS(mat, plot = FALSE)

  coord <- data.frame(dim1 = mds[["x"]],
                      dim2 = mds[["y"]]) %>%
    tibble::rownames_to_column(var = "sample") %>%
    mutate(sample = stringr::str_extract(.[["sample"]], "[^_]+"))

  ggplot(coord, aes(x = dim1, y = dim2, colour = sample)) +
    geom_point()
}
