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
plot_mds <- function(mat, mat_labels, shape_size = 2) {
  mds <- limma::plotMDS(mat, plot = FALSE)

  coord <- data.frame(dim1 = mds[["x"]],
                      dim2 = mds[["y"]]) %>%
    tibble::rownames_to_column(var = "sample") %>%
    mutate(sample = stringr::str_extract(.[["sample"]], "[^_]+"),
           label = mat_labels)

  ggplot(coord, aes(x = dim1, y = dim2)) +
    geom_point(aes(colour = sample, shape = sample), size = shape_size) +
    geom_text(aes(label = mat_labels), hjust = 1.4, vjust = 0.5) +
    xlab(label = "Dimension 1") +
    ylab(label = "Dimension 2")
}
