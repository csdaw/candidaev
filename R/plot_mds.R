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
plot_mds <- function(mat, mat_labels, shape_size = 2, ...) {
  mds <- limma::plotMDS(mat,
                        top = 500,
                        gene.selection = "common",
                        plot = FALSE,
                        ...)

  coord <- data.frame(dim1 = mds[["x"]],
                      dim2 = mds[["y"]]) %>%
    tibble::rownames_to_column(var = "sample") %>%
    mutate(sample = stringr::str_extract(.[["sample"]], "[^_]+"),
           label = mat_labels)

  ggplot(coord, aes(x = dim1, y = dim2)) +
    geom_point(aes(fill = sample, shape = label), size = shape_size, colour = "black")
}
