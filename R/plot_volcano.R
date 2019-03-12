#' Title
#'
#' Description.
#'
#' @param data Description.
#' @param p_val Description.
#' @param lfc Description.
#' @param grp Description.
#' @param colour Description.
#' @param label Description.
#'
#' @return Description.
#'
#' @examples
#' #example
#'
#' @export
plot_volcano <- function(data, p_val, lfc, grp, label) {
  p_val <- rlang::enquo(p_val)
  lfc <- rlang::enquo(lfc)
  grp <- rlang::enquo(grp)
  label <- rlang::enquo(label)


  ggplot(data = dplyr::filter(data, !is.na(!!lfc)),
         mapping = aes(x = !!lfc, y = -log10(!!p_val))) +
    geom_point(mapping = aes(colour = !!grp), size = 0.6) +
    theme_classic(base_size = 8) +
    theme(legend.position = "left",
          legend.title = element_blank(),
          axis.text = element_text(colour = "black", size = 10)) +
    guides(colour = guide_legend(override.aes = list(size = 3))) +
    ggrepel::geom_text_repel(data = dplyr::filter(data, !is.na(!!lfc)),
                             mapping = aes(label = !!label),
                             show.legend = FALSE,
                             size = 2) +
    xlab(expression(log[2]("FC"))) +
    ylab(expression(-log[10]("Adj. p-value")))
}
