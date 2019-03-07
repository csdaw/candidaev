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
plot_volcano <- function(data, p_val, lfc, grp, colour = c("red1", "blue1"), label) {
  p_val <- rlang::enquo(p_val)
  lfc <- rlang::enquo(lfc)
  grp <- rlang::enquo(grp)
  label <- rlang::enquo(label)


  ggplot(data = dplyr::filter(data, !is.na(!!lfc)),
         mapping = aes(x = !!lfc, y = -log10(!!p_val))) +
    geom_point(mapping = aes(colour = !!grp)) +
    scale_colour_manual(values = c(colour, "gray")) +
    ggrepel::geom_text_repel(data = dplyr::filter(data, !is.na(!!lfc)),
                             mapping = aes(label = !!label),
                             show.legend = FALSE) +
    theme_classic() +
    theme(legend.position = "top",
          legend.title = element_blank())
}
