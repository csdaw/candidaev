#' Title
#'
#' Description.
#'
#' @param data Description.
#' @param p_val Description.
#' @param log2fc Description.
#' @param group Description.
#' @param label_fun Description.
#' @param point_size Description.
#' @param label_size Description.
#' @param x_lim Description.
#' @param y_lim Description.
#' @param legend_params Description.
#' @param axis_params Description.
#'
#' @return Description.
#'
#' @examples
#'
#' # example
#'
#' @export
plot_volcano <- function(data, p_val, log2fc, group, label_fun,
                         point_size = 1, label_size = 2,
                         x_lim = c(-10, 10), y_lim = c(0, 10),
                         legend_params = c("left", 11, 10),
                         axis_params = c("black", 10)) {

  # enquo data columns and label function
  p_val <- rlang::enquo(p_val)
  log2fc <- rlang::enquo(log2fc)
  group <- rlang::enquo(group)
  label_fun <- rlang::enquo(label_fun)

  # plot all data that has a valid log2fc value
  p <- ggplot(data = dplyr::filter(data, !is.na(!!log2fc)),
              mapping = aes(x = !!log2fc, y = -log10(!!p_val))) +
    geom_point(mapping = aes(colour = !!group), size = point_size) +
    theme_classic(base_size = 11)

  # adjust legend position, text sizes, text colours
  p <- p +
    theme(legend.position = legend_params[1],
          legend.title = element_text(size = legend_params[2], face = "bold"),
          legend.text = element_text(size = legend_params[3]),
          axis.text = element_text(colour = axis_params[1], size = axis_params[2]))

  # adjust size of dots in legend and add text annotations to certain
  # points on the plot. Also add axis labels
  p <- p +
    guides(colour = guide_legend(override.aes = list(size = 3))) +
    ggrepel::geom_text_repel(data = dplyr::filter(data, !is.na(!!log2fc)),
                             mapping = aes(label = !!label_fun),
                             show.legend = FALSE,
                             size = label_size)

  # add axis limits and axis labels
  p <- p +
    scale_x_continuous(limits = x_lim, expand = c(0,0)) +
    xlab(expression(log[2]("FC"))) +
    scale_y_continuous(limits = y_lim, expand = c(0,0)) +
    ylab(expression(-log[10]("adj. p-val")))

  # return plot
  p
}
