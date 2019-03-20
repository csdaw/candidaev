#' Title
#'
#' Description.
#'
#' @param go_table Description.
#' @param terms_list Description.
#' @param col_pal Description.
#'
#' @return Description.
#'
#' @examples
#' # example
#'
#' @export
plot_go_bar <- function(go_table, terms_list, col_pal) {
  ggplot(data = go_table[terms_list, ],
         aes(x = reorder(GO.term, Cat.ratio),
             y = Cat.ratio*100)) +
    geom_bar(stat = "identity", aes(fill = GO.type)) +
    theme_classic(base_size = 8) +
    scale_y_continuous(limits = c(0,100), expand = expand_scale(mult = c(0, .05))) +
    labs(x = NULL,
         y = "Percent GO term coverage",
         fill = "GO category") +
    theme(legend.justification = c(1,0),
          legend.position = "top",
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 6, colour = "black"),
          panel.grid.major.x = element_line(size = 0.2, colour = "black")) +
    scale_fill_manual(values = col_pal) +
    coord_flip()
}
