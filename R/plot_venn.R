#' Title
#'
#'
#' @param vlist Description
#'
#' @param use_uniprot Description
#'
#' @param type Description
#'
#' @param output Description
#'
#' @param output_dim Description
#'
#' @param output_res Description
#'
#' @param output_units Description
#'
#' @param output_pts Description
#'
#' @param ... Description
#'
#' @return Description
#'
#' @examples
#' # make two data frames
#' v1 <- data.frame(colour = c("green", "red", "blue"), level = c(60, 50, 45))
#'
#' v2 <- data.frame(colour = c("purple", "green", "pink"), level = c(10, 13, 22))
#'
#' # make list of vectors to compare
#' comp <- list(c1 = v1$colour, c2 = v2$colour)
#'
#'
#' # plot venn diagram
#' plot_venn(vlist = comp)
#'
#' # plot venn diagram and save as png
#' plot_venn(vlist = comp, file = "my_venn.png", output = "png")
#'
#' # don't plot venn but get venn partitions
#' part <- plot_venn(vlist = comp, plot = FALSE)
#'
#' @importFrom grDevices dev.off png tiff
#' @export
plot_venn <- function(vlist, use_uniprot = FALSE, type = c("list", "plot", "save"),
                      output = c("tiff", "png"), output_file = NULL, output_dim = c(1000, 1000),
                      output_res = 300, output_units = "px",
                      output_pts = 14, ...) {
  # supress futile logger message from VennDiagram package
  futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")

  if(use_uniprot == TRUE) {
    # extract rownames (UniProt acessions) for comparison
    comparison <- mapply(rownames, vlist)
    # convert UniProt accessions to gene names
    comparison <- lapply(comparison, function(x) match_id(x, candidaev::uniprot,
                                                          "UP_accession", "CGD_gene_name"))
  } else {comparison <- vlist}

  # plot venn in R, plot venn and save file, or get venn partition lists
  if(type == "save") {
    venn_plot <- VennDiagram::venn.diagram(x = comparison,
                                           filename = NULL, ...)
    if(output == "tiff") {
      tiff(filename = file,
           width = output_dim[1],
           height = output_dim[2],
           res = output_res,
           units = output_units,
           pointsize = output_pts,
           family = "sans",
           type = "cairo",
           compression = "lzw")
      grid::grid.draw(venn_plot)
      dev.off()
    } else if (output == "png") {
      png(filename = file,
          width = output_dim[1],
          height = output_dim[2],
          res = output_res,
          units = output_units,
          pointsize = output_pts,
          family = "sans",
          type = "cairo")
      grid::grid.draw(venn_plot)
      dev.off()
    }
  } else if(type == "plot") {
    venn_plot <- VennDiagram::venn.diagram(x = comparison, filename = NULL, ...)
    return(venn_plot)
  } else if(type == "list") {
    venn <- VennDiagram::get.venn.partitions(comparison)
    return(venn)
  }
}
